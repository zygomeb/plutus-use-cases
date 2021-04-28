{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module Lending where

import           Control.Monad                hiding (fmap)
-- import           Data.Aeson           (FromJSON, ToJSON)
import qualified Control.Lens                 as Lens
import           Control.Lens.Operators
import           Control.Lens.TH              (makeClassyPrisms)
import qualified Data.Aeson                   as Aeson
import           Data.Bifunctor               (second)
import qualified Data.Functor                 as Functor
import           Data.Functor.Identity        (Identity)
import qualified Data.List                    as List
import           Data.Map                     as Map
import           Data.OpenUnion.Internal      (Member)
import qualified Data.String                  as String
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import           Data.Text.Prettyprint.Doc    as Pretty
import           Data.Void                    (Void)
import           GHC.Generics                 (Generic)
import           Ledger                       hiding (singleton)
import           Ledger.Ada                   as Ada
import           Ledger.Constraints           as Constraints
import qualified Ledger.Interval              as Interval
import           Ledger.Oracle
import qualified Ledger.Typed.Scripts         as Scripts
import           Ledger.Typed.Tx
import           Ledger.Value                 as Value
import           Playground.Contract          (ToSchema, ensureKnownCurrencies, printJson, printSchemas, stage)
import           Playground.TH                (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types             (ContractCall (amount), KnownCurrency (..))
import           Plutus.Contract              hiding (when)
import qualified Plutus.Contract.StateMachine as SM
import qualified PlutusTx
import           PlutusTx.Prelude             hiding (Semigroup (..), unless)
import qualified Prelude                      as Haskell
import           Text.Printf                  (printf)

----------------------------------------------------------------
-- Pretty
----------------------------------------------------------------

class Pretty p where
  pp :: p -> Pretty.Doc a
  default pp :: Pretty.Pretty p => p -> Pretty.Doc a
  pp = Pretty.pretty

newtype PP a = PP { unPP :: a }

instance Lending.Pretty a => Show (PP a) where
  show = show . pp . unPP

instance Lending.Pretty Text
instance Lending.Pretty Integer

----------------------------------------------------------------
-- General
----------------------------------------------------------------

type Lender = PubKeyHash

-- | Newtype wrapper for lending coins
newtype LC a = LC { unLC :: a }
  deriving newtype (Haskell.Num, Eq, Ord, AdditiveGroup, AdditiveMonoid, AdditiveSemigroup , MultiplicativeSemigroup)
  deriving stock (Generic, Haskell.Eq)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON, ToSchema)
  deriving Haskell.Show via PP (LC a)
  deriving Haskell.Functor via Identity

instance Lending.Pretty a => Lending.Pretty (LC a) where
  pp LC{unLC} = "LC<" <> pp unLC <> ">"

type Amount = LC Integer

----------------------------------------------------------------
-- Exceptions
----------------------------------------------------------------

-- class Exception e => LendingException e where

-- data SomeLendingException
--   = forall e . (Pretty e, LendingException e) => SomeLendingException e
--   deriving Show via (PP SomeLendingException)

data LendingException
  = EEInitializeException ContractError
  | EEStateMachineException SM.SMContractError
  | EEDepositException ContractError
  | EEWithdrawException ContractError
  | EEOtherContractException ContractError
  deriving stock (Generic)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)
  deriving Haskell.Show via (PP LendingException)

instance Lending.Pretty LendingException where
  pp _ = "TODO"

makeClassyPrisms ''LendingException

instance AsContractError LendingException where
    _ContractError = _EEOtherContractException

instance SM.AsSMContractError LendingException where
    _SMContractError = _EEStateMachineException

instance SM.AsSMContractError Text where
  _SMContractError = Lens.prism' (Text.pack . show) (const Nothing)

-- TODO: remove
instance AsLendingException Text where

-- TODO: find out why it fails to deduce
type AsLendingException' e =
  ( AsLendingException e
  , AsContractError e
  , SM.AsSMContractError e
  )

----------------------------------------------------------------
-- State
----------------------------------------------------------------

data LendingAction
  = Deposit Amount
  | Withdraw Amount
  deriving stock (Show, Generic)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)

data LendingState
  = LendingState
    { lsStore  :: [(Lender, Amount)]
    , lsAmount :: Amount
    -- , lsMonetaryPolicyHash :: MonetaryPolicyHash
    -- , lsToken              :: TokenName
    } deriving stock (Show, Generic)
      deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)

initialState :: LendingState
initialState =
  LendingState
    { lsStore = []
    , lsAmount = 0
    }

type LendingStateMachine = SM.StateMachine LendingState LendingAction
-- type LendingState = SM.State LendingState

{-# INLINABLE transition #-}
transition
  :: SM.State LendingState
  -> LendingAction
  -> Maybe (TxConstraints Void Void, SM.State LendingState)
transition SM.State{stateData = currentState, stateValue} action =
  fmap
    (second \newState -> SM.State{stateData = newState, stateValue})
    (step currentState action)

{-# INLINABLE subst #-}
subst
  :: LendingState
  -> LendingAction
  -> Maybe (TxConstraints i o, LendingState)
subst currentState@LendingState{lsAmount} action = do
  -- TODO: oracle validation
  let
    (newConstraints, newState) = case action of
      Deposit amount ->
        let
          -- constraints = Constraints.mustSpendAtLeast amount
          constraints = mempty
          newData = currentState { lsAmount = lsAmount + amount }
        in (constraints, newData)
      Withdraw amount ->
        let
          constraints = mempty
          newData = currentState { lsAmount = lsAmount - amount }
        in (constraints, newData)
  let dateConstraints = Constraints.mustValidateIn $ Interval.from 1 -- TODO
  pure (newConstraints <> dateConstraints, newState)

{-# INLINABLE step #-}
step
  :: LendingState
  -> LendingAction
  -> Maybe (TxConstraints i o, LendingState)
step currentState action = do
  (constraints, newState) <- subst currentState action
  guard $ isValidState newState
  pure (constraints, newState)

{-# INLINABLE isValidState #-}
isValidState :: LendingState -> Bool
isValidState lendingState = isRight (validateState lendingState)

{-# INLINABLE validateState #-}
validateState :: LendingState -> Either Text ()
validateState lendingState = pure ()

{-# INLINABLE lendingStateMachine #-}
lendingStateMachine :: LendingStateMachine
lendingStateMachine = SM.mkStateMachine transition (const False)

{-# INLINABLE mkLendingSMValidator #-}
mkLendingSMValidator :: Scripts.ValidatorType LendingStateMachine
mkLendingSMValidator = SM.mkValidator lendingStateMachine

lendingSMInstance :: Scripts.ScriptInstance LendingStateMachine
lendingSMInstance = Scripts.validator @LendingStateMachine
    $$(PlutusTx.compile [|| mkLendingSMValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.wrapValidator

lendingClient :: SM.StateMachineClient LendingState LendingAction
lendingClient =
  SM.mkStateMachineClient
    (SM.StateMachineInstance lendingStateMachine lendingSMInstance)

lendingValidator :: Validator
lendingValidator = Scripts.validatorScript lendingSMInstance

lendingAddress :: Ledger.Address
lendingAddress = scriptAddress lendingValidator


----------------------------------------------------------------
-- Endpoints
----------------------------------------------------------------

-- deposit( address asset,
-- uint256 amount,
-- address onBehalfOf
-- uint16 referralCode)

-- below is the doc for this endpoint, for now, lets just use ada (returning qAda)

-- the ada address is empty string if i recall correctly.

-- deposits an amount of underlying asset into the reserve, receiving in return overlying aTokens.
-- * - E.g. User deposits 100 USDC and gets in return 100 aUSDC

-- @param asset The address of the underlying asset to deposit

-- @param amount The amount to be deposited

-- @param onBehalfOf The address that will receive the aTokens, same as msg.sender if the user wants to receive them on his own wallet, or a different address if the beneficiary of aTokens is a different wallet

-- @param referralCode Code used to register the integrator originating the operation, for potential rewards. 0 if the action is executed directly by the user, without any middle-man
data DepositParams
  = DepositParams
  { dpToken  :: TokenName
  -- , dpMediator     :: PubKeyHash
  -- , dpReferralCode :: Integer
  , dpAmount :: Amount
  } deriving stock Generic
    deriving anyclass (Aeson.ToJSON, Aeson.FromJSON, ToSchema)

-- /* @dev Withdraws an amount of underlying asset from the reserve, burning the equivalent aTokens owned

-- E.g. User has 100 aUSDC, calls withdraw() and receives 100 USDC, burning the 100 aUSDC

-- @param asset The address of the underlying asset to withdraw

-- @param amount The underlying amount to be withdrawn

-- Send the value type(uint256).max in order to withdraw the whole aToken balance
-- @param to Address that will receive the underlying, same as msg.sender if the user wants to receive it on his own wallet, or a different address if the beneficiary is a different wallet

-- @return The final amount withdrawn

-- **/
-- function withdraw(
-- address asset,
-- uint256 amount,
-- address to
-- )
data WithdrawParams
  = WithdrawParams
  { wpToken  :: TokenName
  , wpAmount :: Amount
  -- , wpRecipient :: PubKeyHash
  } deriving stock Generic
    deriving anyclass (Aeson.ToJSON, Aeson.FromJSON, ToSchema)

-- | Context for lending pool.
data LendingContext
  = LendingContext
  { lcTokenName :: TokenName -- ^ Initial token for lending pool
  } deriving stock (Generic, Haskell.Show)
    deriving anyclass (Aeson.ToJSON, Aeson.FromJSON, ToSchema)

type HasLendingContext = ?lendingContext :: LendingContext

type LendingSchema =
    BlockchainActions
        .\/ Endpoint "initialize" LendingContext
        .\/ Endpoint "deposit" DepositParams
        .\/ Endpoint "withdraw" WithdrawParams

-- TODO: for some reason dynamic types on playground fail to resolve this
-- type HasLendingSchema s =
--   ( HasBlockchainActions s
--   , HasEndpoint "initialize" LendingContext s
--   , HasEndpoint "deposit" DepositParams s
--   , HasEndpoint "withdraw" WithdrawParams s
--   )

validateTransition
  :: (
      --  HasLendingSchema s
       AsLendingException' e
     )
  => Contract w LendingSchema e ()
validateTransition = do
  currentState <- SM.getOnChainState lendingClient
  case currentState of
    Just ((TypedScriptTxOut{tyTxOutData}, _), _) -> do
      logInfo @String $ "Current state is: " <> show tyTxOutData
    Nothing -> logWarn @String "Unable to find current state."

deposit
  :: (
      --  HasLendingSchema s
       AsLendingException' e
     , HasLendingContext
     )
  => DepositParams
  -> Contract w LendingSchema e ()
deposit DepositParams{..} = do
  validateTransition
  logInfo @String "depositing..."
  void $ mapError (_EEStateMachineException #)
       $ SM.runStep lendingClient (Deposit dpAmount)
  validateTransition

withdraw
  :: (
      --  HasLendingSchema s
       AsLendingException' e
     , HasLendingContext
     )
  => WithdrawParams
  -> Contract w LendingSchema e ()
withdraw WithdrawParams{..} = do
  void $ mapError (_EEStateMachineException #)
       $ SM.runStep lendingClient (Withdraw wpAmount)

endpoints
  :: (
        -- HasLendingSchema s
       AsLendingException' e
     )
  => Contract () LendingSchema e ()
endpoints = do
  lendingContext <- mapError (_EEInitializeException #) $ endpoint @"initialize"
  let ?lendingContext = lendingContext
  _ <- SM.runInitialise lendingClient initialState mempty
  forever $ List.foldl1 select
    [ endpoint @"deposit"  >>= deposit
    , endpoint @"withdraw" >>= withdraw
    ]

PlutusTx.unstableMakeIsData ''LC
PlutusTx.makeLift ''LC
PlutusTx.unstableMakeIsData ''LendingAction
PlutusTx.makeLift ''LendingAction
PlutusTx.unstableMakeIsData ''LendingState
PlutusTx.makeLift ''LendingState

mkSchemaDefinitions ''LendingSchema

mkKnownCurrencies []

