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

{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}

module Lending where

import           Control.Monad                hiding (fmap)
-- import           Data.Aeson           (FromJSON, ToJSON)
import qualified Control.Lens                 as Lens
import           Control.Lens.Operators
import           Control.Lens.TH              (makeClassyPrisms)
import qualified Data.Aeson                   as Aeson
import           Data.Bifunctor               (Bifunctor (first), second)
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
import qualified PlutusTx.AssocMap            as AssocMap
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

-- instance Lending.Pretty a => Show (PP a) where
--   show = show . pp . unPP

-- instance Lending.PrettyEntryt
-- instance Lending.Pretty Integer

----------------------------------------------------------------
-- General
----------------------------------------------------------------

-- | Context for lending pool.
data LendingContext
  = LendingContext
  { lcTokenName :: TokenName -- ^ Initial token for lending pool
  } deriving stock (Generic, Haskell.Show)
    deriving anyclass (Aeson.ToJSON, Aeson.FromJSON, ToSchema)


-- | Newtype wrapper for lending coins
newtype LC a = LC { unLC :: a }
  deriving newtype (Haskell.Num, Eq, Ord, AdditiveGroup, AdditiveMonoid, AdditiveSemigroup , MultiplicativeSemigroup)
  deriving stock (Generic, Haskell.Eq)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON, ToSchema)
  -- deriving Haskell.Show via PP (LC a) -- TODO
  deriving stock (Haskell.Show)
  deriving Haskell.Functor via Identity

-- instance Lending.Pretty a => Lending.Pretty (LC a) where
--   pp LC{unLC} = "LC<" <> pp unLC <> ">"

-- | Newtype wrapper for reserved coins
newtype RC a = RC { unRC :: a }
  deriving newtype (Haskell.Num, Eq, Ord, AdditiveGroup, AdditiveMonoid, AdditiveSemigroup , MultiplicativeSemigroup)
  deriving stock (Generic, Haskell.Eq)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON, ToSchema)
  -- deriving Haskell.Show via PP (LC a) -- TODO
  deriving stock (Haskell.Show)
  deriving Haskell.Functor via Identity

{-# INLINABLE  toLC #-}
toLC :: ConversionRate -> AmountReserved -> AmountLending
toLC rate (RC rc) = LC (rate * rc)

{-# INLINABLE fromLC #-}
fromLC :: ConversionRate -> AmountLending -> AmountReserved
-- fromLC rate (LC lc) = RC (lc `Haskell.div` rate)
fromLC rate (LC lc) = RC lc

type AmountLending = LC Integer
type AmountReserved = RC Integer

type ConversionRate = Integer -- TODO: do doubles
type Caller = PubKeyHash
type Lender = PubKeyHash

type ReferalCode = Integer

----------------------------------------------------------------
-- Exceptions
----------------------------------------------------------------

-- class Exception e => LendingException e where

-- data SomeLendingException
--   = forall e . (Pretty e, LendingException e) => SomeLendingException e
--   deriving Show via (PP SomeLendingException)

data LendingException
  = LEInitializeException ContractError
  | LEStateMachineException SM.SMContractError
  | LEDepositException ContractError
  | LEWithdrawException ContractError
  | LEOtherContractException ContractError
  deriving stock (Generic)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)
  -- deriving Haskell.Show via (PP LendingException)
  deriving stock (Haskell.Show) -- TODO

-- instance Lending.Pretty LendingException where
--   pp _ = "TODO"

makeClassyPrisms ''LendingException

instance AsContractError LendingException where
    _ContractError = _LEOtherContractException

instance SM.AsSMContractError LendingException where
    _SMContractError = _LEStateMachineException

instance SM.AsSMContractError Text where
  _SMContractError = Lens.prism' (Text.pack . show) (const Nothing)

-- TODO: remove
instance AsLendingException Text where
  _LendingException = Lens.prism' (Text.pack . show) (const Nothing)

-- TODO: find out why it fails to deduce
type AsLendingException' e =
  ( AsLendingException e
  , AsContractError e
  , SM.AsSMContractError e
  )

data ValidateStateException
  = VSENegativeAmountOfLendingCoins
  | VSENegativeAmountOfReservedCoins
  deriving stock Haskell.Show

data SubstitutionException
  = SEReferalRecipientIsNotPresent
  deriving stock Haskell.Show

----------------------------------------------------------------
-- State
----------------------------------------------------------------

data LendingAction
  = Deposit
    { laDepositAmount :: AmountReserved
    , laReferal       :: (Lender, ReferalCode)
    }
  | Withdraw AmountReserved
  deriving stock (Show, Generic)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)

-- data LendingPool
--   = LendingPool [(Wallet, LendingState)]

-- data LendingState
--   = LendingState
--   { lsReferalCode :: Maybe Integer
--   -- ^ Referal code for each address needed so that others
--   -- can deposit coins on behalf of the original address
--   , lsReserves    :: AmountReserved
--   -- ^ Amount of ada that was paid for lending tokens
--   , lsLending     :: AmountLending
--   -- ^ Amount of lending tokens for each wallet
--   }

-- data LendingContext
--   = LendingContext
--   { lsLendingCoinPrice :: AmountReserved
--   -- ^ Price needed for conversion of lending coins
--   , lsLendingCoinName  :: TokenName
--   -- ^ Token name
--   }

-- | State associated for each wallet addresss in the lending pool
data LendingPoolEntry
  = LendingPoolEntry
  { lpeReferalCode    :: Integer
  -- ^ Referal code for each address needed so that others
  -- can deposit coins on behalf of the original address
  , lpeAmountReserved :: AmountReserved
  -- ^ Amount of ada that was paid for lending tokens
  , lpeAmountLending  :: AmountLending
  -- ^ Amount of lending tokens for each wallet
  } deriving stock (Haskell.Show, Generic)
    deriving anyclass (Aeson.ToJSON, Aeson.FromJSON, ToSchema)

newtype LendingState
  = LendingState
  { lsLendingPool :: AssocMap.Map Lender LendingPoolEntry
  } deriving stock (Haskell.Show, Generic)
    deriving anyclass (Aeson.ToJSON, Aeson.FromJSON, ToSchema)

initialState :: LendingState
initialState = LendingState{lsLendingPool = AssocMap.empty}

type LendingStateMachine = SM.StateMachine LendingState LendingAction

-- {-# INLINABLE reserveCoins #-}
-- reserveCoins :: Caller -> LendingContext -> AmountReserved -> Value
-- reserveCoins caller context@LendingContext{lcTokenName} =
--   let symbol = Scripts.monetaryPolicyHash (lendingSMInstance caller context)
--   in Value.singleton (Value.mpsSymbol symbol) lcTokenName . unRC

{-# INLINABLE transition #-}
transition
  :: Caller
  -> LendingContext
  -> SM.State LendingState
  -> LendingAction
  -> Maybe (TxConstraints Void Void, SM.State LendingState)
transition caller context SM.State{stateData = currentState, stateValue} action =
  fmap
    (second \newState -> SM.State{stateData = newState, stateValue})
    (step caller context currentState action)

{-# INLINABLE step #-}
step
  :: Caller
  -> LendingContext
  -> LendingState
  -> LendingAction
  -> Maybe (TxConstraints i o, LendingState)
step caller context currentState action = do
  (constraints, newState) <-
    either (const Nothing) pure $ subst caller context currentState action
  guard $ isValidState newState
  pure (constraints, newState)

{-# INLINABLE hashReferalCode #-}
-- TODO
hashReferalCode :: Lender -> ReferalCode
hashReferalCode wallet = 2

{-# INLINABLE subst #-}
-- | Substitute given state with the action provided
-- Does nothing if something invalid and returns old state.
-- Note: all validation happens in `validateState` function
-- and the errors are collected there
-- TODO: constraints
subst
  :: forall i o. Caller
  -> LendingContext
  -> LendingState
  -> LendingAction
  -> Either SubstitutionException (TxConstraints i o, LendingState)
subst caller context LendingState{lsLendingPool} action =
  mbUpdatedState <&> first \newConstraints -> newConstraints <> dateConstraints
  where
    conversionRate = 1 -- TODO: oracle and conversion calculations

    -- dateConstraints = Constraints.mustValidateIn $ Interval.from 1
    dateConstraints = mempty

    depositFunds
      :: Lender
      -> AmountReserved
      -> Maybe LendingPoolEntry
      -> (TxConstraints i o, LendingState)
    depositFunds wallet amount poolEntry = (constraints, newData)
      where
        -- constraints = Constraints.mustSpendAtLeast (reserveCoins caller context amount)
        constraints = Constraints.mustSpendAtLeast (Ada.lovelaceValueOf (unRC amount))
        -- how convenient there's no update function in PlutusTx.AssocMap
        newData = LendingState $ AssocMap.insert caller newEntry lsLendingPool
        newEntry = case poolEntry of
          -- pool entry for the wallet is not set, proceed creating new one with the amount
          -- of funds deposited to it
          Nothing -> LendingPoolEntry
              { lpeAmountReserved = amount
              , lpeAmountLending = toLC conversionRate amount
              , lpeReferalCode = hashReferalCode wallet
              }
          -- pool entry for the wallet is present, proceed depositing funds further
          Just lendingPoolEntry@LendingPoolEntry{..} -> lendingPoolEntry
                { lpeAmountReserved =
                    lpeAmountReserved + amount
                , lpeAmountLending =
                    toLC conversionRate
                      $ fromLC conversionRate lpeAmountLending + amount
                }

    mbUpdatedState :: Either SubstitutionException (TxConstraints i o, LendingState)
    mbUpdatedState = let poolEntry = AssocMap.lookup caller lsLendingPool in
      case action of
        Deposit{laDepositAmount, laReferal = (recipient, referalCode)}
          -- referal is not required, proceed depositing funds on original wallet
          | referalCode == 0 -> pure $ depositFunds caller laDepositAmount poolEntry
          -- referal is required, proceed depositing funds on recipient wallet
          | otherwise -> case AssocMap.lookup recipient lsLendingPool of
            -- referal code accepted
            Just recipientLendingPoolEntry@LendingPoolEntry{lpeReferalCode}
              | lpeReferalCode == referalCode ->
                pure $ depositFunds caller laDepositAmount (Just recipientLendingPoolEntry)
            -- referal recipient is not present in lending pool, do nothing
            Nothing -> Left SEReferalRecipientIsNotPresent
        Withdraw amount -> case poolEntry of
          Just lendingPoolEntry@LendingPoolEntry{..} -> let
            constraints = mempty
            newEntry = lendingPoolEntry
              { lpeAmountReserved =
                  lpeAmountReserved - amount
              , lpeAmountLending =
                  toLC conversionRate
                    $ fromLC conversionRate lpeAmountLending - amount
              }
            newData = LendingState $ AssocMap.insert caller newEntry lsLendingPool
            in pure (constraints, newData)

{-# INLINABLE isValidState #-}
isValidState :: LendingState -> Bool
isValidState lendingState = isRight (validateState lendingState)

{-# INLINABLE validateState #-}
-- TODO: all errors from `subst` function
validateState :: LendingState -> Either ValidateStateException ()
validateState LendingState{lsLendingPool} =
  findLeft . fmap validateStateEntry . AssocMap.toList $ lsLendingPool
  where
    {-# INLINABLE findLeft #-}
    -- extract first `Left` occurence in a list of validations
    findLeft = \case
      []             -> pure ()
      (e@(Left _):_) -> e
      (_:xs)         -> findLeft xs

    {-# INLINABLE validateStateEntry #-}
    validateStateEntry
      ( caller
      , LendingPoolEntry
        { lpeAmountLending
        , lpeAmountReserved
        , lpeReferalCode
        }
      ) = do
      unless (lpeAmountLending > LC 0) (Left VSENegativeAmountOfLendingCoins)
      unless (lpeAmountReserved > RC 0) (Left VSENegativeAmountOfReservedCoins)

{-# INLINABLE lendingStateMachine #-}
lendingStateMachine :: Caller -> LendingContext -> LendingStateMachine
lendingStateMachine caller context =
  SM.mkStateMachine Nothing (transition caller context) (const False)

{-# INLINABLE mkLendingSMValidator #-}
mkLendingSMValidator :: (Caller, LendingContext) -> Scripts.ValidatorType LendingStateMachine
mkLendingSMValidator =
  SM.mkValidator . uncurry lendingStateMachine

lendingSMInstance
  :: Caller
  -> LendingContext
  -> Scripts.ScriptInstance LendingStateMachine
lendingSMInstance caller context = Scripts.validator @LendingStateMachine
    validator
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.wrapValidator
        validator
          = $$(PlutusTx.compile [|| mkLendingSMValidator ||])
          `PlutusTx.applyCode` PlutusTx.liftCode (caller, context)

-- lendingClient :: HasLendingCaller => SM.StateMachineClient LendingState LendingAction
-- lendingClient =
--   SM.mkStateMachineClient
--     (SM.StateMachineInstance (lendingStateMachine ?caller) lendingSMInstance)

genStateMachineClient
  :: Caller
  -> LendingContext
  -> Scripts.ScriptInstance LendingStateMachine
  -> SM.StateMachineClient LendingState LendingAction
genStateMachineClient caller context inst =
  let machine = lendingStateMachine caller context
  in SM.mkStateMachineClient (SM.StateMachineInstance machine inst)

lendingValidator :: (HasLendingCaller, HasLendingContext) => Validator
lendingValidator = Scripts.validatorScript (lendingSMInstance ?caller ?lendingContext)

lendingAddress :: (HasLendingCaller, HasLendingContext) => Ledger.Address
lendingAddress = scriptAddress lendingValidator


----------------------------------------------------------------
-- Endpoints
----------------------------------------------------------------

data DepositParams
  = DepositParams
  { dpToken          :: TokenName
  , dpMbOnBehalfOf   :: Lender
  , dpMbReferalCode  :: ReferalCode
  , dpAmountReserved :: AmountReserved
  } deriving stock Generic
    deriving anyclass (Aeson.ToJSON, Aeson.FromJSON, ToSchema)

data WithdrawParams
  = WithdrawParams
  { wpToken          :: TokenName
  , wpAmountReserved :: AmountReserved
  -- , wpRecipient      :: Lender
  } deriving stock Generic
    deriving anyclass (Aeson.ToJSON, Aeson.FromJSON, ToSchema)

type HasLendingContext = ?lendingContext :: LendingContext
type HasLendingClient = ?lendingClient :: SM.StateMachineClient LendingState LendingAction
type HasLendingCaller = ?caller :: Caller

type LendingSchema =
    BlockchainActions
        .\/ Endpoint "initialize" LendingContext
        .\/ Endpoint "deposit" DepositParams
        .\/ Endpoint "withdraw" WithdrawParams
        -- .\/ Endpoint "gen referal code" ()

-- TODO: for some reason dynamic types on playground fail to resolve this
-- type HasLendingSchema s =
--   ( HasBlockchainActions s
--   , HasEndpoint "initialize" LendingContext s
--   , HasEndpoint "deposit" DepositParams s
--   , HasEndpoint "withdraw" WithdrawParams s
--   )

-- | Dry run substitution step and log the output
logValidateTransition
  :: ( AsLendingException' e
     , HasLendingCaller
     , HasLendingClient
     , HasLendingContext
     )
  => LendingAction
  -> Contract w LendingSchema e ()
logValidateTransition action = do
  currentState <- SM.getOnChainState ?lendingClient
  case currentState of
    Just ((TypedScriptTxOut{tyTxOutData}, _), _) -> do
      case validateState tyTxOutData of
        Right _ -> logInfo @String "Current state is OK"
        Left  e -> logWarn @String $ "Current state is invalid: " <> show e <> ". The transition may still be allowed."
      logInfo @String $ "Current state is: " <> show tyTxOutData
      case subst ?caller ?lendingContext tyTxOutData action of
        Right (_, newState) -> case validateState newState of
          Right _ -> logInfo @String "Substituted state is OK"
          Left  e -> logWarn @String $ "Substituted state is invalid: " <> show e <> ". The transition is disallowed."
        Left e -> logWarn @String $ "Cannot substitute state: " <> show e
    Nothing -> logWarn @String "Unable to find current state."

-- | Deposit funds
deposit
  :: ( AsLendingException' e
     , HasLendingContext
     , HasLendingClient
     )
  => DepositParams
  -> Contract w LendingSchema e ()
deposit DepositParams{..} = do
  caller <- pubKeyHash <$> ownPubKey
  let ?caller = caller
  let
    action
      = Deposit
      { laDepositAmount = dpAmountReserved
      , laReferal = (dpMbOnBehalfOf, dpMbReferalCode)
      }
  logValidateTransition action
  logInfo @String $ "Depositing " <> show dpAmountReserved <> " tokens."
  void $ mapError (_LEStateMachineException #)
       $ SM.runStep ?lendingClient action
  logValidateTransition action

-- | Withdraw funds
withdraw
  :: ( AsLendingException' e
     , HasLendingContext
     , HasLendingClient
     )
  => WithdrawParams
  -> Contract w LendingSchema e ()
withdraw WithdrawParams{..} = do
  caller <- pubKeyHash <$> ownPubKey
  let ?caller = caller
  void $ mapError (_LEStateMachineException #)
       $ SM.runStep ?lendingClient (Withdraw wpAmountReserved)

-- genReferalCode
--   :: ( AsLendingException' e
--      , HasLendingContext
--      )
--   => Contract w LendingSchema e ()
-- genReferalCode = do
--   caller <- pubKeyHash <$> ownPubKey
--   let ?caller = caller
--   void $ mapError (_LEStateMachineException #)
--        $ SM.runStep lendingClient GenReferalCode

endpoints
  :: AsLendingException' e
  => Contract () LendingSchema e ()
endpoints = do
  lendingContext <- mapError (_LEInitializeException #) $ endpoint @"initialize"
  caller <- pubKeyHash <$> ownPubKey
  let
    ?lendingContext = lendingContext
    ?caller = caller
    ?lendingClient = genStateMachineClient caller lendingContext (lendingSMInstance caller lendingContext)
  logInfo @String $ "Initialized by " <> show ?caller <> "the context " <> show ?lendingContext
  _ <- mapError (_LEStateMachineException #) $ SM.runInitialise ?lendingClient initialState mempty
  forever $ List.foldl1 select
    [ endpoint @"deposit"          & mapError (_LEDepositException        #) >>= deposit
    , endpoint @"withdraw"         & mapError (_LEWithdrawException       #) >>= withdraw
    -- , endpoint @"gen referal code" & mapError (_LEGenReferalCodeException #) >>= genReferalCode
    ]

PlutusTx.unstableMakeIsData ''LC
PlutusTx.makeLift ''LC
PlutusTx.unstableMakeIsData ''RC
PlutusTx.makeLift ''RC
PlutusTx.unstableMakeIsData ''LendingAction
PlutusTx.makeLift ''LendingAction
PlutusTx.unstableMakeIsData ''LendingState
PlutusTx.makeLift ''LendingState
PlutusTx.unstableMakeIsData ''LendingPoolEntry
PlutusTx.makeLift ''LendingPoolEntry
PlutusTx.unstableMakeIsData ''LendingContext
PlutusTx.makeLift ''LendingContext

mkSchemaDefinitions ''LendingSchema

mkKnownCurrencies []

