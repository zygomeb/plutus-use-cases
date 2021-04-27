{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

{-# LANGUAGE FlexibleInstances     #-}
module Lending where

import           Control.Monad             hiding (fmap)
-- import           Data.Aeson           (FromJSON, ToJSON)
import qualified Control.Lens              as Lens
import           Control.Lens.Operators
import           Control.Lens.TH           (makeClassyPrisms)
import qualified Data.Aeson                as Aeson
import qualified Data.List                 as List
import           Data.Map                  as Map
import           Data.OpenUnion.Internal   (Member)
import qualified Data.String               as String
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import           Data.Text.Prettyprint.Doc as Pretty
import           Data.Void                 (Void)
import           GHC.Generics              (Generic)
import           Ledger                    hiding (singleton)
import           Ledger.Ada                as Ada
import           Ledger.Constraints        as Constraints
import qualified Ledger.Typed.Scripts      as Scripts
import           Ledger.Value              as Value
import           Playground.Contract       (ToSchema, ensureKnownCurrencies, printJson, printSchemas, stage)
import           Playground.TH             (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types          (KnownCurrency (..))
import           Plutus.Contract           hiding (when)
import qualified PlutusTx
import           PlutusTx.Prelude          hiding (Semigroup (..), unless)
import           Prelude                   (Semigroup (..))
import           Text.Printf               (printf)

type Lender = PubKeyHash
type Amount = Integer

data LendingDatum
  = LendingDatum
    { ldStore :: [(Lender, Amount)]
    } deriving stock Show

PlutusTx.unstableMakeIsData ''LendingDatum
PlutusTx.makeLift ''LendingDatum

data LendingAction
  = Deposit Amount
  deriving stock Show

PlutusTx.unstableMakeIsData ''LendingAction
PlutusTx.makeLift ''LendingAction


{-# INLINABLE mkValidator #-}
mkValidator :: LendingDatum -> LendingAction -> ScriptContext -> Bool
mkValidator LendingDatum{..} lendingAction ctx = True
  --   traceIfFalse "beneficiary's signature missing" checkSig      &&
  --   traceIfFalse "deadline not reached"            checkDeadline
  -- where
  --   info :: TxInfo
  --   info = scriptContextTxInfo ctx

  --   checkSig :: Bool
  --   checkSig = beneficiary dat `elem` txInfoSignatories info

  --   checkDeadline :: Bool
  --   checkDeadline = from (deadline dat) `contains` txInfoValidRange info

data Lending
instance Scripts.ScriptType Lending where
    type instance DatumType Lending = LendingDatum
    type instance RedeemerType Lending = LendingAction

lendingInstance :: Scripts.ScriptInstance Lending
lendingInstance = Scripts.validator @Lending
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @LendingDatum @LendingAction

lendingValidator :: Validator
lendingValidator = Scripts.validatorScript lendingInstance

lendingAddress :: Ledger.Address
lendingAddress = scriptAddress lendingValidator



----------------------------------------------------------------
-- Exceptions
----------------------------------------------------------------

-- class Pretty p where
--   pp :: p -> Pretty.Doc a
--   default pp :: Pretty.Pretty p => p -> Pretty.Doc a
--   pp = Pretty.pretty

-- newtype PP a = PP { unPP :: a }

-- instance Lending.Pretty a => Show (PP a) where
--   show = show . pp . unPP

-- instance Lending.Pretty Text

-- -- class Exception e => EvalException e where

-- -- data SomeEvalException
-- --   = forall e . (Pretty e, EvalException e) => SomeEvalException e
-- --   deriving Show via (PP SomeEvalException)


-- instance Lending.Pretty LendingException where
--   pp = \case
--     LEStateNotFoundException  -> "state utxo not found"
--     LEOtherLendingException e -> pp e

-- data LendingException
--   = LEStateNotFoundException
--   | LEOtherLendingException Text
--   deriving stock Generic
--   deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)
--   deriving Show via PP LendingException

-- makeClassyPrisms ''LendingException

-- instance AsContractError LendingException where
--   _ContractError =
--     Lens.prism'
--       (LEOtherLendingException . Text.pack . show)
--       (const Nothing)

-- instance String.IsString LendingException where
--   fromString = LEOtherLendingException . String.fromString


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
  { dpToken        :: TokenName
  , dpMediator     :: PubKeyHash
  , dpReferralCode :: Integer
  , dpAmount       :: Integer
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
  { wpToken     :: TokenName
  , wpAmount    :: Int
  , wpRecipient :: PubKeyHash
  } deriving stock Generic
    deriving anyclass (Aeson.ToJSON, Aeson.FromJSON, ToSchema)

type LendingSchema =
    BlockchainActions
        .\/ Endpoint "deposit" DepositParams
        .\/ Endpoint "withdraw" WithdrawParams

-- instance Monoid (ScriptLookups Lending) where

deposit
  :: ( HasBlockchainActions s
    --  , AsContractError e
     )
  => DepositParams
  -> Contract w s Text ()
deposit DepositParams{..} = do
  (oref, TxOutTx{txOutTxOut = txOut, txOutTxTx = tx}, lendingDatum) <- findLendingDatum dpToken
  logInfo @String $ printf "found lending utxo with datum %s" $ show lendingDatum
  let
    redeemer = undefined

    -- lookups = mconcat
    lookups = List.foldl1 (<>) -- for some reason `Semigroup` instance is missing :/
      [ Constraints.scriptInstanceLookups lendingInstance
      , Constraints.otherScript lendingValidator
      ]

    txConstraints = mconcat
      [ mustSpendScriptOutput oref redeemer
      ]
  ledgerTx <- submitTxConstraintsWith lookups txConstraints
  void $ awaitTxConfirmed (txId ledgerTx)
  logInfo @String $ printf "successfully deposited tokens"

findLendingDatum
  :: HasBlockchainActions s
  => TokenName
  -> Contract w s Text (TxOutRef, TxOutTx, LendingDatum)
findLendingDatum tn = do
  utxos <- utxoAt lendingAddress
  let
    extractedTokens =
      [ (oref, o)
      | (oref, o) <- Map.toList utxos
      , Value.valueOf (txOutValue $ txOutTxOut o) "" tn == 1 -- TODO: probably something else besides ada should be used?
      ]
  case extractedTokens of
    []          -> throwError "utxo state not found"
    [(oref, tot@TxOutTx{txOutTxOut = txOut, txOutTxTx = tx})] -> do
      datumHash <- maybe (throwError "unexpected out type")      pure $ txOutDatumHash txOut
      Datum e   <- maybe (throwError "mismatched datum hash")    pure $ lookupDatum tx datumHash
      datum     <- maybe (throwError "datum has the wrong type") pure $ PlutusTx.fromData @LendingDatum e
      pure (oref, tot, datum)
    _           -> throwError "TODO: don't know how to merge states"

withdraw
  :: ( HasBlockchainActions s
    --  , AsContractError e
     )
  => WithdrawParams
  -> Contract w s Text ()
withdraw WithdrawParams{..} = undefined

endpoints :: Contract () LendingSchema Text ()
endpoints = List.foldl1 select entries >> endpoints
  where
    entries =
      [ endpoint @"deposit"  >>= deposit
      , endpoint @"withdraw" >>= withdraw
      ]

mkSchemaDefinitions ''LendingSchema

mkKnownCurrencies []

