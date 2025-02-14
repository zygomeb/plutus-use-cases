-- | Server for lendex application
module Mlabs.Lending.Contract.Server(
  -- * Contract monads
    UserContract
  , OracleContract
  , AdminContract
  -- * Endpoints
  , userEndpoints
  , oracleEndpoints
  , adminEndpoints
  -- * Errors
  , StateMachine.LendexError
) where

import Prelude

import Control.Monad (forever)
import Data.List.Extra (firstJust)
import Data.Map (toList)
import Ledger.Constraints (ownPubKeyHash, monetaryPolicy, mustIncludeDatum)
import Plutus.Contract qualified as Contract
import Plutus.V1.Ledger.Api (Datum, getSlot)
import Plutus.V1.Ledger.Crypto (pubKeyHash)

import Mlabs.Emulator.Types (ownUserId)
import Mlabs.Lending.Contract.Api qualified as Api
import Mlabs.Lending.Contract.Forge (currencyPolicy, currencySymbol)
import Mlabs.Lending.Contract.StateMachine qualified as StateMachine
import Mlabs.Lending.Logic.Types qualified as Types
import Mlabs.Plutus.Contract (getEndpoint, readDatum, selects)

-- | User contract monad
type UserContract a = Contract.Contract () Api.UserSchema StateMachine.LendexError a

-- | Oracle contract monad
type OracleContract a = Contract.Contract () Api.OracleSchema StateMachine.LendexError a

-- | Admin contract monad
type AdminContract a = Contract.Contract () Api.AdminSchema StateMachine.LendexError a

----------------------------------------------------------
-- endpoints

-- | Endpoints for user
userEndpoints :: Types.LendexId -> UserContract  ()
userEndpoints lid = forever $ selects
  [ act $ getEndpoint @Api.Deposit
  , act $ getEndpoint @Api.Borrow
  , act $ getEndpoint @Api.Repay
  , act $ getEndpoint @Api.SwapBorrowRateModel
  , act $ getEndpoint @Api.SetUserReserveAsCollateral
  , act $ getEndpoint @Api.Withdraw
  , act $ getEndpoint @Api.LiquidationCall
  ]
  where
    act :: Api.IsUserAct a => UserContract a -> UserContract ()
    act readInput = readInput >>= userAction lid


-- | Endpoints for price oracle
oracleEndpoints :: Types.LendexId -> OracleContract ()
oracleEndpoints lid = forever $ selects
  [ act $ getEndpoint @Api.SetAssetPrice
  ]
  where
    act :: Api.IsPriceAct a => OracleContract a -> OracleContract ()
    act readInput = readInput >>= priceOracleAction lid

-- | Endpoints for admin
adminEndpoints :: Types.LendexId -> AdminContract ()
adminEndpoints lid = do
  getEndpoint @Api.StartParams >>= (startLendex lid)
  forever $ selects
    [ act $ getEndpoint @Api.AddReserve
    ]
  where
    act :: Api.IsGovernAct a => AdminContract a -> AdminContract ()
    act readInput = readInput >>= adminAction lid

-- actions

userAction :: Api.IsUserAct a => Types.LendexId -> a -> UserContract ()
userAction lid input = do
  pkh <- pubKeyHash <$> Contract.ownPubKey
  act <- getUserAct input
  inputDatum <- findInputStateDatum lid
  let lookups = monetaryPolicy (currencyPolicy lid) <>
                ownPubKeyHash  pkh
      constraints = mustIncludeDatum inputDatum
  StateMachine.runStepWith lid act lookups constraints

priceOracleAction :: Api.IsPriceAct a => Types.LendexId -> a -> OracleContract ()
priceOracleAction lid input = StateMachine.runStep lid =<< getPriceAct input

adminAction :: Api.IsGovernAct a => Types.LendexId -> a -> AdminContract ()
adminAction lid input = StateMachine.runStep lid =<< getGovernAct input

startLendex :: Types.LendexId -> Api.StartParams -> AdminContract ()
startLendex lid Api.StartParams{..} =
  StateMachine.runInitialise lid  (Types.initLendingPool (currencySymbol lid) sp'coins (fmap Types.UserId sp'admins) (fmap Types.UserId sp'oracles)) sp'initValue

----------------------------------------------------------
-- to act conversion

-- | Converts endpoint inputs to logic actions
getUserAct :: Api.IsUserAct a => a -> UserContract Types.Act
getUserAct act = do
  uid <- ownUserId
  t   <- getCurrentTime
  pure $ Types.UserAct t uid $ Api.toUserAct act

-- | Converts endpoint inputs to logic actions
getPriceAct :: Api.IsPriceAct a => a -> OracleContract Types.Act
getPriceAct act = do
  uid <- ownUserId
  t   <- getCurrentTime
  pure $ Types.PriceAct t uid $ Api.toPriceAct act

getGovernAct :: Api.IsGovernAct a => a -> AdminContract Types.Act
getGovernAct act = do
  uid <- ownUserId
  pure $ Types.GovernAct uid $ Api.toGovernAct act

getCurrentTime :: (Contract.HasBlockchainActions s, Contract.AsContractError e) => Contract.Contract w s e Integer
getCurrentTime = getSlot <$> Contract.currentSlot

----------------------------------------------------------

findInputStateDatum :: Types.LendexId -> UserContract Datum
findInputStateDatum lid = do
  utxos <- Contract.utxoAt (StateMachine.lendexAddress lid)
  maybe err pure $ firstJust (readDatum . snd) $ toList utxos
  where
    err = Contract.throwError $ StateMachine.toLendexError "Can not find Lending app instance"


