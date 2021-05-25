{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module Main (main) where

--------------------------------------------------------------------------------

import GHC.Generics
import Prelude

--------------------------------------------------------------------------------

import Control.Monad (forM, forM_, void, when, forever)
import Control.Monad.Freer (Eff, Member, interpret, reinterpret, type (~>))
import Control.Monad.Freer.Error (Error, throwError)
import Control.Monad.Freer.Extras.Log (LogMsg, logDebug)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson (FromJSON, Result (..), ToJSON, encode, fromJSON)
import qualified Data.Aeson as Aeson
import Data.Bifunctor (Bifunctor (first))
import Data.ByteString.Char8 qualified as Char8
import Data.Default.Class
import Data.List (intercalate)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Row (type Empty, type (.\\))
import Data.Semigroup qualified as Semigroup
import Data.Text.Prettyprint.Doc (Pretty (..), viaShow)

--------------------------------------------------------------------------------

import Cardano.Prelude qualified as Cardano
import Cardano.Wallet.Types qualified (WalletInfo (..))
import Control.Concurrent.Availability qualified as Availability
import Plutus.Contract qualified as Contract
import Plutus.Contract.Effects.ExposeEndpoint qualified as Cardano
import Plutus.Contract.Resumable (Response)
import Plutus.Contract.Schema (Event, Handlers, Input, Output)
import Plutus.Contract.State (Contract, ContractRequest (..), ContractResponse (..))
import Plutus.Contract.State qualified as Contract
import Plutus.PAB.Core qualified as PAB
import Plutus.PAB.Core.ContractInstance.STM qualified as Cardano
import Plutus.PAB.Effects.Contract (ContractEffect (..), PABContract (..))
import Plutus.PAB.Effects.Contract.Builtin (Builtin, SomeBuiltin (..))
import Plutus.PAB.Effects.Contract.Builtin qualified as Builtin
import Plutus.PAB.Events.Contract (ContractPABRequest)
import Plutus.PAB.Events.Contract qualified as Contract
import Plutus.PAB.Events.ContractInstanceState (PartiallyDecodedResponse)
import Plutus.PAB.Events.ContractInstanceState qualified as Contract
import Plutus.PAB.Monitoring.PABLogMsg (ContractEffectMsg (..), PABMultiAgentMsg (..))
import Plutus.PAB.Simulator (Simulation, SimulatorContractHandler, SimulatorEffectHandlers)
import Plutus.PAB.Simulator qualified as Simulator
import Plutus.PAB.Types (PABError (..), WebserverConfig (..))
import Plutus.PAB.Webserver.Server qualified as PAB
import Plutus.V1.Ledger.Ada qualified as Ada
import Plutus.V1.Ledger.Crypto qualified as Ledger
import Plutus.V1.Ledger.Slot qualified as Ledger (Slot (..))
import Plutus.V1.Ledger.Value qualified as Ledger
import Plutus.V1.Ledger.Value qualified as Value 
import PlutusTx.AssocMap qualified as AssocMap
import PlutusTx.Prelude qualified as PlutusTx
import PlutusTx.Prelude ((%))
import Wallet.Emulator.Types (Wallet (..), walletPubKey)
import Wallet.Emulator.Wallet qualified as Wallet

--------------------------------------------------------------------------------

import qualified Mlabs.Lending.Contract.Lendex as Lendex
import qualified Mlabs.Lending.Logic.Types as Lendex
import Mlabs.Lending.Logic.Types (Coin, UserAct(..), UserId(..))
import PrettyLogger

--------------------------------------------------------------------------------

logAction :: MonadIO m => String -> m ()
logAction str = logPrettyColorBold (Vibrant Green) (withNewLines $ str)

logBalance :: MonadIO m => String -> Value.Value -> m ()
logBalance wallet val = do
  logNewLine
  logPrettyBgColor 40 (Vibrant Cyan) (Standard Black) (wallet ++ " BALANCE")
  logNewLine
  logPrettyColor (Vibrant Cyan) (formatValue val)
  logNewLine

formatValue :: Value.Value -> String
formatValue (Value.Value m) = intercalate "\n" (tokenMapToList m)

tokenMapToList :: AssocMap.Map Value.CurrencySymbol (AssocMap.Map Value.TokenName Integer) -> [String]
tokenMapToList m = prettyShow <$> AssocMap.toList m
  where
    prettyShow (_, v) = formatFirst $ AssocMap.toList v
    formatFirst [] = "\n"
    formatFirst tokens = formatTokenValue $ head $ tokens
    formatTokenValue (name, value) =
      case name of
        "" -> "Ada    " ++ (show value)
        (Value.TokenName n) -> (Char8.unpack n) ++ "   " ++ (show value)


main :: IO ()
main = void $
  Simulator.runSimulationWith handlers $ do
    shutdown <- PAB.startServerDebug

    cidInit <- Simulator.activateContract (Wallet 1) Init

    -- _simulatorMagic <- Simulator.activateContract

    -- The initial spend is enough to identify the entire market, provided the initial params are also clear.
    -- TODO: get pool info here.
    -- not quite sure how to get the pool out of this state machine stuff.
    -- apparently it is also the NFT approach?
    lendingPool <- flip Simulator.waitForState cidInit $ \json -> case fromJSON json of
      Success (Just (Semigroup.Last (someJson :: Lendex.LendingPool))) -> Just someJson
      _ -> Nothing

    _ <- Simulator.waitUntilFinished cidInit

    let wallets = [Wallet i | i <- [2 .. 4]]

    cIdsUsers <- fmap Map.fromList $
      forM wallets $ \w -> do
        cId <- Simulator.activateContract w $ User lendingPool
        _ <- Simulator.waitForEndpoint cId "user-action"
        pure (w, cId)

    let cId2 = cIdsUsers Map.! Wallet 2
    let cId3 = cIdsUsers Map.! Wallet 3
    let _cId4 = cIdsUsers Map.! Wallet 4

    ------------------------------------
    -- Demo Simulation
    -- wallet 2 deposits 100 ada
    -- wallet 3 deposits 100 ada
    -- wallet 3 collateralizes 100 aAda
    -- wallet 3 borrows 70 ada
    -- wallet 3 repays the borrow
    -- wallet 3 de-collateralizes 100 aAda
    -- wallet 3 withdraws 100 ada

    let logBalance2 = logBalance "WALLET 2"
    let logBalance3 = logBalance "WALLET 3"

    _ <- Simulator.waitNSlots 2
    logAction "Initial wallet balances"
    logBalance2 =<< Simulator.valueAt (Wallet.walletAddress (Wallet 2))
    logBalance3 =<< Simulator.valueAt (Wallet.walletAddress (Wallet 3))
    logNewLine
    _ <- Simulator.waitNSlots 16
    _ <- Simulator.callEndpointOnInstance cId2 "user-action" (depositAct 100)
    _ <- Simulator.waitNSlots 2
    logAction "Wallet 2 called 'deposit' with 100 Ada"
    logBalance2 =<< Simulator.valueAt (Wallet.walletAddress (Wallet 2))
    logNewLine

-- begin wallet 3 logging balances
    _ <- Simulator.waitNSlots 18
    _ <- Simulator.callEndpointOnInstance cId3 "user-action" (depositAct 100)
    _ <- Simulator.waitNSlots 2
    logAction "Wallet 3 called 'deposit' with 100 Ada"
    logBalance3 =<< Simulator.valueAt (Wallet.walletAddress (Wallet 3))
    logNewLine

    _ <- Simulator.waitNSlots 18
    _ <- Simulator.callEndpointOnInstance cId3 "user-action" (depositAct 100)
    _ <- Simulator.waitNSlots 2
    logAction "Wallet 3 called 'deposit' with 100 Ada"
    logBalance3 =<< Simulator.valueAt (Wallet.walletAddress (Wallet 3))
    logNewLine
   
    _ <- Simulator.waitNSlots 18
    _ <- Simulator.callEndpointOnInstance cId3 "user-action" (collateralize)
    _ <- Simulator.waitNSlots 2
    logAction "Wallet 3 called 'collateralize'"
    logBalance3 =<< Simulator.valueAt (Wallet.walletAddress (Wallet 3))
    logNewLine

    _ <- Simulator.waitNSlots 18
    _ <- Simulator.callEndpointOnInstance cId3 "user-action" (borrow 70)
    _ <- Simulator.waitNSlots 2
    logAction "Wallet 3 called 'borrow' with 70 Ada"
    logBalance3 =<< Simulator.valueAt (Wallet.walletAddress (Wallet 3))
    logNewLine

    _ <- Simulator.waitNSlots 18
    _ <- Simulator.callEndpointOnInstance cId3 "user-action" (repay 90)
    _ <- Simulator.waitNSlots 2
    logAction "Wallet 3 called 'repay' with 90 Ada"
    logBalance3 =<< Simulator.valueAt (Wallet.walletAddress (Wallet 3))
    logNewLine

    _ <- Simulator.waitNSlots 18
    _ <- Simulator.callEndpointOnInstance cId3 "user-action" (decollateralize)
    _ <- Simulator.waitNSlots 2
    logAction "Wallet 3 called 'decollateralize'"
    logBalance "WALLET 3" =<< Simulator.valueAt (Wallet.walletAddress (Wallet 3))
    logNewLine

    _ <- Simulator.waitNSlots 18
    _ <- Simulator.callEndpointOnInstance cId3 "user-action" (withdraw 100)
    _ <- Simulator.waitNSlots 2
    logAction "Wallet 3 called 'withdraw' with 100 Ada"
    logBalance "WALLET 3" =<< Simulator.valueAt (Wallet.walletAddress (Wallet 3))
    logNewLine

    _ <- forever $ do
      liftIO $ print @String "---------------------------------------------  -> UPDATED BALANCES:"
      _ <- Simulator.waitNSlots 10
      _ <- Simulator.logBalances @(Builtin AaveContracts) =<< Simulator.currentBalances
      pure () 
-- we can add a demo that does liquidation later
    shutdown

data AavePAB

data AaveContracts
  = Init
  | User Lendex.LendingPool 
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Pretty AaveContracts where
  pretty = viaShow

instance PABContract AavePAB where
  type ContractDef AavePAB = AaveContracts
  type State AavePAB = PartiallyDecodedResponse ContractPABRequest

  serialisableState _ = id

handleLendexContract ::

  ( Member (Error PABError) effs
  , Member (LogMsg (PABMultiAgentMsg (Builtin AaveContracts))) effs
  ) =>
  ContractEffect (Builtin AaveContracts)
    ~> Eff effs
handleLendexContract = Builtin.handleBuiltin getSchema getContract
  where
    getSchema = \case
      Init -> Builtin.endpointsToSchemas @Lendex.GovernLendexSchemaOnly
      User _ -> Builtin.endpointsToSchemas @Lendex.UserLendexSchemaOnly
    getContract = \case
      Init -> SomeBuiltin (Lendex.startLendex startParams)
      -- what the hell do we call with lending pool to get this to work?
      -- is there something special we need to do to run a statemachine client?
      User _ -> SomeBuiltin (Lendex.userEndpoints)

handlers :: SimulatorEffectHandlers (Builtin AaveContracts)
handlers =
  Simulator.mkSimulatorHandlers @(Builtin AaveContracts) [] $
    interpret handleLendexContract

startParams :: Lendex.StartParams
startParams = Lendex.StartParams
  { sp'coins     = [initCoinCfg]
  , sp'initValue =  initValue    -- ^ init value deposited to the lending app
  }

initValue :: Value.Value
initValue = Value.singleton Ada.adaSymbol Ada.adaToken 10000
             -- TODO: figure out how to support multiple currencies
             -- note: looks like we'll need a minimal minting contract to get currencies working, otherwise we can support Ada collateral, Ada borrow by removing `collateralNonBorrow uid asset` from the contract.
             -- <> Value.Singleton  () (Value.tokenName "USDc")

initCoinCfg = Lendex.CoinCfg
  { coinCfg'coin             = Value.AssetClass (Ada.adaSymbol, Ada.adaToken)
  , coinCfg'rate             = 1 % 1
  , coinCfg'aToken           = Value.tokenName "aAda"
  , coinCfg'interestModel    = Lendex.defaultInterestModel
  , coinCfg'liquidationBonus = 2 % 10
  }

adaCoin :: Coin
adaCoin = Value.AssetClass (Ada.adaSymbol, Ada.adaToken)  

depositAct :: Integer -> UserAct
depositAct i = DepositAct
      { act'amount          = i
      , act'asset           = adaCoin
      }

withdraw :: Integer -> UserAct
withdraw i = WithdrawAct
      { act'amount         = i
      , act'asset          = adaCoin
      }

collateralize :: UserAct
collateralize = SetUserReserveAsCollateralAct
      { act'asset           = adaCoin
      , act'useAsCollateral = True
      , act'portion         = 1 % 1
      }

decollateralize :: UserAct
decollateralize = SetUserReserveAsCollateralAct
      { act'asset           = adaCoin
      , act'useAsCollateral = False
      , act'portion         = 1 % 1
      }

borrow :: Integer -> UserAct
borrow i = BorrowAct
      { act'asset           = adaCoin
      , act'amount          = i
      , act'rate            = Lendex.StableRate
      }

repay :: Integer -> UserAct
repay i = RepayAct
      { act'asset           = adaCoin
      , act'amount          = i
      , act'rate            = Lendex.StableRate
      }
-- --------------------------------------------------------------------------------
