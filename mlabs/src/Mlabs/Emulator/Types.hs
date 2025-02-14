{-# OPTIONS_GHC -fno-specialize #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-specialize #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fobject-code #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Mlabs.Emulator.Types(
    UserId(..)
  , Coin
  , adaCoin
  , ownUserId
) where

import PlutusTx.Prelude

import Data.Aeson (FromJSON, ToJSON)
import Prelude qualified as Hask
import GHC.Generics ( Generic )
import Plutus.Contract (HasBlockchainActions, AsContractError, Contract, ownPubKey)
import Plutus.V1.Ledger.Ada qualified as Ada
import Plutus.V1.Ledger.Contexts (pubKeyHash)
import Plutus.V1.Ledger.Crypto (PubKeyHash(..))
import Plutus.V1.Ledger.Value (AssetClass(..))
import PlutusTx ( unstableMakeIsData ) 
import Playground.Contract (ToSchema)

-- | Address of the wallet that can hold values of assets
data UserId
  = UserId PubKeyHash  -- user address
  | Self               -- addres of the lending platform
  deriving stock (Show, Generic, Hask.Eq, Hask.Ord)
  deriving anyclass (FromJSON, ToJSON)


instance Eq UserId where
  {-# INLINABLE (==) #-}
  Self == Self = True
  UserId a == UserId b = a == b
  _ == _ = False

{-# INLINABLE adaCoin #-}
adaCoin :: Coin
adaCoin = AssetClass (Ada.adaSymbol, Ada.adaToken)

-- | Custom currency
type Coin = AssetClass

deriving newtype instance ToSchema AssetClass

PlutusTx.unstableMakeIsData ''UserId

-- | Get user id of the wallet owner.
ownUserId :: (AsContractError e, HasBlockchainActions s) => Contract w s e UserId
ownUserId = fmap (UserId . pubKeyHash) ownPubKey
