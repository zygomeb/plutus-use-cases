-- | Calculate interest rate parameters
module Mlabs.Lending.Logic.InterestRate(
    updateReserveInterestRates
  , getLiquidityRate
  , getNormalisedIncome
  , getCumulatedLiquidityIndex
  , addDeposit
  , getCumulativeBalance
)  where

import PlutusTx.Prelude
import qualified PlutusTx.Ratio as R

import Mlabs.Lending.Logic.Types

{-# INLINABLE updateReserveInterestRates #-}
updateReserveInterestRates :: Integer -> Reserve -> Reserve
updateReserveInterestRates currentTime reserve = reserve { reserve'interest = nextInterest reserve }
  where
    nextInterest Reserve{..} = reserve'interest
      { ri'liquidityRate     = liquidityRate
      , ri'liquidityIndex    = getCumulatedLiquidityIndex liquidityRate yearDelta $ reserve'interest.ri'liquidityIndex
      , ri'normalisedIncome  = getNormalisedIncome liquidityRate yearDelta $ reserve'interest.ri'liquidityIndex
      , ri'lastUpdateTime    = currentTime
      }
      where
        yearDelta      = getYearDelta lastUpdateTime currentTime
        liquidityRate  = getLiquidityRate reserve
        lastUpdateTime = reserve'interest.ri'lastUpdateTime

{-# INLINABLE getYearDelta #-}
getYearDelta :: Integer -> Integer -> Rational
getYearDelta t0 t1 = R.fromInteger (max 0 $ t1 - t0) * secondsPerSlot * R.recip secondsPerYear
  where
    secondsPerSlot = R.fromInteger 1
    secondsPerYear = R.fromInteger 31622400

{-# INLINABLE getCumulatedLiquidityIndex #-}
getCumulatedLiquidityIndex :: Rational -> Rational -> Rational -> Rational
getCumulatedLiquidityIndex liquidityRate yearDelta prevLiquidityIndex =
  (liquidityRate * yearDelta + R.fromInteger 1) * prevLiquidityIndex

{-# INLINABLE getNormalisedIncome #-}
getNormalisedIncome :: Rational -> Rational -> Rational -> Rational
getNormalisedIncome liquidityRate yearDelta prevLiquidityIndex =
  (liquidityRate * yearDelta + R.fromInteger 1) * prevLiquidityIndex

{-# INLINABLE getLiquidityRate #-}
getLiquidityRate :: Reserve -> Rational
getLiquidityRate Reserve{..} = r * u
  where
    u = getUtilisation reserve'wallet
    r = getBorrowRate (ri'interestModel reserve'interest) u

{-# INLINABLE getUtilisation #-}
getUtilisation :: Wallet -> Rational
getUtilisation Wallet{..} = wallet'borrow R.% liquidity
  where
    liquidity = wallet'deposit + wallet'borrow

{-# INLINABLE getBorrowRate #-}
getBorrowRate :: InterestModel -> Rational -> Rational
getBorrowRate InterestModel{..} u
  | u <= uOptimal = im'base + im'slope1 * (u * R.recip uOptimal)
  | otherwise     = im'base + im'slope2 * (u - uOptimal) * R.recip (R.fromInteger 1 - uOptimal)
  where
    uOptimal = im'optimalUtilisation

{-# INLINABLE addDeposit #-}
addDeposit :: Rational -> Integer -> Wallet -> Either String Wallet
addDeposit normalisedIncome amount wal
  | newDeposit >= 0 = Right wal
      { wallet'deposit       = max 0 newDeposit
      , wallet'scaledBalance = max (R.fromInteger 0) $ wallet'scaledBalance wal + R.fromInteger amount * R.recip normalisedIncome
      }
  | otherwise       = Left "Negative deposit"
  where
    newDeposit = wallet'deposit wal + amount

{-# INLINABLE getCumulativeBalance #-}
getCumulativeBalance :: Rational -> Wallet -> Rational
getCumulativeBalance normalisedIncome Wallet{..} =
  wallet'scaledBalance * normalisedIncome

