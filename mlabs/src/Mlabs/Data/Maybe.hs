-- | Missing primitives for Maybe
module Mlabs.Data.Maybe(
  mapM_
) where

import PlutusTx.Prelude ( Monad(return), Maybe(..) )

{-# INLINABLE mapM_ #-}
mapM_ :: Monad f => (a -> f ()) -> Maybe a -> f ()
mapM_ f = \case
  Nothing -> return ()
  Just a  -> f a

