{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}

module UrnModel (
  MonadUrn(..),
  UrnImpl(..),
  runWithImpl
) where

import           Control.Monad.State.Lazy

import           Control.Monad.Bayes.Class
import           Control.Monad.Bayes.Simple

--------------------------------------------------------------------------------

-- Class of urn transformers, abstracts over probabilistic model.
class MonadTrans t => UrnTrans (t :: (* -> *) -> (* -> *)) where
  -- Remove extra urn operations
  runUrnT :: Monad m => t m a -> m a

-- Defintion of urn monad class and operations.
class (MonadDist m, UrnTrans t, MonadDist (t m)) => MonadUrn t m where
  -- Urn to pass around, allows mutability of urn and multiple urns.
  data Urn t :: *
  -- Create a new urn.
  new :: Monad m => t m (Urn t)
  -- Use urn to create a Bool ("toss a coin").
  toss :: Monad m => Urn t -> t m Bool

--------------------------------------------------------------------------------

-- Beta binomial implementation. We get lots of free deriving, but Haskell
-- cannot derive HasCustomReal due to the associated type family. Hence we have
-- to create instances (albeit empty automatic ones) for MonadDist/Bayes.
newtype UrnBetaBin m a = UrnBetaBin {
  runUrnBetaBin :: StateT [Double] m a
} deriving (Functor, Applicative, Monad, MonadTrans, MonadState [Double],
            Sampleable d, Conditionable)
instance UrnTrans UrnBetaBin where
  runUrnT = (`evalStateT` []) . runUrnBetaBin
instance HasCustomReal m => HasCustomReal (UrnBetaBin m) where
  type CustomReal (UrnBetaBin m) = CustomReal m
instance MonadDist m => MonadDist (UrnBetaBin m)
instance MonadBayes m => MonadBayes (UrnBetaBin m)

-- Actual implementation (copied from BetaBin.hs)
instance MonadBayes m => MonadUrn UrnBetaBin m where
  newtype Urn UrnBetaBin = UrnBetaBin' Int
  new = do
    weight <- uniform 0 1
    weights <- get
    put (weights ++ [fromCustomReal weight])
    return (UrnBetaBin' $ length weights)
  toss (UrnBetaBin' n) = gets (!! n) >>= bernoulli . toCustomReal

--------------------------------------------------------------------------------

-- Mutable urn (Hoppe urn) implementation.
newtype UrnHoppe m a = UrnHoppe {
  runUrnHoppe :: StateT [(Integer, Integer)] m a
} deriving (Functor, Applicative, Monad, MonadTrans,
            MonadState [(Integer, Integer)], Sampleable d, Conditionable)
instance UrnTrans UrnHoppe where
  runUrnT = (`evalStateT` []) . runUrnHoppe
instance HasCustomReal m => HasCustomReal (UrnHoppe m) where
  type CustomReal (UrnHoppe m) = CustomReal m
instance MonadDist m => MonadDist (UrnHoppe m)
instance MonadBayes m => MonadBayes (UrnHoppe m)

-- Actual implementation (copied from models/BetaBin.hs)
instance MonadBayes m => MonadUrn UrnHoppe m where
  newtype Urn UrnHoppe = UrnHoppe' Int
  new = do
    urns <- get
    put (urns ++ [(1,1)])
    return (UrnHoppe' $ length urns)
  toss (UrnHoppe' n) = do
    (i,j) <- gets (!! n)
    let (a,b) = (to i, to j)
        weight = a / (a + b)
    outcome <- bernoulli weight
    let (i',j') = if outcome then (i+1,j) else (i,j+1)
    modify (\xs -> take n xs ++ [(i',j')] ++ drop (n+1) xs)
    return outcome
    where
      to = toCustomReal . fromInteger

--------------------------------------------------------------------------------

data UrnImpl t where
  BetaBin :: UrnImpl UrnBetaBin
  Hoppe :: UrnImpl UrnHoppe

runWithImpl :: (MonadDist m, MonadUrn t m) => UrnImpl t -> t m a -> m a
runWithImpl _ = runUrnT
