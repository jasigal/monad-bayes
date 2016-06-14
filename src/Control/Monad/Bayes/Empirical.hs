{-# LANGUAGE
  TupleSections,
  GeneralizedNewtypeDeriving,
  FlexibleInstances,
  FlexibleContexts
 #-}

module Control.Monad.Bayes.Empirical (
    Population,
    runPopulation,
    --Empirical,
    --runEmpirical,
    --population,
    spawn,
    resample,
    resampleN,
    --evidence,
    collapse,
    proper,
                 ) where

import Prelude hiding (all)

import Control.Monad.Trans.Class
import Control.Monad.State.Lazy
import Control.Monad.Trans.List
import Data.Number.LogFloat as LogFloat
import Data.Monoid
import qualified Data.Foldable as Fold

import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Weighted

-- | Empirical distribution represented as a set of weighted samples.
-- Forward probabilistic computation is handled by the transformed monad,
-- while conditioning is done by updating empirical weights.
-- There is no automatic normalization or aggregation of weights.
newtype Empirical m a = Empirical {unEmpirical :: Weighted (ListT m) a}
    deriving (Functor, Applicative, Monad, MonadDist, MonadBayes)

runEmpirical :: Functor m => Empirical m a -> m [(a, LogFloat)]
runEmpirical = runListT . runWeighted . unEmpirical

fromList :: Monad m => m [(a, LogFloat)] -> Empirical m a
fromList = Empirical . withWeight . ListT

instance MonadTrans Empirical where
    lift = Empirical . lift . lift

-- | The number of samples used for approximation.
population :: Monad m => Empirical m a -> m Int
population e = do
  zs <- runEmpirical e
  return (length zs)

-- | Set the number of samples for the empirical distribution.
-- Bear in mind that invoking `spawn` twice in the same computation
-- leads to multiplying the number of samples.
draw :: MonadDist m => Int -> Empirical m ()
draw n = Empirical $ lift $ ListT $ sequence $ replicate n $ return ()





newtype Population m a = Population {unPopulation :: Empirical m a}
  deriving (Functor, Applicative, Monad, MonadTrans, MonadDist, MonadBayes)

runPopulation :: Functor m => Population m a -> m [(a,LogFloat)]
runPopulation = runEmpirical . unPopulation

fromWeightedList :: Monad m => m [(a,LogFloat)] -> Population m a
fromWeightedList = Population . fromList

spawn :: MonadDist m => Int -> Population m ()
spawn n = Population (draw n) >> factor (1 / fromIntegral n)

-- | Model evidence estimator, also known as pseudo-marginal likelihood.
evidence :: MonadDist m => Population m a -> m LogFloat
evidence = fmap snd . proper

-- | Pick a sample at random from the empirical distribution,
-- according to the weights.
--collapse :: MonadDist m => Empirical m a -> m a
--collapse = fmap fst . proper

-- | A version of 'resampleN' that operates explicitly on lists.
resampleNList :: MonadDist m => Int -> [(a,LogFloat)] -> m [(a,LogFloat)]
resampleNList n ys = do
  let (xs,ws) = unzip ys
  let z = LogFloat.sum ws
  offsprings <- multinomial ys n
  let new_samples = concat [replicate k x | (x,k) <- offsprings]
  return $ map (,z / fromIntegral n) new_samples

-- | A version of 'resample' that operates explicitly on lists.
resampleList :: MonadDist m => [(a,LogFloat)] -> m [(a,LogFloat)]
resampleList ys = resampleNList (length ys) ys

-- | Resample the particles using the underlying monad.
-- Model evidence estimate is preserved in total weight.
resample :: MonadDist m => Population m a -> Population m a
resample d = fromWeightedList $ do
  ys <- runPopulation d
  resampleList ys

-- | As 'resample', but with set new population size.
resampleN :: MonadDist m => Int -> Population m a -> Population m a
resampleN n d = fromWeightedList $ do
  ys <- runPopulation d
  resampleNList n ys

-- | Properly weighted version of 'collapse', that is returned with
-- model evidence estimator from the same run.
proper :: MonadDist m => Population m a -> m (a,LogFloat)
proper = fmap head . (>>= resampleNList 1) . runPopulation


-- | Pick one sample from the empirical distribution and use model evidence as a 'factor'.
collapse :: (MonadBayes m) => Population m a -> m a
collapse e = do
  (x,p) <- proper e
  factor p
  return x
