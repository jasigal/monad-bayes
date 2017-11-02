{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module Main where

import           Control.Monad                  (replicateM)

import           Control.Monad.Bayes.Class
import           Control.Monad.Bayes.Enumerator
import           Control.Monad.Bayes.Simple

import           UrnModel

-- Functions from models/BetaBin.hs
count :: [Bool] -> Int
count = length . filter id

cond :: MonadBayes m => m [Bool] -> m [Bool]
cond d = do
  (first:second:third:rest) <- d
  condition (first  == True)
  condition (second == True)
  condition (third  == False)
  return rest

-- Changed functions to use the urn.
urn :: (MonadDist m, MonadUrn t m) => Int -> t m [Bool]
urn n = do
  u <- new
  replicateM n (toss u)

-- Use UrnImpl GADT to choose the implementation.
model :: MonadBayes m => Int -> m Int
model n = fmap count $ cond $ runWithImpl Hoppe $ urn (n+3)

-- Enumerate works only with Hoppe, as other contains unhandled continuity (I
-- don't know enough about the library yet to have this work for both).
main :: IO ()
main = print $ enumerate @Double (model 5)
-- >> [(0,4.761904761904764e-2),(1,0.11904761904761915),(2,0.1904761904761906),
--     (3,0.2380952380952382),(4,0.23809523809523803),(5,0.16666666666666674)]
