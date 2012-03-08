{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Main where


import Data.Char (chr,ord)
import Data.List (foldl')
import System.Random (mkStdGen, random, randoms)
import System.IO(IOMode(..), hClose, hGetContents, openFile)

import GA (Entity(..), GAConfig(..), 
           evolveVerbose, randomSearch)
  
import TAC
import Control.Monad.Random

import Control.Monad.Identity

ifRand :: (RandomGen g) => Float -> (Rand g a) -> a -> Rand g a
ifRand prob act def = do
  p <- getRandomR (0.0, 1.0)
  if p < prob then
    act
  else
    return def

data Score = Score Double Int
             deriving (Show, Read, Eq)

scoreToDouble (Score diff len) = if len > 5 then diff + ((fromIntegral len)/2) else diff + 100

instance Ord Score where
  compare a b = compare (scoreToDouble a) (scoreToDouble b)

instance Entity Program Score () () IO where
  -- generate a random entity, i.e. a random string
  genRandom _ seed = return $ evalRand getRandom g
    where
        g = mkStdGen seed
  
  -- crossover operator
  crossover _ _ seed e1 e2 = return $ Just $ Program ([o1,o2] !! pickout) i
    where
      g = mkStdGen seed
      (pickout, g') = randomR (0,1) g
      Program o1 i1 = e1
      Program o2 i2 = e2
      cps = zipWith (\x y -> [x,y]) i1 i2
      picks = map (flip mod 2) $ randoms g'
      i = zipWith (!!) cps picks

  -- mutation operator
  mutation _ p seed e = return $ Just $ evalRand act g
    where
      g = mkStdGen seed
      Program o is = e
      possiblyReplaceInst i = ifRand 0.1 getRandom i
      mutateInst (Instruction op x y accum z) = do
        op' <- ifRand 0.1 getRandom op
        x' <- ifRand 0.05 getRandom x
        y' <- ifRand 0.05 getRandom y
        accum' <- ifRand 0.1 getRandom accum
        z' <- ifRand 0.01 randomReg z
        return $ Instruction op' x' y' accum' z'
      adddropInst i = do
        a <- ifRand 0.1 (return []) [i]
        b <- ifRand 0.1 (liftM (\x->[x]) getRandom) []
        c <- ifRand 0.1 (return [i]) []
        return (a ++ b ++ c)
      act = do
        is' <- mapM possiblyReplaceInst is
        is'' <- mapM mutateInst is'
        is''' <- liftM concat $ mapM adddropInst is''
        o' <- ifRand 0.01 randomOutReg o
        return $ Program o' is'''
        
      
      
  
  -- score: distance between current string and target
  -- NOTE: lower is better
  score _ e = do
    --print $ "Last V: " ++ (show lastv)
    --print $ "len: " ++ (show len)
    return $ Just $ Score vErr len
    where 
      lastv = last $ evalProgram 100 e 
      vErr = abs (lastv - (3.141579*2))
      Program _ is = e
      len = fromIntegral $ length is
    
  -- whether or not a scored entity is perfect
  isPerfect (_,Score d _) = d < 0.01
  
  showGeneration gi (_,archive) = "best entity (gen. " 
                                  ++ show gi ++ "): " ++ (show e) 
                                  ++ " [fitness: " ++ show fitness ++ "]"
                                    where
                                      (Just fitness, e) = head archive

main :: IO() 
main = do
        g <- newStdGen
        let cfg = GAConfig 
                    100 -- population size
                    25 -- archive size (best entities to keep track of)
                    300 -- maximum number of generations
                    0.8 -- crossover rate (% of entities by crossover)
                    0.2 -- mutation rate (% of entities by mutation)
                    0.0 -- parameter for crossover (not used here)
                    0.2 -- parameter for mutation (% of replaced letters)
                    False -- whether or not to use checkpointing
                    False -- don't rescore archive in each generation
        -- Do the evolution!
        -- Note: if either of the last two arguments is unused, just use () as a value
        es <- evolveVerbose g cfg () ()
        let e = snd $ head es :: Program
        
        putStrLn $ "best entity (GA): " ++ (show e)
        print $ evalProgram 100 e

        -- Compare with random search with large budget
        -- 100k random entities, equivalent to 1000 generations of GA
        es' <- randomSearch g 100 () ()
        let e' = snd $ head es' :: Program
       
        putStrLn $ "best entity (random search): " ++ (show e')
        print $ evalProgram 100 e

