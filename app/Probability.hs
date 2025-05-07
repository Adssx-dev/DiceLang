{-# LANGUAGE RecordWildCards #-}
module Probability
where

import Types

import qualified Data.Map as M
import Data.List

type Histogram = M.Map Int Double

exprHistogram :: Expression -> Histogram
exprHistogram (ExpConst value) = M.fromList [(value, 1.0)]
exprHistogram (ExpDice dice) = multiDiceHistogram dice (count dice)
exprHistogram (ExpAdd expr1 expr2) = mergeHistograms (+) (exprHistogram expr1) (exprHistogram expr2)
exprHistogram (ExpSub expr1 expr2) = mergeHistograms (-) (exprHistogram expr1) (exprHistogram expr2)
exprHistogram (ExpMul expr1 expr2) = mergeHistograms (*) (exprHistogram expr1) (exprHistogram expr2)
exprHistogram (ExpDiv expr1 expr2) = mergeHistograms div (exprHistogram expr1) (exprHistogram expr2)
exprHistogram (ExpMax expr1 expr2) = mergeHistograms max (exprHistogram expr1) (exprHistogram expr2)
exprHistogram (ExpMin expr1 expr2) = mergeHistograms min (exprHistogram expr1) (exprHistogram expr2)
exprHistogram (ExpParenthesis expr) = exprHistogram expr

singleDieHistogram die@Dice{..} = M.fromList $ 
    zip 
        [1..maxValue] 
        (replicate maxValue (1.0 / (fromIntegral maxValue :: Double)))

multiDiceHistogram die@Dice{..} 0 = M.fromList [(0, 1)]
multiDiceHistogram die@Dice{..} remaining = mergeHistograms (+) (singleDieHistogram die) (multiDiceHistogram die (remaining - 1))

mergeHistograms :: (Int -> Int -> Int) -> Histogram -> Histogram -> Histogram
mergeHistograms fcn h1 h2 = M.fromList newList
    where
        l1 = M.toList h1
        l2 = M.toList h2
        newList = map (\x -> (fst $ head x, sum $ map snd x)) $
            groupBy (\x y -> fst x == fst y) $ 
            sortBy (\x y -> compare (fst x) (fst y)) $ 
            concatMap (\x -> map (mergeElems fcn x) l2) l1
 
mergeElems :: Num a => (a -> a -> a) -> (a, Double) -> (a, Double) -> (a, Double)
mergeElems fcn (e1Val, e1Prob) (e2Val, e2Prob) = (fcn e1Val e2Val, e1Prob * e2Prob)
