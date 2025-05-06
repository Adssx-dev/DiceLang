{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Evaluator
where
import System.Random ( uniformR, RandomGen )
import Types


evaluateDieInternal :: RandomGen a => a -> Dice -> (Int, a)
evaluateDieInternal randomGen die = value 
    where 
        value = uniformR (1 :: Int, maxValue die) randomGen 

        
evaluateDice :: RandomGen a => a -> Dice -> Int -> (Int, a)
evaluateDice randomGen Dice{count=0} tmpSum = (tmpSum, randomGen)
evaluateDice randomGen die@Dice{..} tmpSum = 
    let 
        (currentValue, newRandomGen) = evaluateDieInternal randomGen die
    in 
        evaluateDice newRandomGen Dice{count=count-1,maxValue=maxValue} (tmpSum  + currentValue)

evaluateExpression :: RandomGen a => a -> Expression -> (Int, a)
evaluateExpression rg (ExpDice dice) = evaluateDice rg dice 0
evaluateExpression rg (ExpConst value) = (value, rg)
evaluateExpression rg (ExpAdd expr1 expr2) = evaluateWithOperator rg expr1 expr2 (+)
evaluateExpression rg (ExpSub expr1 expr2) = evaluateWithOperator rg expr1 expr2 (-)
evaluateExpression rg (ExpMul expr1 expr2) = evaluateWithOperator rg expr1 expr2 (*)
evaluateExpression rg (ExpDiv expr1 expr2) = evaluateWithOperator rg expr1 expr2 div
evaluateExpression rg (ExpMax expr1 expr2) = evaluateWithOperator rg expr1 expr2 max
evaluateExpression rg (ExpMin expr1 expr2) = evaluateWithOperator rg expr1 expr2 min
evaluateExpression rg (ExpParenthesis expr) = evaluateExpression rg expr

-- Utility function for evaluateExpression that does the handliung of generators as most of them differ only by 
-- the function to call after evaluating both expressions
evaluateWithOperator :: RandomGen b => b -> Expression -> Expression -> (Int -> Int -> Int) -> (Int, b)
evaluateWithOperator rg expr1 expr2 operator = (operator res1 res2, rg3)
    where
        (res1, rg2) = evaluateExpression rg expr1
        (res2, rg3) = evaluateExpression rg2 expr2