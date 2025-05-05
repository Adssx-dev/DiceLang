module Main where
import Types
import Evaluator
import System.Random (newStdGen)

main :: IO ()
main = do
    rg <- newStdGen
    let exp = ExpAdd (ExpDie Dice{maxValue=6, count= 2}) (ExpConst 1)
    let res = evaluateExpression rg exp
    putStrLn $ "Hello, Haskell!\n" ++ printExpr exp ++ "\n" ++ show res
    
