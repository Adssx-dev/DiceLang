module Main where
import Types
import Evaluator
import Parsers
import System.Random (newStdGen)
import Text.Megaparsec (parse)
import Probability (exprHistogram)



main :: IO ()
main = do
    rg <- newStdGen
    let exp = ExpAdd (ExpDice Dice{maxValue=6, count= 10}) (ExpConst 1)
    let res = evaluateExpression rg exp
    let parsed = parse parseExpression "" "2d8 + 1d6 + 4"
    let histo = exprHistogram (ExpDice Dice{maxValue=6, count= 10})
    putStrLn $ "Hello, Haskell!\n"
        ++ printExpr exp
        ++ "\n"
        ++ show res
        ++ "\n"
        ++ show parsed
        ++ "\n"
        ++ show histo
    case parsed of
        Left a -> print a
        Right exp -> do
            print (evaluateExpression rg exp) 
            print (exprHistogram exp)

