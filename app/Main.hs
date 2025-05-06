module Main where
import Types
import Evaluator
import Parsers
import System.Random (newStdGen)
import Text.Megaparsec (parse)



main :: IO ()
main = do
    rg <- newStdGen
    let exp = ExpAdd (ExpDice Dice{maxValue=6, count= 10}) (ExpConst 1)
    let res = evaluateExpression rg exp
    let parsed = parse parseExpression "" "1+ 7d6 * 3"
    putStrLn $ "Hello, Haskell!\n"
        ++ printExpr exp
        ++ "\n"
        ++ show res
        ++ "\n"
        ++ show parsed
    case parsed of
        Left a -> print a
        Right exp -> print (evaluateExpression rg exp)

