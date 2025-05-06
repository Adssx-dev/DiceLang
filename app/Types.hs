module Types
where

data Dice = Dice
    { maxValue :: Int
    , count :: Int
    } deriving Show

data Expression =
    ExpDice Dice |
    ExpConst Int |
    ExpAdd Expression Expression |
    ExpSub Expression Expression |
    ExpMul Expression Expression |
    ExpDiv Expression Expression |
    ExpMax Expression Expression |
    ExpMin Expression Expression |
    ExpParenthesis Expression
    deriving Show

printDie :: Dice -> String
printDie die = show (count die) ++ 
    "d" ++ 
    show (maxValue die)

printExpr :: Expression -> String
printExpr (ExpDice dice) = printDie dice
printExpr (ExpConst value) = show value
printExpr (ExpAdd expr1 expr2) = printExpr expr1 ++ "+" ++ printExpr expr2
printExpr (ExpSub expr1 expr2) = printExpr expr1 ++ "-" ++ printExpr expr2
printExpr (ExpMul expr1 expr2) = printExpr expr1 ++ "*" ++ printExpr expr2
printExpr (ExpDiv expr1 expr2) = printExpr expr1 ++ "/" ++ printExpr expr2
printExpr (ExpMax expr1 expr2) = printExpr expr1 ++ "^" ++ printExpr expr2
printExpr (ExpMin expr1 expr2) = printExpr expr1 ++ "v" ++ printExpr expr2
printExpr (ExpParenthesis expr ) = "(" ++ printExpr expr ++ ")"



