{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module Parsers
where
import Control.Applicative
--import Data.Text (Text)
import Data.Void
import Text.Megaparsec hiding (State, count)
import qualified Text.Megaparsec 
import qualified Text.Megaparsec.Char as MPC
import qualified Control.Monad.Combinators.Expr as Exp
--import qualified Data.Text as T
import qualified Text.Megaparsec.Char.Lexer as Lex
import Types
import Control.Monad (void)

type Error = String
type Input = String

type Parser a = Parsec Error Input a


sc :: Parser ()
sc = Lex.space
  MPC.space1                       
  (Lex.skipLineComment "//")       
  (Lex.skipBlockComment "/*" "*/") 

lexeme :: Parser a -> Parser a
lexeme = Lex.lexeme sc

symbol = Lex.symbol sc

parseDice :: Parser Expression
parseDice = do
    diceCount <- Lex.decimal
    void $ MPC.string "d"
    diceSize <- lexeme Lex.decimal
    return $ ExpDice Dice{count=diceCount, maxValue=diceSize}


parseConstant = do
    value <- lexeme Lex.decimal
    return $ ExpConst value


parseTerm = try parseDice
    <|> try parseConstant


parseExpression = Exp.makeExprParser parseTerm table
    where
        table = [ [binary "*" ExpMul, binary "/" ExpDiv]
                , [binary "+" ExpAdd, binary "-" ExpSub]
                , [binary "^" ExpMax, binary "v" ExpMin]
                ]
        binary  name f = Exp.InfixL  (f <$ symbol name)
        prefix  name f = Exp.Prefix  (f <$ symbol name)
        postfix name f = Exp.Postfix (f <$ symbol name)

