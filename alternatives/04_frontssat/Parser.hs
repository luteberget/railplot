module Parser where

import Text.Megaparsec
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Char
import Text.Megaparsec.Expr
import qualified Text.Megaparsec.Char.Lexer as L
import Data.List (sortOn)
import Control.Monad (void)
import Data.Void
import qualified Data.Map as Map
import Data.Map (Map)


import Input

type Parser = Parsec Void String

parseTopoFile :: String -> IO (Either String SolverInput)
parseTopoFile fn = do
  contents <- readFile fn
  return (parseTopo fn contents)

parseTopo :: String -> String -> Either String SolverInput
parseTopo name contents = case MP.parse (sc >> some stmt <* eof) name contents of
  (Left err) -> Left $ parseErrorPretty err
  (Right topo) -> Right $ convert topo

convert :: [Stmt] -> SolverInput
convert xs = SolverInput [n | (_,n,_) <- nodesSorted] edges []
  where
    nodesSorted = sortOn (\(_,_,p) -> p) [x | (NodeStmt x) <- xs]
    nodeNumbers = Map.fromList [(name,i) | (i,(name, _, _)) <- zip [0..] nodesSorted]
    edges = [((nodeNumbers Map.! n1, p1),(nodeNumbers Map.! n2, p2)) | (EdgeStmt ((n1,p1),(n2,p2))) <- xs]

sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "//"
    blockCmnt = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

--  number :: Parser Double
--  number = lexeme L.float
number :: Parser Double
number =  (try (lexeme L.float)) <|> (do x <- lexeme L.decimal ; return (fromIntegral x))


identifier :: Parser String
identifier = lexeme ((:) <$> letterChar <*> many bodyChar)
  where bodyChar = alphaNumChar <|> (char '-') <|> (char '_')

data Stmt = NodeStmt (String,Node,Double) | EdgeStmt ((String,Port),(String,Port))

stmt :: Parser Stmt
stmt = nodeStmt <|> edgeStmt

nodeStmt :: Parser Stmt
nodeStmt = do
  symbol "node"
  n <- identifier
  s <- shape 
  p <- number
  return (NodeStmt (n,s,p))

edgeStmt :: Parser Stmt
edgeStmt = do
  symbol "edge"
  n1 <- identifier
  symbol "."
  p1 <- port
  n2 <- identifier
  symbol "."
  p2 <- port
  return (EdgeStmt ((n1,p1),(n2,p2)))

port :: Parser Port
port = (symbol "out" >> return PBegin)
   <|> (symbol "in" >> return PEnd)
   <|> (symbol "trunk" >> return PTrunk)
   <|> (symbol "left" >> return PLeft)
   <|> (symbol "right" >> return PRight)

shape :: Parser Node
shape = (symbol "begin" >> return BeginNode)
    <|> (symbol "end" >> return EndNode)
    <|> (symbol "outleftsw" >> return (SwitchNode SLeft Up))
    <|> (symbol "outrightsw" >> return (SwitchNode SRight Up))
    <|> (symbol "inleftsw" >> return (SwitchNode SLeft Down))
    <|> (symbol "inrightsw" >> return (SwitchNode SRight Down))
