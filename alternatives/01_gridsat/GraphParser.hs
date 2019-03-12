module GraphParser where

import Data.Void
import Text.Megaparsec
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Char
import Text.Megaparsec.Expr
import qualified Text.Megaparsec.Char.Lexer as L

import Data.Maybe (catMaybes)
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad (join)

data Side = SwLeft | SwRight | SwUnknown
  deriving (Show, Eq, Ord)
data Dir = Incoming | Outgoing
  deriving (Show, Eq, Ord)
data NodeData = Start String | End String | Sw Side Dir String (String,String)
  deriving (Show, Eq, Ord)

data Statement 
  = NodeStmt String NodeData
  | EdgeStmt (String,String) Int [((String,String),Double)]
  deriving (Show, Eq, Ord)


-- convert :: [EdgeStatement] -> ([Edge],[String])
-- convert stmts = (edges,boundaries)
--   where 
--     dnodes = Map.fromList $ join [ [(n1, name), (n2, name)] 
--                                  | DNodeStatement n1 n2 <- stmts 
--                                  , let name = n1 ++ "-" ++ n2 ]
--     boundaries = [ b | (EdgeStatement b []) <- stmts ]
--     edges = [ Edge (dnodes Map.! a) (dnodes Map.! b) l 
--             | (EdgeStatement a x) <- stmts, (b,l) <- x ]

type Output = [Statement]
parseFile :: String -> IO (Either String Output)
parseFile fn = do
  contents <- readFile fn
  return (parseEdges fn contents)

parseEdges :: String -> String -> Either String Output
parseEdges name contents = case MP.parse (sc >> some statement <* eof) name contents of
  (Left err) -> Left $ parseErrorPretty err
  (Right statements) -> Right $ statements

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "//"
    blockCmnt = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

number :: Parser Double
number =  (try (lexeme L.float)) <|> (do x <- lexeme L.decimal ; return (fromIntegral x))

integer :: Parser Int
integer = lexeme L.decimal

identifier :: Parser String
identifier = lexeme ((:) <$> letterChar <*> many bodyChar)
  where bodyChar = alphaNumChar <|> (char '_')

statement :: Parser Statement
statement = node <|> edge

node :: Parser Statement
node = do
  symbol "node"
  name <- identifier
  dat <- nodeData
  return (NodeStmt name dat)

nodeData :: Parser NodeData
nodeData = startData <|> endData <|> swData

startData :: Parser NodeData
startData = do
  symbol "start"
  name <- identifier
  return (Start name)
endData :: Parser NodeData
endData = do
  symbol "end"
  name <- identifier
  return (End name)
swData :: Parser NodeData
swData = do
  symbol "switch"
  side <- ((symbol "left" >> return SwLeft) <|> (symbol "right" >> return SwRight))
          <|> (symbol "unknown" >> return SwUnknown)
  dir  <- ((symbol "incoming" >> return Incoming) <|> (symbol "outgoing" >> return Outgoing))
  n1 <- identifier
  n2 <- identifier
  n3 <- identifier
  return (Sw side dir n1 (n2,n3))

edge :: Parser Statement
edge = do
  symbol "edge"
  start <- identifier
  end <- identifier
  symbol "level"
  level <- integer
  other <- list $ do
    n1 <- identifier; n2 <- identifier; dist <- number;
    return ((n1,n2),dist)
  return (EdgeStmt (start,end) level other)

-- boundary :: Parser EdgeStatement
-- boundary = do
--   symbol "boundary"
--   name <- identifier
--   return (EdgeStatement name [])
-- 
-- linear :: Parser EdgeStatement
-- linear = do
--   symbol "linear"
--   n1 <- identifier
--   symbol "-"
--   n2 <- identifier
--   l <- number
--   return (EdgeStatement n1 [(n2,l)])
-- 
list :: Parser a -> Parser [a]
list =  (between (symbol ("(")) (symbol (")"))) . (\x -> sepBy x (symbol ","))
-- 
-- switch :: Parser EdgeStatement
-- switch = do
--   symbol "switch"
--   n1 <- identifier
--   symbol "-"
--   other <- list $ do
--     name <- identifier
--     l <- number
--     return (name,l)
--   return (EdgeStatement n1 other)
-- 
-- node :: Parser EdgeStatement
-- node = do
--   let node = do
--       n <- identifier
--       optional (list (many ((identifier >> (return "")) <|> (number >> (return "")))))
--       return n
--   symbol "node"; n1 <- node; symbol "-"; n2 <- node;
--   return (DNodeStatement n1 n2)
-- 
