{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}

module Main where

import qualified Data.ByteString.Lazy.Char8 as C
import Text.Yaml.Reference (Code(..),Token(..),yaml)
import Data.Map(Map)
import Data.Map as Map

import Text.Parsec.Prim
import Text.Parsec.Combinator
import Text.Parsec.Pos (newPos)

import Data.Functor.Identity

import Lib

deriving instance Eq Token
deriving instance Ord Code
deriving instance Ord Token

data Document = Document Node
    deriving (Show, Eq)

data Node = Scalar Token | Mapping (Map Node Node) | Sequence [Node]
    deriving (Show, Eq, Ord)

satisfy :: Stream s Identity Token => (Token -> Bool) -> ParsecT s u Identity Token
satisfy f = token showTok posFromTok testTok
    where
        showTok t = show t
        posFromTok t = newPos "" (tLine t) (tLineChar t)
        testTok t = if f t then Just t else Nothing

oneOf :: Stream s Identity Token => [Code] -> ParsecT s u Identity Token
oneOf cs = satisfy (\t -> elem (tCode t) cs)

noneOf :: Stream s Identity Token => [Code] -> ParsecT s u Identity Token
noneOf cs = satisfy (\t -> not (elem (tCode t) cs))

code :: Stream s Identity Token => Code -> ParsecT s u Identity Token
code c = satisfy (\t -> c == tCode t) <?> show [c]

parseStream :: Stream s Identity Token => ParsecT s u Identity [Document]
parseStream = many parseDocument

parseDocument :: Stream s Identity Token => ParsecT s u Identity Document
parseDocument = do
    node <- between (code BeginDocument) (code EndDocument) internal
    return $ Document node
    where
        internal = do
            optional $code DirectivesEnd
            parseNode

parseNode :: Stream s Identity Token => ParsecT s u Identity Node
parseNode = do
    node <- between (code BeginNode) (code EndNode) internal
    skipMany $ code Break
    return node
    where
        internal = do
            skipMany $ code Break
            choice [parseScalar, parseSequence, parseMapping]

parseScalar :: Stream s Identity Token => ParsecT s u Identity Node
parseScalar = do
    text <- between (code BeginScalar) (code EndScalar) (code Text)
    return $ Scalar text

parseSequence :: Stream s Identity Token => ParsecT s u Identity Node
parseSequence = do
    nodes <- between (code BeginSequence) (code EndSequence) (many1 node)
    return $ Sequence nodes
    where
        node = do
            optional $ code Indent
            code Indicator
            optional $ code White
            node <- parseNode
            skipMany $ code Break
            return node

parseMapping :: Stream s Identity Token => ParsecT s u Identity Node
parseMapping = do
    pairs <- between (code BeginMapping) (code EndMapping) (many1 pair)
    return $ Mapping $ Map.fromList pairs
    where
        pair = do
            optional $ code Indent
            code BeginPair
            key <- parseNode
            code Indicator
            optional $ code White
            value <- parseNode
            code EndPair
            return (key, value)

main :: IO ()
main = do
    bytes <- C.readFile "test.yaml"
    let tokens = yaml "-" bytes False
    putStr $ show tokens
    case (runParser parseStream () "" tokens) of
      Left e -> putStrLn $ show e
      Right s -> putStrLn $ show s
