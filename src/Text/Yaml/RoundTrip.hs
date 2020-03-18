{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}

module Text.Yaml.RoundTrip (
    Document(..),
    Node(..),
    parseStream
    ) where

import Text.Yaml.Reference (Code(..),Token(..),yaml)
import Data.Map(Map)
import Data.Map as Map

import Text.Parsec.Prim
import Text.Parsec.Combinator
import Text.Parsec.Pos (newPos)

import Data.Functor.Identity

deriving instance Eq Token
deriving instance Ord Code
deriving instance Ord Token

data Document = Document Node
    deriving (Show, Eq)

data Node = Scalar Token | Mapping [(Node, Node)] | Sequence [Node]
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

notCode :: Stream s Identity Token => Code -> ParsecT s u Identity Token
notCode c = satisfy (\t -> c /= tCode t) <?> show [c]

bracket :: Stream s Identity Token => Code -> Code -> ParsecT s u Identity a -> ParsecT s u Identity a
bracket startCode endCode contentParser = do
    between (code startCode) (code endCode) contentParser

parseStream :: Stream s Identity Token => ParsecT s u Identity [Document]
parseStream = many parseDocument

parseDocument :: Stream s Identity Token => ParsecT s u Identity Document
parseDocument = do
    node <- bracket BeginDocument EndDocument parseNode
    return $ Document node

parseNode :: Stream s Identity Token => ParsecT s u Identity Node
parseNode = do
    node <- bracket BeginNode EndNode (choice [try parseScalar, try parseSequence, try parseMapping])
    return node

parseScalar :: Stream s Identity Token => ParsecT s u Identity Node
parseScalar = do
    text <- bracket BeginScalar EndScalar $ do
        text' <- code Text
        return text'
    return $ Scalar text

parseSequence :: Stream s Identity Token => ParsecT s u Identity Node
parseSequence = do
    nodes <- bracket BeginSequence EndSequence (many parseNode)
    return $ Sequence nodes

parseMapping :: Stream s Identity Token => ParsecT s u Identity Node
parseMapping = do
    pairs <- bracket BeginMapping EndMapping (many parsePair)
    return $ Mapping pairs

parsePair :: Stream s Identity Token => ParsecT s u Identity (Node, Node)
parsePair = do
    bracket BeginPair EndPair contentParser
    where
        contentParser = do
            key <- parseNode
            value <- parseNode
            return (key, value)
