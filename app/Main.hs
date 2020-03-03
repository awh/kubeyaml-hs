module Main where

import qualified Data.ByteString.Lazy.Char8 as C
import Text.Yaml.Reference (Code(..),Token(..),yaml)
import Data.Map(Map)
import Data.Map as Map

import Lib

data YamlNode = ScalarNode Token | MappingNode (Map YamlNode YamlNode) | SequenceNode [YamlNode]

-- Extract all documents from stream
documents :: [Token] -> [YamlNode]
documents = undefined

main :: IO ()
main = do
    bytes <- C.readFile "test.yaml"
    let tokens = yaml "-" bytes False
    putStr $ show tokens
    --putStr $ concat $ map tText tokens
