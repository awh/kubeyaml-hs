module Main where

import qualified Data.ByteString.Lazy.Char8 as C
import Text.Yaml.Reference (Code(..),Token(..),yaml)

import Lib

main :: IO ()
main = do
    bytes <- C.readFile "test.yaml"
    let tokens = yaml "-" bytes False
    putStr $ show tokens
    --putStr $ concat $ map tText tokens
