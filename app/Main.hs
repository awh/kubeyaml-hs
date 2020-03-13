
module Main where

import qualified Data.ByteString.Lazy.Char8 as C
import Text.Yaml.Reference (Code(..),Token(..),yaml)

import Text.Yaml.RoundTrip
import Text.Parsec (runParser)

type Namespace = String
type Kind = String
type MetadataName = String
type ContainerName = String

findNode :: [String] -> Node -> Maybe Node
findNode ns node =

find :: Namespace -> Kind -> MetadataName -> ContainerName -> Document -> Maybe Scalar
find ns k mn cn d =
    case d of
        Mapping md -> undefined
        _          -> Nothing 

main :: IO ()
main = do
    bytes <- C.readFile "test.yaml"
    let tokens = yaml "-" bytes False
    putStr $ show tokens
    case (runParser parseStream () "" tokens) of
      Left e -> putStrLn $ show e
      Right s -> putStrLn $ show s
