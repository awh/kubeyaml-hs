
module Main where

import qualified Data.ByteString.Lazy.Char8 as C
import Text.Yaml.Reference (Code(..),Token(..),yaml)

import qualified Data.List (find)

import Text.Yaml.RoundTrip
import Text.Parsec (runParser)

type Namespace = String
type Kind = String
type MetadataName = String
type ContainerName = String

lookupScalar :: String -> [(Node, Node)] -> Maybe Node
lookupScalar s ns = fmap snd $ Data.List.find pred ns 
    where
        pred :: (Node, Node) -> Bool
        pred (Scalar t, _) = s == tText t
        pred _ = False

findNode :: [String] -> Node -> Maybe Node
findNode (n:ns) (Mapping kvps) =
    case (lookupScalar n kvps) of
      Just v -> findNode ns v
      _ -> Nothing
findNode _ _ = Nothing

find :: Namespace -> Kind -> MetadataName -> ContainerName -> Document -> Maybe Node

find ns k mn cn (Document root) =
    where

        metadataName = findNode ["metadata", "name"] root
        containers = findNode ["spec", "template", "spec", "containers"]

find _ _ _ _ _ = Nothing


main :: IO ()
main = do
    bytes <- C.readFile "test.yaml"
    let tokens = yaml "-" bytes False
    putStr $ show tokens
    case (runParser parseStream () "" tokens) of
      Left e -> putStrLn $ show e
      Right s -> putStrLn $ show s
