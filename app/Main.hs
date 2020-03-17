
module Main where

import qualified Data.ByteString.Lazy.Char8 as C
import Text.Yaml.Reference (Code(..),Token(..),yaml)

import Data.Maybe (isJust, catMaybes)
import qualified Data.List (find)

import Text.Yaml.RoundTrip
import Text.Parsec (runParser)

import Debug.Trace

type Namespace = String
type Kind = String
type MetadataName = String
type ContainerName = String

-- If the node is single line scalar, return its text
textOf :: Maybe Node -> Maybe String
textOf (Just (Scalar token)) = Just $ tText token
textOf _ = Nothing

-- If the node is a sequence, return its elements
elementsOf :: Maybe Node -> Maybe [Node]
elementsOf (Just (Sequence nodes)) = Just nodes
elementsOf _ = Nothing

-- If the node is a mapping, return its kvps
kvpsOf :: Maybe Node -> Maybe [(Node, Node)]
kvpsOf (Just (Mapping kvps)) = Just kvps
kvpsOf _ = Nothing

findElementByName :: String -> [Node] -> Maybe Node
findElementByName name ns = Data.List.find pred ns
    where
        pred :: Node -> Bool
        pred (Mapping kvps) = isJust $ do
            name' <- textOf $ lookupScalar "name" kvps
            if name == name' then Just name else Nothing
        pred _ = False

lookupScalar :: String -> [(Node, Node)] -> Maybe Node
lookupScalar s ns = fmap snd $ Data.List.find pred ns 
    where
        pred :: (Node, Node) -> Bool
        pred (Scalar t, _) = s == tText t
        pred _ = False

nodeAt :: [String] -> Node -> Maybe Node
nodeAt [] n = Just n
nodeAt (n:ns) (Mapping kvps) =
    case (lookupScalar n kvps) of
      Just v -> nodeAt ns v
      _ -> Nothing
nodeAt _ _ = Nothing

withDefaultText :: String -> Maybe String -> Maybe String
withDefaultText _ (Just s) = Just s
withDefaultText s Nothing = Just s

findContainer :: Namespace -> Kind -> MetadataName -> ContainerName -> Document -> Maybe Token
findContainer ns k mn cn (Document root) = do
        ns' <- withDefaultText "default" $ textOf $ nodeAt ["metadata", "namespace"] root
        k'  <- textOf $ nodeAt ["kind"] root
        mn' <- textOf $ nodeAt ["metadata", "name"] root
        cs  <- elementsOf $ nodeAt ["spec", "template", "spec", "containers"] root
        c   <- kvpsOf $ findElementByName cn cs
        i   <- lookupScalar "image" c
        if ns == ns' && k == k' && mn == mn'
        then case i of
            (Scalar t) -> Just t
            _ -> Nothing
        else Nothing

findContainers :: Namespace -> Kind -> MetadataName -> ContainerName -> [Document] -> [Token]
findContainers ns k mn cn ds = catMaybes $ map (findContainer ns k mn cn) ds

printMatching :: [Token] -> [Document] -> IO ()
printMatching ts ds = putStr $ replaceTokens rts "helloworld" ts
    where
        rts = findContainers "default" "Deployment" "nginx-deployment" "nginx" ds

replaceTokens :: [Token] -- ^ Scalar tokens to be replaced
              -> String  -- ^ Replacement value
              -> [Token] -- ^ Original token stream
              -> String
replaceTokens rts rv ts = concat $ fmap toString ts
    where
        toString t = if (elem t rts) then rv else (tText t)

main :: IO ()
main = do
    bytes <- C.readFile "test.yaml"
    let ts = yaml "-" bytes False
    case (runParser parseStream () "" ts) of
      Left e -> putStrLn $ show e
      Right ds -> printMatching ts ds
