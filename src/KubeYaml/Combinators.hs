module KubeYaml.Combinators
    ( textOf
    , elementsOf
    , kvpsOf
    , nodeAt
    , withDefaultText
    , findElementByName
    , lookupScalar
    )
    where

import Text.Yaml.RoundTrip (Document(..),Node(..))
import qualified Data.List (find)
import Data.Maybe (isJust)
import Text.Yaml.Reference (Token(..))

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
