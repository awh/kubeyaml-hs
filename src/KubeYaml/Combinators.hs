module KubeYaml.Combinators
    ( scalarToken
    , textOf
    , elementsOf
    , kvpsOf
    , nodeAt
    , findElementByName
    , lookupScalar
    )
    where

import Text.Yaml.RoundTrip (Document(..),Node(..),ScalarFields(..),MapEntry(..))
import qualified Data.List (find)
import Data.Maybe (isJust)
import Text.Yaml.Reference (Token(..))

-- If the node is a single line scalar, return its token
scalarToken :: Node -> Maybe Token
scalarToken (Scalar fields) = Just $ scaToken fields
scalarToken _ = Nothing

-- If the node is single line scalar, return its text
textOf :: Node -> Maybe String
textOf (Scalar fields) = Just $ tText $ scaToken fields
textOf _ = Nothing

-- If the node is a sequence, return its elements
elementsOf :: Node -> Maybe [Node]
elementsOf (Sequence nodes) = Just nodes
elementsOf _ = Nothing

-- If the node is a mapping, return its kvps
kvpsOf :: Node -> Maybe [MapEntry]
kvpsOf (Mapping kvps) = Just kvps
kvpsOf _ = Nothing

findElementByName :: String -> [Node] -> Maybe Node
findElementByName name ns = Data.List.find pred ns
    where
        pred :: Node -> Bool
        pred (Mapping kvps) = isJust $ do
            name' <- lookupScalar "name" kvps >>= textOf
            if name == name' then Just name else Nothing
        pred _ = False

lookupScalar :: String -> [MapEntry] -> Maybe Node
lookupScalar s ns = fmap mapEntryValue $ Data.List.find pred ns
    where
        pred :: MapEntry -> Bool
        pred (MapEntry _ (Scalar fields) _) = s == (tText $ scaToken fields)
        pred _ = False

nodeAt :: [String] -> Node -> Maybe Node
nodeAt [] n = Just n
nodeAt (n:ns) (Mapping kvps) =
    case (lookupScalar n kvps) of
      Just v -> nodeAt ns v
      _ -> Nothing
nodeAt _ _ = Nothing
