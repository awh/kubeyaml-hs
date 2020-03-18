module KubeYaml
    ( applyFilter
    , ImageOptions(..)
    )
    where

import Data.Maybe (catMaybes)

import Text.Yaml.Reference (Code(..),Token(..),yaml)
import Text.Yaml.RoundTrip (Document(..),Node(..))

import KubeYaml.Combinators (textOf,elementsOf,kvpsOf,nodeAt,withDefaultText,findElementByName,lookupScalar)

data ImageOptions = ImageOptions
    { namespace :: String
    , kind      :: String
    , name      :: String
    , container :: String
    , image     :: String
    }

type Namespace = String
type Kind = String
type MetadataName = String
type ContainerName = String

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

replaceTokens :: [Token] -- ^ Scalar tokens to be replaced
              -> String  -- ^ Replacement value
              -> [Token] -- ^ Original token stream
              -> String
replaceTokens rts rv ts = concat $ fmap toString ts
    where
        toString t = if (elem t rts) then rv else (tText t)

applyFilter :: ImageOptions -> [Token] -> [Document] -> String
applyFilter io ts ds = replaceTokens rts (image io) ts
    where
        rts = findContainers (namespace io) (kind io) (name io) (container io) ds
