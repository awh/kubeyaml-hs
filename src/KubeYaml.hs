module KubeYaml
    ( updateImages
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

-- | Search the document for a matching container, and return the text token
-- from the input stream which holds its image reference.
findImageToken :: Namespace -> Kind -> MetadataName -> ContainerName -> Document -> Maybe Token
findImageToken ns k mn cn (Document root) = do
        ns' <- withDefaultText "default" $ textOf $ nodeAt ["metadata", "namespace"] root
        k'  <- textOf $ nodeAt ["kind"] root
        mn' <- textOf $ nodeAt ["metadata", "name"] root
        cs  <- elementsOf $ nodeAt ["spec", "template", "spec", "containers"] root
        c   <- kvpsOf $ findElementByName cn cs
        i   <- lookupScalar "image" c
        if ns == ns' && k == k' && mn == mn'
        then case i of
            (Scalar token) -> Just token
            _ -> Nothing
        else Nothing

-- | Search a list of documents for matching containers, returning the text
-- tokens holding their image references.
findImageTokens :: Namespace -> Kind -> MetadataName -> ContainerName -> [Document] -> [Token]
findImageTokens ns k mn cn ds = catMaybes $ map (findImageToken ns k mn cn) ds

-- | Concatenate a token stream back into a string, replacing any tokens in a
-- supplied set with a specific string.
replaceTokens :: [Token] -- ^ Scalar tokens to be replaced
              -> String  -- ^ Replacement value
              -> [Token] -- ^ Original token stream
              -> String
replaceTokens rts rv ts = concat $ fmap toString ts
    where
        toString t = if (elem t rts) then rv else (tText t)

-- | Update the image references in a document stream according to supplied
-- options.
updateImages :: ImageOptions -> [Token] -> [Document] -> String
updateImages io ts ds =
    replaceTokens rts (image io) ts
    where
        rts = findImageTokens (namespace io) (kind io) (name io) (container io) ds
