module KubeYaml.Commands.Set
    ( SetOptions(..)
    , setCommand
    , SetResult(..)
    )
    where

import Data.Char (toLower)
import Data.Maybe (catMaybes,fromMaybe,isJust)

import qualified Data.List (find)

import Text.Yaml.Reference (Code(..),Token(..),yaml)
import Text.Yaml.RoundTrip (Document(..),Node(..))

import KubeYaml.Combinators (textOf,elementsOf,kvpsOf,nodeAt,findElementByName,scalarToken)

type Namespace = String
type Kind = String
type MetadataName = String
type Path = [String]
type Value = String

data SetOptions = SetOptions
    { namespace  :: Namespace
    , kind       :: Kind
    , name       :: MetadataName
    , pathValues :: [(Path,Value)]
    }

data SetResult = SetManifestNotFound | SetSuccess String [Path]

matchingManifest :: Namespace -> Kind -> MetadataName -> Document -> Maybe Document
matchingManifest ns k mn d@(Document root) = do
        let ns' = fromMaybe "default" $ nodeAt ["metadata", "namespace"] root >>= textOf
        k'  <- nodeAt ["kind"] root >>= textOf
        mn' <- nodeAt ["metadata", "name"] root >>= textOf

        if ns == ns' && (map toLower k) == (map toLower k') && mn == mn'
        then Just d 
        else Nothing

findManifest :: Namespace -> Kind -> MetadataName -> [Document] -> Maybe Document
findManifest ns k mn ds = Data.List.find (isJust . matchingManifest ns k mn) ds

-- | Search a document for specified paths, returning a list of matching tokens
-- and their desired replacement values and a list of any unfound paths.
findTokens :: [(Path,Value)] -> Document -> ([(Token,Value)],[Path])
findTokens [] _ = ([],[])
findTokens ((p,v):pvs) d =
    let (replacements,unfounds) = findTokens pvs d in
    case (maybeToken p d) of
        Just token -> ((token,v):replacements, unfounds)
        Nothing -> (replacements, p:unfounds)
    where
        maybeToken :: Path -> Document -> Maybe Token
        maybeToken p (Document root) = nodeAt p root >>= scalarToken

-- | Concatenate a token stream back into a string, replacing any tokens in a
-- supplied set with a specific string.
replaceTokens :: [(Token,Value)] -- ^ Scalar tokens to be replaced
              -> [Token]         -- ^ Original token stream
              -> String
replaceTokens rtvs ts = concat $ fmap toString ts
    where
        toString t =
            case (lookup t rtvs) of
                Just rv -> rv
                Nothing -> tText t

-- | Set values in a document stream according to supplied options.
setCommand :: SetOptions -> [Token] -> [Document] -> SetResult
setCommand so ts ds =
    case maybeManifest of
        Just m -> let (replacements, unfounds) = findTokens (pathValues so) m in SetSuccess (replaceTokens replacements ts) unfounds
        Nothing -> SetManifestNotFound
    where
        maybeManifest = findManifest (namespace so) (kind so) (name so) ds
