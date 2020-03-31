module Main where

import Data.List (intercalate)
import Data.List.Split (splitOn)

import qualified Data.ByteString.Lazy.Char8 as C
import Text.Yaml.Reference (Code(..),Token(..),yaml)

import KubeYaml.Commands.Image (updateImages ,ImageOptions(..))
import KubeYaml.Commands.Set (SetOptions(..),setCommand,SetResult(..))

import System.Exit (die)
import System.IO (stdin,stdout)

import Text.Yaml.RoundTrip
import Text.Parsec (runParser)

import Options.Applicative (Parser,long,info,(<**>),fullDesc,strOption,helper,subparser,command,progDesc,execParser,maybeReader,ReadM,some,argument,metavar)

data Options = Options
    { optCommand :: Command
    }

data Command = Image ImageOptions | Set SetOptions

imageCommandParser = Image <$> imageOptionsParser

imageOptionsParser :: Parser ImageOptions
imageOptionsParser = ImageOptions
    <$> strOption (long "namespace")
    <*> strOption (long "kind")
    <*> strOption (long "name")
    <*> strOption (long "container")
    <*> strOption (long "image")

setCommandParser = Set <$> setOptionsParser

setOptionsParser :: Parser SetOptions
setOptionsParser = SetOptions
    <$> strOption (long "namespace")
    <*> strOption (long "kind")
    <*> strOption (long "name")
    <*> some (argument pathValuePair (metavar "PATHS"))

pathValuePair :: ReadM ([String], String)
pathValuePair = maybeReader f
    where
        f s =
            case (splitOn "=" s) of
                (k:v:[]) -> Just (splitOn "." k, v)
                _ -> Nothing

optionsParser :: Parser Options
optionsParser = Options <$> subparser
    (  command "image" (info imageCommandParser (progDesc "update an image ref"))
    <> command "set" (info setCommandParser (progDesc "update values by their dot notation paths"))
    )

-- | Codes we need to parse to recover a nested datastructure from the token
-- stream.
parseableCodes = [ BeginDocument
                 , EndDocument
                 , BeginScalar
                 , EndScalar
                 , BeginMapping
                 , EndMapping
                 , BeginSequence
                 , EndSequence
                 , BeginPair
                 , EndPair
                 , Text
                 , Indent
                 , Break
                 ]

main :: IO ()
main = do
    options <- execParser $ info (optionsParser <**> helper) fullDesc
    case (optCommand options) of
        Image imageOptions -> do
            bytes <- C.hGetContents stdin
            let tokens = yaml "-" bytes False
                tokens' = filter tokenFilter tokens
            case (runParser parseStream () "-" tokens') of
                Left e -> die $ (show tokens ++ show e)
                Right docs -> putStr $ updateImages imageOptions tokens docs
        Set setOptions -> do
            bytes <- C.hGetContents stdin
            let tokens = yaml "-" bytes False
                tokens' = filter tokenFilter tokens
            case (runParser parseStream () "-" tokens') of
                Left e -> die $ (show tokens ++ show e)
                Right docs ->
                    case (setCommand setOptions tokens docs) of
                        SetManifestNotFound -> do
                            C.hPut stdout bytes
                            die "manifest not found"
                        SetSuccess yaml unfound -> do
                            putStr yaml
                            if (length unfound) > 0
                            then die (intercalate "\n" $ "unable to resolve path(s):" : (reconstitutePaths unfound))
                            else return ()
    where
        tokenFilter t = elem (tCode t) parseableCodes

        reconstitutePaths :: [[String]] -> [String]
        reconstitutePaths paths = map (intercalate ".") paths
