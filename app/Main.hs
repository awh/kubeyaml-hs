module Main where

import qualified Data.ByteString.Lazy.Char8 as C
import Text.Yaml.Reference (Code(..),Token(..),yaml)

import KubeYaml.Commands.Image (updateImages ,ImageOptions(..))

import System.Exit (die)
import System.IO (stdin)

import Text.Yaml.RoundTrip
import Text.Parsec (runParser)

import Options.Applicative (Parser,long,info,(<**>),fullDesc,strOption,helper,subparser,command,progDesc,execParser)

data Options = Options
    { optCommand :: Command
    }

data Command = Image ImageOptions

imageCommandParser = Image <$> imageOptionsParser

imageOptionsParser :: Parser ImageOptions
imageOptionsParser = ImageOptions
    <$> strOption (long "namespace")
    <*> strOption (long "kind")
    <*> strOption (long "name")
    <*> strOption (long "container")
    <*> strOption (long "image")

optionsParser :: Parser Options
optionsParser = Options <$> subparser (command "image" (info imageCommandParser (progDesc "update an image ref")))

-- | Codes we need to parse to recover a nested datastructure from the token
-- stream.
parseableCodes = [ BeginDocument
                 , EndDocument
                 , BeginNode
                 , EndNode
                 , BeginScalar
                 , EndScalar
                 , BeginMapping
                 , EndMapping
                 , BeginSequence
                 , EndSequence
                 , BeginPair
                 , EndPair
                 , Text]

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
    where
        tokenFilter t = elem (tCode t) parseableCodes
