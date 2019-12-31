{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module IO.Taskell where

import ClassyPrelude

import Control.Monad.Reader (runReader)
import Data.FileEmbed       (embedFile)
import System.Directory     (doesFileExist, getCurrentDirectory)

import Data.Time.Zones (TZ)

import Config             (usage, version)
import Data.Taskell.Lists (Lists, analyse, initial)

import           IO.Config         (Config, general, github, markdown, trello)
import           IO.Config.General (filename)
import qualified IO.Config.GitHub  as GitHub (token)
import qualified IO.Config.Trello  as Trello (token)

import IO.Markdown (MarkdownInfo (MarkdownInfo), parse, serialize)

import qualified IO.HTTP.GitHub as GitHub (GitHubIdentifier, getLists)
import qualified IO.HTTP.Trello as Trello (TrelloBoardID, getLists)

import UI.CLI (PromptYN (PromptYes), promptYN)

data IOInfo = IOInfo
    { ioTZ     :: TZ
    , ioConfig :: Config
    }

type ReaderConfig a = ReaderT IOInfo IO a

data Next
    = Output Text
    | Error Text
    | Load FilePath
           Lists
    | Exit

parseArgs :: [Text] -> ReaderConfig Next
parseArgs ["-v"]                   = pure $ Output version
parseArgs ["-h"]                   = pure $ Output usage
parseArgs ["-t", boardID, file]    = loadTrello boardID file
parseArgs ["-g", identifier, file] = loadGitHub identifier file
parseArgs ["-i", file]             = fileInfo file
parseArgs [file]                   = loadFile file
parseArgs []                       = (pack . filename . general <$> asks ioConfig) >>= loadFile
parseArgs _                        = pure $ Error (unlines ["Invalid options", "", usage])

load :: ReaderConfig Next
load = getArgs >>= parseArgs

colonic :: FilePath -> Text -> Text
colonic path = ((pack path <> ": ") <>)

loadFile :: Text -> ReaderConfig Next
loadFile filepath = do
    mPath <- exists filepath
    case mPath of
        Nothing   -> pure Exit
        Just path -> either (Error . colonic path) (Load path) <$> readData path

loadRemote :: (token -> FilePath -> ReaderConfig Next) -> token -> Text -> ReaderConfig Next
loadRemote createFn identifier filepath = do
    let path = unpack filepath
    exists' <- fileExists path
    if exists'
        then pure $ Error (filepath <> " already exists")
        else createFn identifier path

loadTrello :: Trello.TrelloBoardID -> Text -> ReaderConfig Next
loadTrello = loadRemote createTrello

loadGitHub :: GitHub.GitHubIdentifier -> Text -> ReaderConfig Next
loadGitHub = loadRemote createGitHub

fileInfo :: Text -> ReaderConfig Next
fileInfo filepath = do
    let path = unpack filepath
    exists' <- fileExists path
    if exists'
        then either (Error . colonic path) (Output . analyse filepath) <$> readData path
        else pure $ Error (filepath <> " does not exist")

createRemote ::
       (Config -> Maybe token)
    -> Text
    -> (token -> ReaderT token IO (Either Text Lists))
    -> token
    -> FilePath
    -> ReaderConfig Next
createRemote tokenFn missingToken getFn identifier path = do
    config <- asks ioConfig
    tz <- asks ioTZ
    case tokenFn config of
        Nothing -> pure $ Error missingToken
        Just token -> do
            lists <- lift $ runReaderT (getFn identifier) token
            case lists of
                Left txt -> pure $ Error txt
                Right ls ->
                    promptCreate path >>=
                    bool (pure Exit) (Load path ls <$ lift (writeData tz config ls path))

createTrello :: Trello.TrelloBoardID -> FilePath -> ReaderConfig Next
createTrello =
    createRemote
        (Trello.token . trello)
        (decodeUtf8 $(embedFile "templates/trello-token.txt"))
        Trello.getLists

createGitHub :: GitHub.GitHubIdentifier -> FilePath -> ReaderConfig Next
createGitHub =
    createRemote
        (GitHub.token . github)
        (decodeUtf8 $(embedFile "templates/github-token.txt"))
        GitHub.getLists

exists :: Text -> ReaderConfig (Maybe FilePath)
exists filepath = do
    let path = unpack filepath
    exists' <- fileExists path
    if exists'
        then pure $ Just path
        else promptCreate path >>= bool (pure Nothing) (Just path <$ createPath path)

fileExists :: FilePath -> ReaderConfig Bool
fileExists path = lift $ doesFileExist path

promptCreate :: FilePath -> ReaderConfig Bool
promptCreate path = do
    cwd <- lift $ pack <$> getCurrentDirectory
    lift $ promptYN PromptYes $ concat ["Create ", cwd, "/", pack path, "?"]

-- creates taskell file
createPath :: FilePath -> ReaderConfig ()
createPath path = do
    config <- asks ioConfig
    tz <- asks ioTZ
    lift (writeData tz config initial path)

-- writes Tasks to json file
writeData :: TZ -> Config -> Lists -> FilePath -> IO ()
writeData tz config tasks path = void (writeFile path output)
  where
    output = encodeUtf8 $ runReader (serialize tasks) (MarkdownInfo tz (markdown config))

-- reads json file
readData :: FilePath -> ReaderConfig (Either Text Lists)
readData path = parse <$> asks ioConfig <*> readFile path
