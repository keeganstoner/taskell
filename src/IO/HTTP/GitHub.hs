{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module IO.HTTP.GitHub
    ( GitHubToken
    , GitHubIdentifier
    , getNextLink
    , getLists
    ) where

import ClassyPrelude

import Control.Lens  ((^.))
import Data.Either   (lefts, rights)
import Data.Sequence (mapWithIndex, (!?))
import Data.Text     (splitOn, strip)

import Data.Aeson
import UI.CLI     (prompt)

import Network.HTTP.Client       (requestHeaders)
import Network.HTTP.Simple       (getResponseBody, getResponseHeader, getResponseStatusCode, httpBS,
                                  parseRequest)
import Network.HTTP.Types.Header (HeaderName)

import IO.HTTP.Aeson          (parseError)
import IO.HTTP.GitHub.Card    (Card)
import IO.HTTP.GitHub.Column  (Column, cardsURL, columnToList)
import IO.HTTP.GitHub.Project (Project, columnsURL, name)

import Data.Taskell.List  (List)
import Data.Taskell.Lists (Lists)

type GitHubToken = Text

type GitHubIdentifier = Text

type ReaderGitHubToken a = ReaderT GitHubToken IO a

concatEithers :: [Either String [a]] -> Either String [a]
concatEithers vals =
    if null errors
        then Right $ concat (rights vals)
        else Left $ unlines errors
  where
    errors = lefts vals

root :: Text
root = "https://api.github.com/"

headers :: ReaderGitHubToken [(HeaderName, ByteString)]
headers = do
    token <- ask
    return
        [ ("User-Agent", "smallhadroncollider/taskell")
        , ("Accept", "application/vnd.github.inertia-preview+json")
        , ("Authorization", encodeUtf8 ("token " ++ token))
        ]

getNextLink :: [ByteString] -> Maybe Text
getNextLink bs = do
    lnks <- splitOn "," . decodeUtf8 <$> headMay bs
    let rel = "rel=\"next\""
    next <- find (isSuffixOf rel) lnks
    stripPrefix "<" =<< stripSuffix (">; " ++ rel) (strip next)

fetch' :: [ByteString] -> Text -> ReaderGitHubToken (Int, [ByteString])
fetch' bs url = do
    initialRequest <- lift $ parseRequest (unpack url)
    rHeaders <- headers
    let request = initialRequest {requestHeaders = rHeaders}
    response <- lift $ httpBS request
    let responses = bs ++ [getResponseBody response]
    case getNextLink (getResponseHeader "Link" response) of
        Nothing  -> return (getResponseStatusCode response, responses)
        Just lnk -> fetch' responses lnk

fetch :: Text -> ReaderGitHubToken (Int, [ByteString])
fetch = fetch' []

getCards :: Text -> ReaderGitHubToken (Either Text [Card])
getCards url = do
    (status, body) <- fetch url
    return $
        case status of
            200 ->
                case concatEithers (eitherDecodeStrict <$> body) of
                    Right cards -> Right cards
                    Left err    -> Left (parseError err)
            429 -> Left "Too many cards"
            _ -> Left $ tshow status ++ " error while fetching " ++ url

addCard :: Column -> ReaderGitHubToken (Either Text List)
addCard column = do
    cards <- getCards $ column ^. cardsURL
    return $ columnToList column <$> cards

addCards :: [Column] -> ReaderGitHubToken (Either Text Lists)
addCards columns = do
    cols <- sequence (addCard <$> columns)
    return $ fromList <$> sequence cols

getColumns :: Text -> ReaderGitHubToken (Either Text Lists)
getColumns url = do
    putStrLn "Fetching project from GitHub..."
    (status, body) <- fetch url
    case status of
        200 ->
            case concatEithers (eitherDecodeStrict <$> body) of
                Right columns -> addCards columns
                Left err      -> return $ Left (parseError err)
        404 -> return . Left $ "Could not find GitHub project ."
        401 -> return . Left $ "You do not have permission to view GitHub project " ++ url
        _ -> return . Left $ tshow status ++ " error. Cannot fetch columns from GitHub."

printProjects :: Seq Project -> Text
printProjects projects = unlines $ toList display
  where
    names = (^. name) <$> projects
    line i nm = concat ["[", tshow (i + 1), "] ", nm]
    display = line `mapWithIndex` names

chooseProject :: [Project] -> ReaderGitHubToken (Either Text Lists)
chooseProject projects = do
    let projects' = fromList projects
    putStrLn $ printProjects projects'
    chosen <- lift $ prompt "Import project"
    let project = (projects' !?) =<< (-) 1 <$> readMay chosen
    case project of
        Nothing   -> return $ Left "Invalid project selected"
        Just proj -> getColumns (proj ^. columnsURL)

getLists :: GitHubIdentifier -> ReaderGitHubToken (Either Text Lists)
getLists identifier = do
    putStrLn "Fetching project list from GitHub...\n"
    let url = concat [root, identifier, "/projects"]
    (status, body) <- fetch url
    case status of
        200 ->
            case concatEithers (eitherDecodeStrict <$> body) of
                Right projects ->
                    if null projects
                        then return . Left $ concat ["\nNo projects found for ", identifier, "\n"]
                        else chooseProject projects
                Left err -> return $ Left (parseError err)
        404 ->
            return . Left $
            "Could not find GitHub org/repo. For organisation make sure you use 'orgs/<org-name>' and for repos use 'repos/<username>/<repo-name>'"
        401 ->
            return . Left $
            "You do not have permission to view the GitHub projects for " ++ identifier
        _ -> return . Left $ tshow status ++ " error. Cannot fetch projects from GitHub."
