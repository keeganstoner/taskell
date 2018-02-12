{-# LANGUAGE OverloadedStrings #-}
module IO.Config where

import System.Directory
import Data.Ini.Config
import UI.Theme
import Brick.Themes (themeToAttrMap, loadCustomizations)
import Brick (AttrMap)
import qualified Data.Text.IO as T

data GeneralConfig = GeneralConfig {
        filename :: FilePath
    }

data LayoutConfig = LayoutConfig {
        columnWidth :: Int,
        columnPadding :: Int
    }

data Config = Config {
        general :: GeneralConfig,
        layout :: LayoutConfig
    }

defaultConfig :: Config
defaultConfig = Config {
        general = GeneralConfig {
            filename = "taskell.md"
        },
        layout = LayoutConfig {
            columnWidth = 24,
            columnPadding = 3
        }
    }

getDir :: IO FilePath
getDir = (++ "/.taskell") <$> getHomeDirectory

getThemePath :: IO FilePath
getThemePath = (++ "/theme.ini") <$> getDir

getConfigPath :: IO FilePath
getConfigPath = (++ "/config.ini") <$> getDir

setup :: IO Config
setup = do
    getDir >>= createDirectoryIfMissing True
    createConfig
    createTheme
    getConfig

writeTheme :: FilePath -> IO ()
writeTheme path = writeFile path $ unlines [
        "[default]",
        "default.bg = brightBlack",
        "default.fg = white",
        "",
        "[other]",
        "title.fg = green",
        "titleCurrent.fg = blue",
        "taskCurrent.fg = magenta"
    ]

createTheme :: IO ()
createTheme = do
    path <- getThemePath
    exists <- doesFileExist path
    if exists then return () else writeTheme path

writeConfig :: FilePath -> IO ()
writeConfig path = writeFile path $ unlines [
        "[general]",
        "filename = " ++ filename (general defaultConfig),
        "",
        "[layout]",
        "column_width = " ++ (show . columnWidth $ layout defaultConfig),
        "column_padding = " ++ (show . columnPadding $ layout defaultConfig)
    ]

createConfig :: IO ()
createConfig = do
    path <- getConfigPath
    exists <- doesFileExist path
    if exists then return () else writeConfig path

configParser :: IniParser Config
configParser = do
    generalCf <- section "general" $ do
        filenameCf <- fieldOf "filename" string
        return GeneralConfig { filename = filenameCf }
    layoutCf <- section "layout" $ do
        columnWidthCf <- fieldOf "column_width" number
        columnPaddingCf <- fieldOf "column_padding" number
        return LayoutConfig { columnWidth = columnWidthCf, columnPadding = columnPaddingCf }
    return Config { general = generalCf, layout = layoutCf }

getConfig :: IO Config
getConfig = do
    content <- getConfigPath >>= T.readFile
    let config = parseIniFile content configParser

    return $ case config of
        Right c -> c
        Left _ -> defaultConfig

-- generate theme
generateAttrMap :: IO AttrMap
generateAttrMap = do
    path <- getThemePath
    customizedTheme <- loadCustomizations path defaultTheme
    return . themeToAttrMap $ case customizedTheme of
        Left _ -> defaultTheme
        Right theme -> theme
