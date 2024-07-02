module Taskell.IO.Keyboard.Parser where

import ClassyPrelude hiding (try)

import Data.Attoparsec.Text

import Taskell.Utility.Parser

import Taskell.Events.Actions.Types (ActionType, read)
-- import Taskell.IO.Keyboard.Types

import Taskell.IO.Keyboard.Types (Binding(..), Bindings)
import Graphics.Vty.Input.Events (Key(..), Modifier(..))  -- Importing Key and Modifier


-- utility functions
commentP :: Parser ()
commentP = lexeme $ skipMany ((char '#' <|> char ';') *> manyTill anyChar endOfLine)

stripComments :: Parser a -> Parser a
stripComments p = lexeme $ commentP *> p <* commentP

-- ini parser
-- keyP :: Parser Binding
-- keyP = lexeme $ BKey <$> (char '<' *> word <* char '>')


-- NEW

-- Updated key parser to handle modifiers and keys
keyP :: Parser Binding
keyP = lexeme $ do
    _ <- char '<'
    mBinding <- try (BMod <$> modifierP <* char '+' <*> keyNameP) <|> 
                (BKey <$> (pack <$> manyTill anyChar (char '>')))
    _ <- char '>'
    return mBinding


modifierP :: Parser Modifier
modifierP = choice
    [ string "Shift" *> pure MShift
    , string "Ctrl"  *> pure MCtrl
    , string "Alt"   *> pure MAlt
    ]

-- Parser for Keys
keyNameP :: Parser Key
keyNameP = choice
    [ string "Up"    *> pure KUp
    , string "Down"  *> pure KDown
    , string "Left"  *> pure KLeft
    , string "Right" *> pure KRight
    , string "Enter" *> pure KEnter
    , string "Space" *> pure (KChar ' ')
    , string "Backspace" *> pure KBS
    ]
-------



charP :: Parser Binding
charP = lexeme $ BChar <$> anyChar

bindingP :: Parser [Binding]
bindingP = lexeme $ (keyP <|> charP) `sepBy` char ','

lineP :: Parser [(Binding, ActionType)]
lineP =
    stripComments $ do
        name <- read <$> word
        _ <- lexeme $ char '='
        binds <- bindingP
        pure $ (, name) <$> binds

bindingsP :: Parser Bindings
bindingsP = stripComments $ concat <$> many' lineP

-- run parser
bindings :: Text -> Either Text Bindings
bindings ini = first (const "Could not parse keyboard bindings.") (parseOnly bindingsP ini)
