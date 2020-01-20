{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Events.Actions.Types where

import ClassyPrelude hiding (Left, Nothing, Right)

data ActionType
    = Quit
    | Undo
    | Redo
    | Search
    | Due
    | Help
    | Previous
    | Next
    | Left
    | Right
    | Bottom
    | Top
    | New
    | NewAbove
    | NewBelow
    | Duplicate
    | Edit
    | Clear
    | Delete
    | Detail
    | DueDate
    | ClearDate
    | MoveUp
    | MoveDown
    | MoveLeft
    | MoveRight
    | Complete
    | MoveMenu
    | ListNew
    | ListEdit
    | ListDelete
    | ListRight
    | ListLeft
    | Nothing
    deriving (Show, Eq, Ord, Enum)

allActions :: [ActionType]
allActions = [toEnum 0 ..]

read :: Text -> ActionType
read "quit"       = Quit
read "undo"       = Undo
read "redo"       = Redo
read "search"     = Search
read "due"        = Due
read "help"       = Help
read "previous"   = Previous
read "next"       = Next
read "left"       = Left
read "right"      = Right
read "bottom"     = Bottom
read "top"        = Top
read "new"        = New
read "newAbove"   = NewAbove
read "newBelow"   = NewBelow
read "duplicate"  = Duplicate
read "edit"       = Edit
read "clear"      = Clear
read "delete"     = Delete
read "detail"     = Detail
read "dueDate"    = DueDate
read "clearDate"  = ClearDate
read "moveUp"     = MoveUp
read "moveDown"   = MoveDown
read "moveLeft"   = MoveLeft
read "moveRight"  = MoveRight
read "complete"   = Complete
read "moveMenu"   = MoveMenu
read "listNew"    = ListNew
read "listEdit"   = ListEdit
read "listDelete" = ListDelete
read "listRight"  = ListRight
read "listLeft"   = ListLeft
read _            = Nothing
