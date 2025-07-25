module Taskell.UI.Theme
    ( titleAttr
    , statusBarAttr
    , titleCurrentAttr
    , taskCurrentAttr
    , subtaskCurrentAttr
    , subtaskCompleteAttr
    , subtaskIncompleteAttr
    , taskAttr
    , disabledAttr
    , taskProj1
    , dlToAttr
    , defaultTheme
    , dlDue
    , dlSoon
    , dlOverdue
    , dlFar
    ) where


import Brick        (AttrName, attrName)
import Brick.Themes (Theme, newTheme)
import Brick.Util   (fg, on)
import Graphics.Vty
    ( defAttr, black, green, blue, magenta, yellow, red, white, bold, withStyle)

import Taskell.Data.Date (Deadline (..))

-- attrs
statusBarAttr :: AttrName
statusBarAttr = attrName "statusBar"

titleAttr :: AttrName
titleAttr = attrName "title"

titleCurrentAttr :: AttrName
titleCurrentAttr = attrName "titleCurrent"

taskCurrentAttr :: AttrName
taskCurrentAttr = attrName "taskCurrent"

taskProj1 :: AttrName
taskProj1 = attrName "taskProj1"

taskAttr :: AttrName
taskAttr = attrName "task"

subtaskCurrentAttr :: AttrName
subtaskCurrentAttr = attrName "subtaskCurrent"

subtaskCompleteAttr :: AttrName
subtaskCompleteAttr = attrName "subtaskComplete"

subtaskIncompleteAttr :: AttrName
subtaskIncompleteAttr = attrName "subtaskIncomplete"

disabledAttr :: AttrName
disabledAttr = attrName "disabled"

dlOverdue, dlDue, dlSoon, dlFar :: AttrName
dlDue = attrName "dlDue"

dlSoon = attrName "dlSoon"

dlFar = attrName "dlFar"

dlOverdue = attrName "dlOverdue"

-- convert deadline into attribute
dlToAttr :: Deadline -> AttrName
dlToAttr dl =
    case dl of
        Plenty   -> dlFar
        ThisWeek -> dlFar
        Tomorrow -> dlSoon
        Today    -> dlDue
        Passed   -> dlOverdue

-- default theme
defaultTheme :: Theme
defaultTheme =
    newTheme
        defAttr
        [ (statusBarAttr, black `on` green)
        , (titleAttr, fg green)
        , (titleCurrentAttr, fg blue)
        , (taskCurrentAttr, fg magenta)
        , (taskProj1, fg white)
        -- , (taskCurrentAttr, fg (Color240 198))
        , (subtaskCurrentAttr, fg magenta)
        , (subtaskIncompleteAttr, fg blue)
        , (subtaskCompleteAttr, fg yellow)
        -- , (taskCurrentAttr, fg magenta)
        , (disabledAttr, fg yellow)
        , (dlOverdue, withStyle (fg red) bold)    -- Make dlDue bold
        , (dlDue, fg red)    -- Make dlDue bold
        , (dlSoon, fg yellow) -- Make dlSoon bold
        , (dlFar, fg green)
        -- , (taskAttr, withStyle (fg white) bold)
        , (taskAttr, fg white)
        ]
