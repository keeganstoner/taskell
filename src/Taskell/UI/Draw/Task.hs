module Taskell.UI.Draw.Task
    ( TaskWidget(..)
    , renderTask
    , parts
    ) where

import ClassyPrelude

import Control.Lens ((^.))
import Control.Lens ((&), (.~))

import Brick

import           Taskell.Data.Date          (deadline, timeToText, timeToDisplayFormatted)
import qualified Taskell.Data.Task          as T (Task, contains, countCompleteSubtasks,
                                                  countSubtasks, description, due, hasSubtasks,
                                                  name, subtasks)
import           Taskell.Events.State.Types (current, mode, searchTerm, time, timeZone)
import           Taskell.IO.Config.Layout   (descriptionIndicator)
import           Taskell.Types              (ListIndex (..), TaskIndex (..))
import           Taskell.UI.Draw.Field      (getText, textField, widgetFromMaybe)
import           Taskell.UI.Draw.Mode
import           Taskell.UI.Draw.Types      (DSWidget, DrawState (..), ReaderDrawState, TWidget)
import           Taskell.UI.Theme
import           Taskell.UI.Types           (ResourceName)

import qualified Taskell.Data.Subtask as ST (Subtask, complete, name)

import Taskell.Data.Date (deadline, timeToDisplay)


data TaskWidget = TaskWidget
    { textW     :: TWidget
    , dateW     :: TWidget
    , summaryW  :: TWidget
    , subtasksW :: TWidget
    }

-- -- | Takes a task's 'due' property and renders a date with appropriate styling (e.g. red if overdue)
-- renderDate :: T.Task -> DSWidget
-- renderDate task = do
--     now <- (^. time) <$> asks dsState
--     tz <- (^. timeZone) <$> asks dsState
--     pure . fromMaybe emptyWidget $
--         (\date -> withAttr (dlToAttr $ deadline now date) (txt $ timeToText tz now date)) <$>
--         task ^. T.due


-- | Renders the due date with appropriate styling based on the deadline
renderDate :: T.Task -> DSWidget
renderDate task = do
    now <- (^. time) <$> asks dsState
    tz <- (^. timeZone) <$> asks dsState
    pure . fromMaybe emptyWidget $
        (\date -> withAttr (dlToAttr $ deadline now date) (txt $ timeToDisplayFormatted tz now date)) <$>
        task ^. T.due





-- | Renders the appropriate completed sub task count e.g. "[2/3]"
renderSubtaskCount :: T.Task -> DSWidget
renderSubtaskCount task =
    pure . fromMaybe emptyWidget $ bool Nothing (Just indicator) (T.hasSubtasks task)
  where
    complete = tshow $ T.countCompleteSubtasks task
    total = tshow $ T.countSubtasks task
    indicator = txt $ concat ["[", complete, "/", total, "]"]

-- | Renders the description indicator
renderDescIndicator :: T.Task -> DSWidget
renderDescIndicator task = do
    indicator <- descriptionIndicator <$> asks dsLayout
    pure . fromMaybe emptyWidget $ const (txt indicator) <$> task ^. T.description -- show the description indicator if one is set

-- | Renders the task text
renderText :: T.Task -> DSWidget
renderText task = pure $ textField (task ^. T.name)

-- | Renders the appropriate indicators: description, sub task count, and due date
indicators :: T.Task -> DSWidget
indicators task = do
    widgets <- sequence (($ task) <$> [renderDate, renderDescIndicator, renderSubtaskCount])
    pure . hBox $ padRight (Pad 1) <$> widgets

-- | The individual parts of a task widget
parts :: T.Task -> ReaderDrawState TaskWidget
parts task =
    TaskWidget <$> renderText task <*> renderDate task <*> renderDescIndicator task <*>
    renderSubtaskCount task


--- 12/4/24 Adding new feature for subtask display

-- | Renders uncompleted subtasks
renderUncompleteSubtasks :: T.Task -> DSWidget
renderUncompleteSubtasks task = do
    let subtasks = task ^. T.subtasks
    let uncompleted = filter (not . (^. ST.complete)) subtasks
    if null uncompleted
        then pure emptyWidget
        else do
            let subtaskLines = map (\st -> 
                    withAttr subtaskCompleteAttr $ 
                    txt "  • " <+> textField (st ^. ST.name)) (toList uncompleted)
            pure $ vBox subtaskLines


-- WORKS 11/6/24

-- | Renders an individual task
renderTask' :: (Int -> ResourceName) -> Int -> Int -> T.Task -> DSWidget
renderTask' rn listIndex taskIndex task = do
    eTitle <- editingTitle . (^. mode) <$> asks dsState -- is the title being edited? (for visibility)
    selected <- (== (ListIndex listIndex, TaskIndex taskIndex)) . (^. current) <$> asks dsState -- is the current task selected?
    let important = "!" `isInfixOf` (task ^. T.name) -- Check if the task name contains "!"
    let breakLineTask = "---" `isInfixOf` (task ^. T.name) -- Check for a break line task
    -- let taskName = task ^. T.name
    let taskName = task ^. T.name <> if selected then " *" else ""

    -- TEST
    let task1 = "TASK1" `isInfixOf` (task ^. T.name)

    taskField <- getField . (^. mode) <$> asks dsState -- get the field, if it's being edited
    after <- indicators task -- get the indicators widget
    
    -- widget <- renderText task
    widget <- renderText (task & T.name .~ taskName)

    -- subtasks display
    subtasksWidget <- renderUncompleteSubtasks task

    let name = rn taskIndex
        widget' = widgetFromMaybe widget taskField
        -- prefix = if selected then "> " else "  "
        postfix = if selected then " *" else ""
        -- postfix = if selected then txt " *" else emptyWidget
        
        -- attr 
        --     | important = taskCurrentAttr
        --     | task1 = taskProj1
        --     | otherwise = taskAttr
    let attr
        --   | selected = taskCurrentAttr
          | breakLineTask && "TODAY" `isInfixOf` taskName = dlDue
          | breakLineTask && "TOMORROW" `isInfixOf` taskName = dlSoon
          | breakLineTask = dlFar
          | important = dlDue
          | otherwise = taskAttr

    -- pure $
    --     cached name .
    --     (if selected && not eTitle
    --          then visible
    --          else id) .
    --     padBottom (Pad 1) .
    --     -- (<=> withAttr disabledAttr (padLeft (Pad 2) after)) .
    --     (<=> withAttr disabledAttr after) .
    --     withAttr attr $
    --     -- vBox [widget', postfix]

    --     ((if selected && not eTitle then widget' else widget) <=> subtasksWidget)-- <+> txt postfix
    pure $
        cached name .
        (if selected && not eTitle then visible else id) .
        padBottom (Pad 1) .
        withAttr attr $
        ((if selected && not eTitle then widget' else widget) <=>
         withAttr disabledAttr after <=>
         subtasksWidget)

        -- if selected && not eTitle then widget' else widget
        -- txt prefix <+> (if selected && not eTitle then widget' else widget)
        
        -- (prefixWidget <+> withAttr attr (if selected && not eTitle then widget' else widget))











-- -- | Renders an individual task
-- renderTask' :: (Int -> ResourceName) -> Int -> Int -> T.Task -> DSWidget
-- renderTask' rn listIndex taskIndex task = do
--     eTitle <- editingTitle . (^. mode) <$> asks dsState -- is the title being edited? (for visibility)
--     selected <- (== (ListIndex listIndex, TaskIndex taskIndex)) . (^. current) <$> asks dsState -- is the current task selected?

--     let isBreakLineTask = "---" `isInfixOf` (task ^. T.name)
--     let taskText = task ^. T.name
    
--     attr
--         | isBreakLineTask && "TODAY" `isInfixOf` taskText = dlDue
--         | isBreakLineTask && "TOMORROW" `isInfixOf` taskText = dlSoon
--         | isBreakLineTask = dlFar
--         | selected = taskCurrentAttr
--         | otherwise = taskAttr

--     taskField <- getField . (^. mode) <$> asks dsState -- get the field, if it's being edited
--     after <- indicators task -- get the indicators widget
--     widget <- renderText task
--     let name = rn taskIndex
--         widget' = widgetFromMaybe widget taskField
--     pure $
--         cached name .
--         (if selected && not eTitle
--              then visible
--              else id) .
--         padBottom (Pad 1) .
--         (<=> withAttr disabledAttr after) .
--         withAttr
--             (if selected
--                  then taskCurrentAttr
--                  else taskAttr) $
--         if selected && not eTitle
--             then widget'
--             else widget

renderTask :: (Int -> ResourceName) -> Int -> Int -> T.Task -> DSWidget
renderTask rn listIndex taskIndex task = do
    searchT <- (getText <$>) . (^. searchTerm) <$> asks dsState
    let taskWidget = renderTask' rn listIndex taskIndex task
    case searchT of
        Nothing -> taskWidget
        Just term ->
            if T.contains term task
                then taskWidget
                else pure emptyWidget

-- renderTask :: (Int -> ResourceName) -> Int -> Int -> Bool -> T.Task -> DSWidget
-- renderTask rn listIndex taskIndex includeNumbering task = do
--     searchT <- (getText <$>) . (^. searchTerm) <$> asks dsState
--     taskWidget <- renderTask' rn listIndex taskIndex task
--     let numberWidget = if includeNumbering then txt $ tshow (taskIndex + 1) <> ". " else emptyWidget
--     -- Combine widgets within the monadic context
--     let combinedWidget = hBox [numberWidget, taskWidget]
--     case searchT of
--         Nothing -> return combinedWidget
--         Just term -> if T.contains term task
--                         then return combinedWidget
--                         else return emptyWidget