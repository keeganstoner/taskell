module Taskell.UI.Draw.Modal.Detail
    ( detail
    ) where

import ClassyPrelude

import Control.Lens ((^.))

import Data.Sequence (mapWithIndex)

import Brick

import Data.Time.Zones (TZ)

import           Taskell.Data.Date                 (deadline, timeToDisplay, timeToDisplayFormatted)
import qualified Taskell.Data.Subtask              as ST (Subtask, complete, name)
import           Taskell.Data.Task                 (Task, description, due, name, subtasks)
import           Taskell.Events.State              (getCurrentTask)
import           Taskell.Events.State.Modal.Detail (getCurrentItem, getField)
import           Taskell.Events.State.Types        (time, timeZone)
import           Taskell.Events.State.Types.Mode   (DetailItem (..))
import           Taskell.UI.Draw.Field             (Field, textField, widgetFromMaybe)
import           Taskell.UI.Draw.Types             (DrawState (..), ModalWidget, TWidget)
import           Taskell.UI.Theme                  (disabledAttr, dlToAttr, subtaskCompleteAttr,
                                                    subtaskCurrentAttr, subtaskIncompleteAttr, taskCurrentAttr)

import Data.Time.Clock (UTCTime) -- Make sure you have this import


-- renderSubtask :: Maybe Field -> DetailItem -> Int -> ST.Subtask -> TWidget
-- renderSubtask f current i subtask = padBottom (Pad 1) $ prefix <+> final
--   where
--     cur =
--         case current of
--             DetailItem c -> i == c
--             _            -> False
--     done = subtask ^. ST.complete
--     attr =
--         withAttr
--             (if cur
--                  then subtaskCurrentAttr
--                  else (if done
--                            then subtaskCompleteAttr
--                            else subtaskIncompleteAttr))
--     prefix =
--         attr . txt $
--         if done
--             then "[x] "
--             else "[ ] "
--     widget = textField (subtask ^. ST.name)
--     final
--         | cur = visible . attr $ widgetFromMaybe widget f
--         | otherwise = attr widget


renderSubtask :: Maybe Field -> DetailItem -> Int -> ST.Subtask -> TWidget
renderSubtask f current i subtask = padBottom (Pad 1) $ prefix <+> final-- <+> postfix
  where
    cur =
        case current of
            DetailItem c -> i == c
            _            -> False
    done = subtask ^. ST.complete
    important = "!" `isInfixOf` (subtask ^. ST.name) -- Check if the subtask name contains "!"

    -- Define the attribute based on completion status instead of selection
    attr = withAttr $ if done
                        then subtaskCompleteAttr
                        else if important && not done
                            then subtaskCurrentAttr -- Use the current attribute for important uncompleted subtasks
                            else subtaskIncompleteAttr
                            
    -- Define prefix to include the ">" only when selected
    -- prefixSymbol = if cur then "> " else "  "  -- Add a ">" symbol if the subtask is selected
    checkbox = 
        if done
            then "[x] "
            else "[ ] "
    prefix =
        attr . txt $ checkbox -- Combine the new symbol with the existing checkbox
    
    subtaskName = subtask ^. ST.name <> if cur then " *" else ""

    -- postfix = if cur then txt " *" else emptyWidget

    widget = textField subtaskName
    final
        | cur = visible . attr $ widgetFromMaybe widget f
        | otherwise = attr widget


renderSummary :: Maybe Field -> DetailItem -> Task -> TWidget
renderSummary f i task = padTop (Pad 1) $ padBottom (Pad 2) w'
  where
    w = textField $ fromMaybe "No description" (task ^. description)
    w' =
        case i of
            DetailDescription -> visible $ widgetFromMaybe w f
            _                 -> w





-- renderDate :: TZ -> UTCTime -> Maybe Field -> DetailItem -> Task -> TWidget
-- renderDate tz now field item task =
--     case item of
--         DetailDate -> visible $ prefix <+> widgetFromMaybe widget field
--         _ ->
--             case day of
--                 Just d  -> prefix <+> withAttr (dlToAttr (deadline now d)) widget
--                 Nothing -> emptyWidget
--   where
--     day = task ^. due
--     prefix = txt "Due: "
--     widget = textField $ maybe "" (timeToDisplay tz) day


-- Rendering function for the date field in the task details
renderDate :: TZ -> UTCTime -> Maybe Field -> DetailItem -> Task -> TWidget
renderDate tz now field item task =
    case item of
        DetailDate -> visible $ prefix <+> widgetFromMaybe widget field
        _ ->
            case day of
                Just d  -> prefix <+> withAttr (dlToAttr (deadline now d)) widget
                Nothing -> emptyWidget
  where
    day = task ^. due
    prefix = txt "Due: "
    widget = textField $ maybe "" (timeToDisplayFormatted tz now) day  -- Pass 'now' to timeToDisplayFormatted





detail :: ModalWidget
detail = do
    state <- asks dsState
    let now = state ^. time
    let tz = state ^. timeZone
    pure $
        fromMaybe ("Error", txt "Oops") $ do
            task <- getCurrentTask state
            i <- getCurrentItem state
            let f = getField state
            let sts = task ^. subtasks
                w
                    | null sts = withAttr disabledAttr $ txt "No sub-tasks"
                    | otherwise = vBox . toList $ renderSubtask f i `mapWithIndex` sts
            -- let taskNameWidget = withAttr taskCurrentAttr $ txt (task ^. name)

            pure (task ^. name, renderDate tz now f i task <=> renderSummary f i task <=> w)
            -- pure (task ^. name, taskNameWidget <=> renderDate tz now f i task <=> renderSummary f i task <=> w)
