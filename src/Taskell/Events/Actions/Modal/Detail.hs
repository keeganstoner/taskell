{-# LANGUAGE OverloadedLists #-}

module Taskell.Events.Actions.Modal.Detail
    ( event
    , events
    ) where

import ClassyPrelude

import Graphics.Vty.Input.Events
import Taskell.Events.Actions.Types as A (ActionType(..))
import Taskell.Events.State (clearDate, normalMode, quit, store, undo, write)
import Taskell.Events.State.Modal.Detail as Detail
import Taskell.Events.State.Types
import Taskell.Events.State.Types.Mode (DetailItem(..), DetailMode(..))
import Taskell.IO.Keyboard.Types (Actions)
import qualified Taskell.UI.Draw.Field as F (event)


import System.IO.Unsafe (unsafePerformIO)
import System.IO (hPutStrLn, stderr)

events :: Actions
events
    -- general
 =
    [ (A.Quit, quit)
    , (A.Undo, (write =<<) . undo)
    , (A.Previous, previousSubtask)
    , (A.Next, nextSubtask)
    , (A.MoveUp, (write =<<) . (up =<<) . store)
    , (A.MoveDown, (write =<<) . (down =<<) . store)
    , (A.New, (Detail.insertMode =<<) . (Detail.lastSubtask =<<) . (Detail.newItem =<<) . store)
    , (A.NewAbove, Detail.newAbove)
    , (A.NewBelow, Detail.newBelow)
    , (A.Edit, (Detail.insertMode =<<) . store)
    , (A.Complete, (write =<<) . (setComplete =<<) . store)
    , (A.Delete, (write =<<) . (Detail.remove =<<) . store)
    , (A.DueDate, (editDue =<<) . store)
    , (A.ClearDate, (write =<<) . (clearDate =<<) . store)
    , (A.Detail, (editDescription =<<) . store)
    ]

normal :: Event -> Stateful
normal (EvKey KEsc _) = normalMode
normal _ = pure


-- insert :: Event -> Stateful
-- insert (EvKey KEsc _) s = do
--     item <- getCurrentItem s
--     case item of
--         DetailDescription -> (write =<<) $ finishDescription s
--         DetailDate -> showDetail s
--         (DetailItem _) -> (write =<<) . showDetail $ finishSubtask s
-- insert (EvKey KEnter _) s = do
--     item <- getCurrentItem s
--     case item of
--         DetailDescription -> (write =<<) $ finishDescription s
--         DetailDate -> (write =<<) $ finishDue s
--         (DetailItem _) -> do
--             updatedState <- finishSubtask s
--             (write =<<) $ showDetail updatedState
-- insert e s = updateField (F.event e) s


-- insert :: Event -> Stateful
-- insert (EvKey KEsc _) s = do
--     item <- getCurrentItem s
--     case item of
--         DetailDescription -> (write =<<) $ finishDescription s
--         DetailDate -> showDetail s
--         (DetailItem _) -> (write =<<) . (showDetail =<<) $ finishSubtask s
-- -- insert (EvKey KEnter _) s = do
-- insert (EvKey KEnter mods) s
--     | null mods = do  -- Only trigger on Enter with no modifiers
--         item <- getCurrentItem s
--         case item of
--             DetailDescription -> (write =<<) $ finishDescription s
--             DetailDate -> (write =<<) $ finishDue s
--             (DetailItem _) -> do
--                 updatedState <- finishSubtask s
--                 (write =<<) $ showDetail updatedState
--     | otherwise = return s
-- insert e s = updateField (F.event e) s


insert :: Event -> Stateful
insert e s = unsafePerformIO $ do
    hPutStrLn stderr ("Received event: " ++ show e)  -- Log the event and any modifiers
    return $ case e of
        (EvKey KEsc _) -> do
            item <- getCurrentItem s
            case item of
                DetailDescription -> (write =<<) $ finishDescription s
                DetailDate -> showDetail s
                (DetailItem _) -> (write =<<) . (showDetail =<<) $ finishSubtask s
        (EvKey KEnter mods) ->
            if null mods then do
                item <- getCurrentItem s
                case item of
                    DetailDescription -> (write =<<) $ finishDescription s
                    DetailDate -> (write =<<) $ finishDue s
                    (DetailItem _) -> do
                        updatedState <- finishSubtask s
                        (write =<<) $ showDetail updatedState
            else return s  -- Allow other actions like Shift + Enter to pass through
        _ -> updateField (F.event e) s



event :: Event -> Stateful
event e s = do
    m <- getCurrentMode s
    case m of
        DetailNormal -> normal e s
        (DetailInsert _) -> insert e s
