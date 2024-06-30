module Taskell.UI.Draw.Modal.Due
    ( due
    ) where

import ClassyPrelude

import Brick
import Taskell.Data.Seq ((<#>))

import qualified Taskell.Data.Task     as T (Task)
import           Taskell.Types         (Pointer)
import           Taskell.UI.Draw.Task  (TaskWidget (..), parts)
import           Taskell.UI.Draw.Types (DSWidget, ModalWidget)
import           Taskell.UI.Theme      (taskAttr, taskCurrentAttr)
import           Taskell.UI.Types      (ResourceName (RNDue))

-- Function to render a single task within the modal
renderTask :: Int -> Int -> T.Task -> DSWidget
renderTask current position task = do
    (TaskWidget text date _ _) <- parts task
    let selected = current == position
    let attr = if selected then taskCurrentAttr else taskAttr
    let shw = if selected then visible else id
    let indexText = txt $ tshow (position + 1) <> ". "
    let taskContent = vBox [text, date]  -- This keeps the date and text in their original vertical arrangement
    pure . shw . cached (RNDue position) . withAttr attr . padBottom (Pad 1) $
        hBox [padRight (Pad 1) indexText, taskContent]  -- Align index to the left of the task content


-- The main function for rendering the 'Due' modal which displays tasks with due dates
due :: Seq (Pointer, T.Task) -> Int -> ModalWidget
due tasks selected = do
    let renderIndexedTask (index, (_, task)) = renderTask selected index task
    let indexedTasks = zip [0..] (toList tasks)  -- Create an indexed list of tasks
    widgets <- mapM renderIndexedTask indexedTasks  -- Render each indexed task
    let widgetBox = if null indexedTasks
                        then txt "No due tasks"
                        else vBox widgets  -- Box all widgets vertically
    pure ("Due Tasks", widgetBox)