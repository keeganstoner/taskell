{-# LANGUAGE NoImplicitPrelude #-}

module Events.Actions
    ( event
    , generateActions
    , ActionSets
    ) where

import ClassyPrelude

import Control.Lens ((^.))

import Graphics.Vty.Input.Events (Event (..))

import Events.State.Types      (State, Stateful, mode)
import Events.State.Types.Mode (DetailMode (..), ModalType (..), Mode (..))

import IO.Keyboard       (generate)
import IO.Keyboard.Types (Bindings, BoundActions)

import qualified Events.Actions.Insert       as Insert
import qualified Events.Actions.Modal        as Modal
import qualified Events.Actions.Modal.Detail as Detail
import qualified Events.Actions.Modal.Help   as Help
import qualified Events.Actions.Normal       as Normal
import qualified Events.Actions.Search       as Search

fallback :: BoundActions -> Event -> Stateful
fallback norm e state =
    case lookup e norm of
        Just ev -> ev state
        Nothing -> Normal.event e state

-- takes an event and returns a Maybe State
event' :: Event -> BoundActions -> Stateful
-- for other events pass through to relevant modules
event' e norm state =
    case state ^. mode of
        Normal    -> Normal.event e state
        Search {} -> Search.event (fallback norm) e state
        Insert {} -> Insert.event e state
        Modal {}  -> Modal.event e state
        _         -> pure state

-- returns new state if successful
event :: ActionSets -> Event -> State -> State
event actions e state = do
    let norm = normal actions
    let mEv =
            case state ^. mode of
                Normal                        -> lookup e norm
                Modal (Detail _ DetailNormal) -> lookup e $ detail actions
                Modal Help                    -> lookup e $ help actions
                _                             -> Nothing
    fromMaybe state $
        case mEv of
            Nothing -> event' e norm state
            Just ev -> ev state

data ActionSets = ActionSets
    { normal :: BoundActions
    , detail :: BoundActions
    , help   :: BoundActions
    }

generateActions :: Bindings -> ActionSets
generateActions bindings =
    ActionSets
    { normal = generate bindings Normal.events
    , detail = generate bindings Detail.events
    , help = generate bindings Help.events
    }
