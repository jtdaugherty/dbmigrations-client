module Main where

import Control.Monad (void)
import Graphics.Vty (Event)
import Brick.Main

import Types
import UI
import Events

app :: App St Event
app =
    App { appDraw = drawUI
        , appChooseCursor = neverShowCursor
        , appHandleEvent = appEvent
        , appStartEvent = return
        , appAttrMap = const attributeMap
        , appLiftVtyEvent = id
        }

main :: IO ()
main = do
    let initialState = St undefined undefined
    void $ defaultMain app initialState
