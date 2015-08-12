module Events
  ( appEvent
  )
where

import Graphics.Vty
import Brick.Main

import Types

appEvent :: St -> Event -> EventM (Next St)
appEvent st e =
    case e of
        EvKey (KChar 'q') [] -> halt st
        _ -> continue st
