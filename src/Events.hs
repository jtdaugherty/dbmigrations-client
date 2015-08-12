module Events
  ( appEvent
  , startEvent
  )
where

import Control.Monad.Trans (liftIO)
import Control.Lens
import Graphics.Vty
import Brick.Main
import Brick.Widgets.List

import qualified Database.Schema.Migrations.Backend as B
import qualified Database.Schema.Migrations.Store as S

import Types

appEvent :: St -> Event -> EventM (Next St)
appEvent st e =
    case e of
        EvKey (KChar 'q') [] -> halt st
        _ -> continue st

startEvent :: St -> EventM St
startEvent = reloadMigrations

reloadMigrations :: St -> EventM St
reloadMigrations st = do
    installedMs <- liftIO $ B.getMigrations $ st^.backend
    availableMs <- liftIO $ S.getMigrations $ st^.store

    return $ st & installedMigrations .~ installedMs
                & availableMigrations .~ availableMs
                & migrationList.listElementsL .~ availableMs
