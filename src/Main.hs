module Main where

import Control.Monad (void)
import System.Exit

import Database.Schema.Migrations (ensureBootstrappedBackend)
import Database.Schema.Migrations.Backend (Backend(..))
import Database.Schema.Migrations.Filesystem (filesystemStore, FilesystemStoreSettings(..))
import Moo.CommandUtils (makeBackend)

import Graphics.Vty (Event)
import Brick.Main
import Brick.Types
import Brick.Widgets.Core
import Brick.Widgets.List

import Moo.Core

import Types
import UI
import Events

app :: App St Event
app =
    App { appDraw = drawUI
        , appChooseCursor = neverShowCursor
        , appHandleEvent = appEvent
        , appStartEvent = startEvent
        , appAttrMap = const attributeMap
        , appLiftVtyEvent = id
        }

main :: IO ()
main = do
    result <- loadConfiguration Nothing
    cfg <- case result of
        Left e -> putStrLn e >> exitFailure
        Right c -> return c

    let theStore = filesystemStore $ FSStore $ _migrationStorePath cfg
    theBackend <- makeBackend (_databaseType cfg)
                              (DbConnDescriptor $ _connectionString cfg)

    ensureBootstrappedBackend theBackend
    commitBackend theBackend

    let initialState = St theStore theBackend [] []
                       (list (Name "migrationList") (const str) [])
                       cfg
    void $ defaultMain app initialState
