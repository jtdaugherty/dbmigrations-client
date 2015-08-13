module Main where

import Control.Monad (void)
import Control.Concurrent (newChan)
import Data.Default (def)
import System.Exit

import Database.Schema.Migrations (ensureBootstrappedBackend)
import Database.Schema.Migrations.Backend (Backend(..))
import Database.Schema.Migrations.Filesystem (filesystemStore, FilesystemStoreSettings(..))
import Moo.CommandUtils (makeBackend)

import Graphics.Vty (mkVty)
import Brick.Main

import Moo.Core

import Types
import UI
import Events

app :: App St AppEvent
app =
    App { appDraw = drawUI
        , appChooseCursor = showFirstCursor
        , appHandleEvent = appEvent
        , appStartEvent = startEvent
        , appAttrMap = const attributeMap
        , appLiftVtyEvent = VtyEvent
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

    chan <- newChan

    void $ customMain (mkVty def) chan app $ initialState cfg theBackend theStore chan
