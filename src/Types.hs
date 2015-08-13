{-# LANGUAGE TemplateHaskell #-}
module Types
  ( St(..)
  , UIMode(..)
  , AppEvent(..)
  , uiMode, backend, store, config
  , availableMigrations, installedMigrations
  , migrationList, editMigrationName, editMigrationDeps
  , initialState, editingMigration, status
  , setStatus
  )
where

import Control.Lens
import Control.Monad (void)
import Control.Monad.Trans (liftIO)
import Control.Concurrent (Chan, forkIO, writeChan, threadDelay)
import Graphics.Vty (Event)

import Moo.Core
import Database.Schema.Migrations.Migration (Migration)
import qualified Database.Schema.Migrations.Backend as B
import qualified Database.Schema.Migrations.Store as S

import Brick.Types
import Brick.Main
import Brick.Widgets.Core
import Brick.Widgets.List
import Brick.Widgets.Edit

data UIMode = MigrationListing | EditMigration

data AppEvent =
    VtyEvent Event
    | ClearStatus String

data St =
    St { _uiMode :: UIMode
       , _backend :: B.Backend
       , _store :: S.MigrationStore
       , _config :: Configuration
       , _status :: Maybe String
       , _statusChan :: Chan AppEvent

       -- Data from the most recent read of the store and backend
       , _availableMigrations :: [String]
       , _installedMigrations :: [String]

       -- mode: MigrationListing
       , _migrationList :: List String

       -- mode: EditMigration
       , _editMigrationName :: Editor
       , _editMigrationDeps :: List (Bool, String)
       , _editingMigration :: Maybe Migration
       }

makeLenses ''St

initialState :: Configuration -> B.Backend -> S.MigrationStore -> Chan AppEvent -> St
initialState cfg theBackend theStore ch =
    St { _uiMode = MigrationListing
       , _backend = theBackend
       , _store = theStore
       , _availableMigrations = []
       , _installedMigrations = []
       , _migrationList = list (Name "migrationList") (const str) []
       , _config = cfg
       , _editMigrationName = editor (Name "editMigrationName") (str . unlines) (Just 1) ""
       , _editMigrationDeps = list (Name "editMigrationDeps") (const (str . snd)) []
       , _editingMigration = Nothing
       , _status = Nothing
       , _statusChan = ch
       }

setStatus :: String -> St -> EventM St
setStatus msg st = do
    -- set the status now, fork a thread that clears it later
    void $ liftIO $ forkIO $ do
        threadDelay $ 1000000 * 7
        writeChan (st^.statusChan) (ClearStatus msg)

    return $ st & status .~ Just msg
