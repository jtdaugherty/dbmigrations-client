{-# LANGUAGE TemplateHaskell #-}
module Types
  ( St(..)
  , UIMode(..)
  , uiMode, backend, store, config
  , availableMigrations, installedMigrations
  , migrationList, newMigrationName, newMigrationDeps
  , initialState
  )
where

import Control.Lens (makeLenses)

import Moo.Core
import qualified Database.Schema.Migrations.Backend as B
import qualified Database.Schema.Migrations.Store as S

import Brick.Types
import Brick.Widgets.Core
import Brick.Widgets.List
import Brick.Widgets.Edit

data UIMode = MigrationListing | CreateMigration

data St =
    St { _uiMode :: UIMode
       , _backend :: B.Backend
       , _store :: S.MigrationStore
       , _config :: Configuration

       -- Data from the most recent read of the store and backend
       , _availableMigrations :: [String]
       , _installedMigrations :: [String]

       -- mode: MigrationListing
       , _migrationList :: List String

       -- mode: CreateMigration
       , _newMigrationName :: Editor
       , _newMigrationDeps :: List (Bool, String)
       }

makeLenses ''St

initialState :: Configuration -> B.Backend -> S.MigrationStore -> St
initialState cfg theBackend theStore =
    St { _uiMode = MigrationListing
       , _backend = theBackend
       , _store = theStore
       , _availableMigrations = []
       , _installedMigrations = []
       , _migrationList = list (Name "migrationList") (const str) []
       , _config = cfg
       , _newMigrationName = editor (Name "newMigrationName") (str . unlines) (Just 1) ""
       , _newMigrationDeps = list (Name "newMigrationDeps") (const (str . snd)) []
       }
