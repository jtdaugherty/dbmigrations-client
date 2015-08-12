{-# LANGUAGE TemplateHaskell #-}
module Types where

import Control.Lens (makeLenses)

import Moo.Core
import qualified Database.Schema.Migrations.Backend as B
import qualified Database.Schema.Migrations.Store as S

import Brick.Widgets.List

data St =
    St { _store :: S.MigrationStore
       , _backend :: B.Backend
       , _availableMigrations :: [String]
       , _installedMigrations :: [String]
       , _migrationList :: List String
       , _config :: Configuration
       }

makeLenses ''St
