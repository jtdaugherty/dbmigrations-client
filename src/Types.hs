{-# LANGUAGE TemplateHaskell #-}
module Types where

import Control.Lens (makeLenses)

import Database.Schema.Migrations.Backend (Backend)
import Database.Schema.Migrations.Store (MigrationStore)

data St =
    St { _store :: MigrationStore
       , _backend :: Backend
       }

makeLenses ''St
