{-# LANGUAGE OverloadedStrings #-}
module UI
  ( drawUI
  , attributeMap
  )
where

import Control.Applicative
import Control.Lens
import Data.List (intersperse)
import Data.Monoid
import Graphics.Vty
import Moo.Core (Configuration(..))

import Database.Schema.Migrations.Migration (Migration(mId))

import Brick.Types (Widget, Padding(..))
import Brick.AttrMap
import Brick.Util
import Brick.Markup
import Brick.Widgets.Core
import Brick.Widgets.Edit
import Brick.Widgets.List
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Data.Text.Markup (fromText)

import Types

attributeMap :: AttrMap
attributeMap = attrMap defAttr
    [ (headerAttr, black `on` green)
    , (footerAttr, black `on` green)
    , (editAttr,   white `on` blue)
    , (listSelectedAttr, white `on` blue)
    , (keyAttr,    fg white)
    ]

headerAttr :: AttrName
headerAttr = "header"

keyAttr :: AttrName
keyAttr = "keybinding"

footerAttr :: AttrName
footerAttr = "footer"

drawUI :: St -> [Widget]
drawUI st = [mainUI st]

mainUI :: St -> Widget
mainUI st =
    vBox [ header st
         , drawBody st
         , footer st
         , str $ maybe " " id $ st^.status
         ]

header :: St -> Widget
header st = withDefAttr headerAttr $
    hBox [ borderElem bsHorizontal
         , str "dbmigrations"
         , hBorder
         , str $ "Path: " <> (_migrationStorePath $ st^.config)
         , borderElem bsHorizontal
         ]

footer :: St -> Widget
footer st = withDefAttr footerAttr $
    hBox [ borderElem bsHorizontal
         , help st
         , hBorder
         , str $ _connectionString $ st^.config
         , borderElem bsHorizontal
         ]

help :: St -> Widget
help st =
    let mkPair (k, desc) = (k @? keyAttr) <> (fromText $ ":" <> desc)
        pairs = case st^.uiMode of
          MigrationListing -> [ ("Esc", "quit")
                              , ("n", "new")
                              , ("e", "edit")
                              , ("E", "edit-raw")
                              ]
          EditMigration -> [ ("Esc", "cancel")
                           , ("Space", "toggle-dep")
                           , ("Enter", "save")
                           ]
    in markup $ mconcat $ intersperse " " $ mkPair <$> pairs

drawBody :: St -> Widget
drawBody st =
    case st^.uiMode of
        MigrationListing -> drawMigrationList st
        EditMigration -> drawEditMigrationForm st

drawMigrationList :: St -> Widget
drawMigrationList st = renderList (st^.migrationList) $ (const (padRight Max . str))

drawEditMigrationForm :: St -> Widget
drawEditMigrationForm st =
    vBox [ str "Name: " <+> case st^.editingMigration of
             Nothing -> renderEditor (st^.editMigrationName)
             Just m -> str $ mId m
         , hBorderWithLabel $ str "Dependencies"
         , renderList (st^.editMigrationDeps) drawMigrationDepListElem
         ]

drawMigrationDepListElem :: Bool -> (Bool, String) -> Widget
drawMigrationDepListElem isSelected (isDep, name) =
    let theAttr = if isSelected then withAttr listSelectedAttr else id
        d = str $ if isDep then " * " else "   "
    in theAttr $ padRight Max $ d <+> str name
