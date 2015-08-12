{-# LANGUAGE OverloadedStrings #-}
module UI
  ( drawUI
  , attributeMap
  )
where

import Control.Lens
import Data.Monoid
import Graphics.Vty
import Moo.Core (Configuration(..))

import Brick.AttrMap
import Brick.Util
import Brick.Widgets.Core
import Brick.Widgets.Edit
import Brick.Widgets.List
import Brick.Widgets.Border
import Brick.Widgets.Border.Style

import Types

attributeMap :: AttrMap
attributeMap = attrMap defAttr
    [ (headerAttr, black `on` green)
    , (footerAttr, black `on` green)
    , (editAttr,   white `on` blue)
    , (listSelectedAttr, white `on` blue)
    ]

headerAttr :: AttrName
headerAttr = "header"

footerAttr :: AttrName
footerAttr = "footer"

drawUI :: St -> [Widget]
drawUI st = [mainUI st]

mainUI :: St -> Widget
mainUI st =
    vBox [ header st
         , drawBody st
         , footer st
         ]

header :: St -> Widget
header st = withDefAttr headerAttr $
    hBox [ borderElem bsHorizontal
         , "dbmigrations"
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
    case st^.uiMode of
        MigrationListing -> "Esc:quit n:new"
        EditMigration -> "Esc:quit Spc:toggle dep Enter:save"

drawBody :: St -> Widget
drawBody st =
    case st^.uiMode of
        MigrationListing -> drawMigrationList st
        EditMigration -> drawEditMigrationForm st

drawMigrationList :: St -> Widget
drawMigrationList st = renderList (st^.migrationList)

drawEditMigrationForm :: St -> Widget
drawEditMigrationForm st =
    vBox [ "Name: " <+> renderEditor (st^.editMigrationName)
         , hBorderWithLabel "Dependencies"
         , renderList (st^.editMigrationDeps)
         ]
