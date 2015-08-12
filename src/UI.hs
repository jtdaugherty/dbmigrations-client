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
import Brick.Widgets.List
import Brick.Widgets.Border
import Brick.Widgets.Border.Style

import Types

attributeMap :: AttrMap
attributeMap = attrMap defAttr
    [ (headerAttr, black `on` green)
    , (footerAttr, black `on` green)
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
         , drawMigrationList st
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
         , help
         , hBorder
         , str $ _connectionString $ st^.config
         , borderElem bsHorizontal
         ]

help :: Widget
help = "q:quit"

drawMigrationList :: St -> Widget
drawMigrationList st = renderList (st^.migrationList)
