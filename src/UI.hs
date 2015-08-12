{-# LANGUAGE OverloadedStrings #-}
module UI
  ( drawUI
  , attributeMap
  )
where

import Graphics.Vty

import Brick.AttrMap
import Brick.Util
import Brick.Widgets.Core
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
    vBox [ header
         , migrationList st
         , footer
         ]

header :: Widget
header = withDefAttr headerAttr $
    borderElem bsHorizontal <+> "dbmigrations" <+> hBorder

footer :: Widget
footer = withDefAttr footerAttr $
    borderElem bsHorizontal <+> help <+> hBorder

help :: Widget
help = "q:quit"

migrationList :: St -> Widget
migrationList st = "migration list" <+> fill ' '
