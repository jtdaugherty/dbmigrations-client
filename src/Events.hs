{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
module Events
  ( appEvent
  , startEvent
  )
where

import Control.Applicative ((<$>))
import Control.Monad.Trans (liftIO)
import Control.Lens
import Graphics.Vty
import Brick.Main
import Brick.Types
import Brick.Widgets.Core
import Brick.Widgets.List
import Brick.Widgets.Edit

import Data.Text.Zipper

import Database.Schema.Migrations
  ( createNewMigration
  )
import Database.Schema.Migrations.Migration (Migration(..))
import qualified Database.Schema.Migrations.Backend as B
import qualified Database.Schema.Migrations.Store as S

import Types

appEvent :: St -> Event -> EventM (Next St)
appEvent st e =
    case st^.uiMode of
        MigrationListing -> migrationListingEvent st e
        EditMigration -> editMigrationEvent st e

migrationListingEvent :: St -> Event -> EventM (Next St)
migrationListingEvent st e =
    case e of
        EvKey (KChar 'q') [] -> halt st
        EvKey KEsc [] -> halt st
        EvKey KUp [] -> continue $ st & migrationList %~ handleEvent e
        EvKey KDown [] -> continue $ st & migrationList %~ handleEvent e
        EvKey (KChar 'e') [] -> do
            case st^.migrationList.listSelectedL of
                Nothing -> continue st
                Just i -> do
                    result <- liftIO $ S.loadMigration (st^.store) (st^.migrationList.listElementsL.ix i)
                    case result of
                        Left _ -> continue st
                        Right m ->
                            continue $ st & uiMode .~ EditMigration
                                          & editMigrationName.editContentsL .~ (stringZipper [mId m] $ Just 1)
                                          & editMigrationDeps .~ migrationDepsList st (mDeps m)
                                          & editingMigration .~ Just m
        EvKey (KChar 'n') [] -> continue $ st & uiMode .~ EditMigration
                                              & editMigrationName.editContentsL .~ (stringZipper [] $ Just 1)
                                              & editMigrationDeps .~ migrationDepsList st []
                                              & editingMigration .~ Nothing
        _ -> continue st

migrationDepsList :: St -> [String] -> List (Bool, String)
migrationDepsList st deps =
    list (Name "editMigrationDeps") drawElem $ flip map (st^.availableMigrations) $ \m ->
        (m `elem` deps, m)
    where
        drawElem isSelected (isDep, name) =
            let theAttr = if isSelected then withAttr listSelectedAttr else id
                d = if isDep then " * " else "   "
            in theAttr $ padRight Max $ d <+> str name

editMigrationEvent :: St -> Event -> EventM (Next St)
editMigrationEvent st e =
    case e of
        EvKey KEsc [] -> continue $ st & uiMode .~ MigrationListing
        EvKey KUp [] -> continue $ st & editMigrationDeps %~ handleEvent e
        EvKey KDown [] -> continue $ st & editMigrationDeps %~ handleEvent e
        EvKey (KChar ' ') [] -> case st^.editMigrationDeps.listSelectedL of
            Nothing -> continue st
            Just i -> continue $ st & editMigrationDeps.listElementsL.ix i._1 %~ not
        EvKey KEnter [] -> do
            status <- case st^.editingMigration of
                Nothing -> liftIO $ createNewMigration (st^.store)
                    (concat $ getEditContents $ st^.editMigrationName)
                    (snd <$> (filter fst $ st^.editMigrationDeps.listElementsL))
                Just m -> do
                    let updatedM = m { mDeps = snd <$> (filter fst $ st^.editMigrationDeps.listElementsL)
                                     }
                    liftIO $ S.saveMigration (st^.store) updatedM
                    return $ Right updatedM
            continue =<< (reloadMigrations $ st & uiMode .~ MigrationListing)
        _ -> case st^.editingMigration of
            Nothing -> continue st
            Just _ -> continue $ st & editMigrationName %~ handleEvent e

startEvent :: St -> EventM St
startEvent = reloadMigrations

reloadMigrations :: St -> EventM St
reloadMigrations st = do
    installedMs <- liftIO $ B.getMigrations $ st^.backend
    availableMs <- liftIO $ S.getMigrations $ st^.store

    return $ st & installedMigrations .~ installedMs
                & availableMigrations .~ availableMs
                & migrationList .~ list (Name "migrationList") (const (padRight Max . str)) availableMs
