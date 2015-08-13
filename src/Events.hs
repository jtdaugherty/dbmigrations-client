{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
module Events
  ( appEvent
  , startEvent
  )
where

import Control.Applicative ((<$>))
import Control.Monad (void)
import Control.Monad.Trans (liftIO)
import Control.Concurrent (forkIO, writeChan, threadDelay)
import Control.Lens
import Data.Monoid
import Graphics.Vty
import System.Process
import System.Posix.Env (getEnvDefault)

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

appEvent :: St -> AppEvent -> EventM (Next St)
appEvent st e =
    case st^.uiMode of
        MigrationListing -> migrationListingEvent st e
        EditMigration -> editMigrationEvent st e

withSelectedMigration :: St -> Lens' St (List a) -> (Int -> EventM (Next St)) -> EventM (Next St)
withSelectedMigration st listLens act =
    case st^.listLens.listSelectedL of
        Nothing -> continue st
        Just i -> act i

migrationListingEvent :: St -> AppEvent -> EventM (Next St)
migrationListingEvent st (VtyEvent e) =
    case e of
        -- Quit the program
        EvKey (KChar 'q') [] -> halt st
        EvKey KEsc [] -> halt st
        -- Up and down select migrations
        EvKey KUp [] -> continue $ st & migrationList %~ handleEvent e
        EvKey KDown [] -> continue $ st & migrationList %~ handleEvent e
        -- Edit a migration inside the program
        EvKey (KChar 'e') [] ->
            withSelectedMigration st migrationList $ \i -> do
                result <- liftIO $ S.loadMigration (st^.store) (st^.migrationList.listElementsL.ix i)
                case result of
                    Left _ -> continue st
                    Right m ->
                        continue $ st & uiMode .~ EditMigration
                                      & editMigrationName.editContentsL .~ (stringZipper [mId m] $ Just 1)
                                      & editMigrationDeps .~ migrationDepsList st (mDeps m)
                                      & editingMigration .~ Just m
        -- Create a new migration
        EvKey (KChar 'n') [] -> continue $ st & uiMode .~ EditMigration
                                              & editMigrationName.editContentsL .~ (stringZipper [] $ Just 1)
                                              & editMigrationDeps .~ migrationDepsList st []
                                              & editingMigration .~ Nothing
        -- Spawn an external editor to edit the migration
        EvKey (KChar 'E') [] ->
            withSelectedMigration st migrationList $ \i -> do
                result <- liftIO $ S.loadMigration (st^.store) (st^.migrationList.listElementsL.ix i)
                case result of
                    Left _ -> continue st
                    Right m -> suspendAndResume $ do
                        editorPath <- getEnvDefault "EDITOR" "vi"
                        path <- S.fullMigrationName (st^.store) (mId m)
                        callProcess editorPath [path]
                        return st
        _ -> continue st
migrationListingEvent st (ClearStatus msg) = continue $ clearStatus msg st

migrationDepsList :: St -> [String] -> List (Bool, String)
migrationDepsList st deps =
    list (Name "editMigrationDeps") drawElem $ flip map (st^.availableMigrations) $ \m ->
        (m `elem` deps, m)
    where
        drawElem isSelected (isDep, name) =
            let theAttr = if isSelected then withAttr listSelectedAttr else id
                d = if isDep then " * " else "   "
            in theAttr $ padRight Max $ d <+> str name

editMigrationEvent :: St -> AppEvent -> EventM (Next St)
editMigrationEvent st (VtyEvent e) =
    let migrationNameL = editMigrationName.to getEditContents.to concat
    in case e of
        -- Esc takes you back to the migration listing
        EvKey KEsc [] -> continue $ st & uiMode .~ MigrationListing
        -- Up and down navigate the dependency list
        EvKey KUp [] -> continue $ st & editMigrationDeps %~ handleEvent e
        EvKey KDown [] -> continue $ st & editMigrationDeps %~ handleEvent e
        -- Toggle the dependency state of the selected dependency
        EvKey (KChar ' ') [] ->
            withSelectedMigration st editMigrationDeps $ \i ->
                continue $ st & editMigrationDeps.listElementsL.ix i._1 %~ not
        -- Ignore enter keypresses if the migration name editor is empty
        EvKey KEnter [] | length (st^.migrationNameL) == 0 -> continue st
        -- Enter saves the migration being created or modified
        EvKey KEnter [] -> do
            result <- case st^.editingMigration of
                Nothing -> liftIO $ createNewMigration (st^.store)
                    (st^.migrationNameL)
                    (snd <$> (filter fst $ st^.editMigrationDeps.listElementsL))
                Just m -> do
                    let updatedM = m { mDeps = snd <$> (filter fst $ st^.editMigrationDeps.listElementsL)
                                     }
                    liftIO $ S.saveMigration (st^.store) updatedM
                    return $ Right updatedM
            case result of
                Left err -> continue =<< setStatus ("Error: " <> err) st
                Right _ -> continue
                    =<< setStatus "Migration saved."
                    =<< (reloadMigrations $ st & uiMode .~ MigrationListing)
        -- Only honor text input if we are editing a new migration; we
        -- don't permit the name of existing migrations to be changed
        _ -> case st^.editingMigration of
            Nothing -> continue $ st & editMigrationName %~ handleEvent e
            Just _ -> continue st
editMigrationEvent st (ClearStatus msg) = continue $ clearStatus msg st

startEvent :: St -> EventM St
startEvent = reloadMigrations

reloadMigrations :: St -> EventM St
reloadMigrations st = do
    installedMs <- liftIO $ B.getMigrations $ st^.backend
    availableMs <- liftIO $ S.getMigrations $ st^.store

    return $ st & installedMigrations .~ installedMs
                & availableMigrations .~ availableMs
                & migrationList .~ list (Name "migrationList") (const (padRight Max . str)) availableMs

setStatus :: String -> St -> EventM St
setStatus msg st = do
    -- set the status now, fork a thread that clears it later
    void $ liftIO $ forkIO $ do
        threadDelay $ 1000000 * 7
        writeChan (st^.statusChan) (ClearStatus msg)

    return $ st & status .~ Just msg

clearStatus :: String -> St -> St
clearStatus msg st =
    if st^.status == Just msg
    then st & status .~ Nothing
    else st
