{-# LANGUAGE TupleSections #-}
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

migrationListingEvent :: St -> AppEvent -> EventM (Next St)
migrationListingEvent st (VtyEvent e) =
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
        EvKey (KChar 'E') [] -> do
            case st^.migrationList.listSelectedL of
                Nothing -> continue st
                Just i -> do
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
        EvKey KEsc [] -> continue $ st & uiMode .~ MigrationListing
        EvKey KUp [] -> continue $ st & editMigrationDeps %~ handleEvent e
        EvKey KDown [] -> continue $ st & editMigrationDeps %~ handleEvent e
        EvKey (KChar ' ') [] -> case st^.editMigrationDeps.listSelectedL of
            Nothing -> continue st
            Just i -> continue $ st & editMigrationDeps.listElementsL.ix i._1 %~ not
        EvKey KEnter [] | length (st^.migrationNameL) == 0 -> continue st
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
