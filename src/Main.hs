module Main where

import Control.Monad (void, when)
import Control.Concurrent (newChan)
import Data.Default (def)
import System.Exit
import System.Environment
import System.Console.GetOpt

import Database.Schema.Migrations (ensureBootstrappedBackend)
import Database.Schema.Migrations.Backend (Backend(..))
import Database.Schema.Migrations.Filesystem (filesystemStore, FilesystemStoreSettings(..))
import Moo.CommandUtils (makeBackend)

import Graphics.Vty (mkVty)
import Brick.Main

import Moo.Core

import Types
import UI
import Events

app :: App St AppEvent
app =
    App { appDraw = drawUI
        , appChooseCursor = showFirstCursor
        , appHandleEvent = appEvent
        , appStartEvent = startEvent
        , appAttrMap = const attributeMap
        , appLiftVtyEvent = VtyEvent
        }

data Arg = Help
         | ConfigFile FilePath
           deriving (Eq, Show)

data ArgConfig =
    ArgConfig { argConfigFile :: Maybe FilePath
              }

defaultArgConfig :: ArgConfig
defaultArgConfig =
    ArgConfig { argConfigFile = Nothing
              }

opts :: [OptDescr Arg]
opts =
    [ Option "h" ["help"] (NoArg Help) "This help output"
    , Option "c" ["config-file"] (ReqArg ConfigFile "PATH")
      ("Path to the moo config file to use (default: use only environment variables)")
    ]

updateConfig :: ArgConfig -> Arg -> ArgConfig
updateConfig c Help = c
updateConfig c (ConfigFile f) = c { argConfigFile = Just f }

usage :: IO a
usage = do
  pn <- getProgName
  let header = "Usage: " ++ pn ++ " [options]"
  putStrLn $ usageInfo header opts
  exitFailure

main :: IO ()
main = do
    args <- getArgs
    let (os, _, _) = getOpt Permute opts args
        argCfg = foldl updateConfig defaultArgConfig os

    when (Help `elem` os) usage

    result <- loadConfiguration $ argConfigFile argCfg
    cfg <- case result of
        Left e -> putStrLn e >> exitFailure
        Right c -> return c

    let theStore = filesystemStore $ FSStore $ _migrationStorePath cfg
    theBackend <- makeBackend (_databaseType cfg)
                              (DbConnDescriptor $ _connectionString cfg)

    ensureBootstrappedBackend theBackend
    commitBackend theBackend

    chan <- newChan

    void $ customMain (mkVty def) chan app $ initialState cfg theBackend theStore chan
