{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module TestImport
  ( module TestImport,
    module X,
  )
where

import Application (makeFoundation, makeLogWare)
import ClassyPrelude as X hiding (Handler, delete, deleteBy)
-- Wiping the test database

import Control.Monad.Fail (MonadFail)
import Control.Monad.Trans.Control (MonadBaseControl)
import Database.MongoDB.Admin (dropCollection)
import Database.MongoDB.Query (allCollections)
import Database.Persist as X hiding (get)
import Database.Persist.MongoDB hiding (master)
import Foundation as X
import Model as X
import Settings (appDatabaseConf)
import Test.Hspec as X
import Yesod.Auth as X
import Yesod.Core.Unsafe (fakeHandlerGetLogger)
import Yesod.Default.Config2 (loadYamlSettings, useEnv)
import Yesod.Test as X

runDB :: Action IO a -> YesodExample App a
runDB query = do
  app <- getTestYesod
  liftIO $ runDBWithApp app query

runDBWithApp :: App -> Action IO a -> IO a
runDBWithApp app query = do
  liftIO $
    runMongoDBPool
      (mgAccessMode $ appDatabaseConf $ appSettings app)
      query
      (appConnPool app)

runHandler :: Handler a -> YesodExample App a
runHandler handler = do
  app <- getTestYesod
  fakeHandlerGetLogger appLogger app handler

withApp :: SpecWith (TestApp App) -> Spec
withApp = before $ do
  settings <-
    loadYamlSettings
      ["config/test-settings.yml", "config/settings.yml"]
      []
      useEnv
  foundation <- makeFoundation settings
  wipeDB foundation
  logWare <- liftIO $ makeLogWare foundation
  return (foundation, logWare)

-- This function will wipe your database.
-- 'withApp' calls it before each test, creating a clean environment for each
-- spec to run in.
wipeDB :: App -> IO ()
wipeDB app = void $ runDBWithApp app dropAllCollections

dropAllCollections :: (MonadIO m, MonadBaseControl IO m, MonadFail m) => Action m [Bool]
dropAllCollections = (filter (not . isSystemCollection) <$> allCollections) >>= mapM dropCollection
  where
    isSystemCollection = isPrefixOf "system."
