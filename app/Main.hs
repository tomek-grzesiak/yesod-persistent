{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.TH
import Database.Persist.Sql
import Yesod
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Logger (runStderrLoggingT, logInfoN)
import Data.Pool (Pool)
import Control.Monad.Reader
import Data.Text
import Data.Aeson
import GHC.Generics

-- Define our entities as usual
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person
    firstName String
    lastName String
    age Int
    PersonName firstName lastName
    deriving Show Generic
|]

-- We keep our connection pool in the foundation. At program initialization, we
-- create our initial pool, and each time we need to perform an action we check
-- out a single connection from the pool.
newtype PersistPg = PersistPg ConnectionPool

-- We'll create a single route, to access a person. It's a very common
-- occurrence to use an Id type in routes.
mkYesod "PersistPg" [parseRoutes|
/person/#PersonId PersonR GET
|]

instance ToJSON Person
instance FromJSON Person

-- Nothing special here
instance Yesod PersistPg

-- Now we need to define a YesodPersist instance, which will keep track of
-- which backend we're using and how to run an action.
instance YesodPersist PersistPg where
    type YesodPersistBackend PersistPg = SqlBackend

    runDB action = do
        PersistPg pool <- getYesod
        runSqlPool action pool


-- We'll just return the show value of a person, or a 404 if the Person doesn't
-- exist.

getPersonR :: PersonId -> Handler Value
getPersonR personId = do
    person <- runDB $ get404 personId
    all <- runDB $ selectList [PersonAge >. 25, PersonAge <=. 30] []
    return $ toJSON person

openConnectionCount :: Int
openConnectionCount = 10

main :: IO ()
main = runStderrLoggingT $ withPostgresqlPool "postgresql://postgres:test@localhost:5432/postgres" openConnectionCount $ \pool -> do
      logInfoN ("logger test":: Text)
      liftIO $ do
        warp 3000 $ PersistPg pool
        runResourceT $ flip runSqlPool pool $ do
            migration
            ins

migration :: MonadIO m => ReaderT SqlBackend m ()
migration = runMigration migrateAll

ins :: MonadIO m => ReaderT SqlBackend m ()
ins = do
     insert $ Person "Michael" "Snoyman" 26
     return ()