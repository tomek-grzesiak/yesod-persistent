{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-} 

module Main where

import Protolude hiding (Handler)

import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.TH
import Yesod
import Control.Monad.Logger (runStderrLoggingT, logInfoN)

import Data.Aeson.TH


-- Define our entities as usual
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person
    firstName Text
    lastName Text
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
/person/ AllPersonR GET POST
|]


-- Nothing special here
instance Yesod PersistPg

class (HasConnectionPool m) => RunRepo m a where
    runRepo :: ReaderT SqlBackend m a -> m a

class HasConnectionPool m where
    getPool :: m ConnectionPool

instance HasConnectionPool (HandlerFor PersistPg) where
    getPool = do
       PersistPg pool <- getYesod
       return pool

instance RunRepo (HandlerFor PersistPg) a where 
    runRepo = \sql -> getPool >>= runSqlPool sql


getPersonR ::  PersonId -> Handler Value
getPersonR personId = do
    person <- runRepo $ get404 personId
    return $ toJSON person

postAllPersonR :: Handler ()
postAllPersonR = do
    person <- requireCheckJsonBody  :: Handler Person
    runRepo $ insert person
    return ()
    
getAllPersonR :: Handler Value
getAllPersonR = do
    all <- runRepo $ selectList [PersonAge >. 25, PersonAge <=. 100] []
    let decompose = fmap extractPerson all
    return $ toJSONList decompose

openConnectionCount :: Int
openConnectionCount = 10

extractPerson :: Entity Person -> Person
extractPerson (Entity y x) = x

main :: IO ()
main = runStderrLoggingT $ withPostgresqlPool "postgresql://postgres:postgres@localhost:5432/postgres" openConnectionCount $ \pool -> do
      logInfoN ("logger test":: Text)
      liftIO $ do
        let mig = migration >> ins
        runSqlPersistMPool mig pool
        warp 3000 $ PersistPg pool

migration :: MonadIO m => ReaderT SqlBackend m ()
migration = runMigration migrateAll

migrationShow :: MonadIO m => ReaderT SqlBackend m [Text]
migrationShow = showMigration migrateAll

ins :: MonadIO m => ReaderT SqlBackend m (Key Person)
ins = insert $ Person "Tomasz" "Grzesiak" 40

$(deriveJSON defaultOptions 'Person)
