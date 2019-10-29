{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes       	        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

import           Control.Monad.Logger    (runNoLoggingT)
import           Data.Conduit            (awaitForever, (.|))
import           Data.Text               (intercalate, Text)
import           Database.Persist.Sqlite (ConnectionPool, 
                                          runMigration, createSqlitePoolFromInfo,
                                          mkSqliteConnectionInfo, runSqlPool,
                                          SqlBackend, SqlPersistT)
import           Yesod.Core
import           Yesod.Persist

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person
    name Text
|]

data App = App { appConnPool :: ConnectionPool }

mkYesod "App" [parseRoutes|
/stream StreamR GET
/buffer BufferedR GET
|]

instance Yesod App
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB :: SqlPersistT Handler a -> Handler a
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master

instance YesodPersistRunner App where
    getDBRunner :: Handler (DBRunner App, Handler ())
    getDBRunner = defaultGetDBRunner appConnPool

getStreamR :: Handler TypedContent
getStreamR =
    respondSourceDB typePlain $ selectSource [] [Asc PersonName] .| awaitForever toBuilder
  where
    toBuilder (Entity _ (Person name)) = do
        sendChunkText name
        sendChunkText "\n"
        sendFlush

getBufferedR :: Handler TypedContent
getBufferedR = do
    names <- runDB $ selectList [] [Asc PersonName]
    return $ toTypedContent $ intercalate "\n" $ map (personName . entityVal) names

main :: IO ()
main = do
    pool <- runNoLoggingT $ createSqlitePoolFromInfo (mkSqliteConnectionInfo "db.sqlite3") 1
    runNoLoggingT $ flip runSqlPool pool $ do
        runMigration migrateAll
        deleteWhere ([] :: [Filter Person])
        insert_ $ Person "Charlie"
        insert_ $ Person "Alice"
        insert_ $ Person "Bob"
    warp 3000 App { appConnPool = pool }
