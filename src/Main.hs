{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger
import Control.Monad.Trans.Resource (runResourceT, ResourceT)
import Data.Aeson hiding (json)
import Data.Aeson.TH
import Data.Char (toLower)
import qualified Data.Text as Text
import Data.Maybe (fromMaybe)
import Network.HTTP.Types.Status (status404)
import Network.Wai (Middleware)
import Network.Wai.Middleware.AddHeaders
import System.Environment
import Web.Scotty
import Web.PathPieces
import qualified Database.Persist.Class as DB
import qualified Database.Persist.Sqlite as Sqlite
import Database.Persist.TH


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Todo
    title String
    completed Bool
    order Int
    deriving Show
|]

$(deriveToJSON defaultOptions { fieldLabelModifier = (map toLower . drop 4)} ''Todo)

instance ToJSON (Sqlite.Entity Todo) where
  toJSON entity = object
      [ "id" .= key
      , "url" .= ("http://todobackend-scotty.herokuapp.com/todos/" ++ keyText)
      , "title" .= todoTitle val
      , "completed" .= todoCompleted val
      , "order" .= todoOrder val
      ]
    where
      key = Sqlite.entityKey entity
      val = Sqlite.entityVal entity
      keyText = Text.unpack $ toPathPiece key


data TodoAction = TodoAction
  { _actTitle :: Maybe String
  , _actCompleted :: Maybe Bool
  , _actOrder :: Maybe Int
  } deriving Show

instance FromJSON TodoAction where
  parseJSON (Object o) = TodoAction
                         <$> o .:? "title"
                         <*> o .:? "completed"
                         <*> o .:? "order"
  parseJSON _ = mzero

instance ToJSON TodoAction where
  toJSON (TodoAction mTitle mCompl mOrder) = noNullsObject
      [ "title"     .= mTitle
      , "completed" .= mCompl
      , "order"     .= mOrder
      ]
    where
      noNullsObject = object . filter notNull
      notNull (_, Null) = False
      notNull _         = True

actionToTodo :: TodoAction -> Todo
actionToTodo (TodoAction mTitle mCompleted mOrder) = Todo title completed order
  where
    title     = fromMaybe "" mTitle
    completed = fromMaybe False mCompleted
    order     = fromMaybe 0 mOrder

allowCors :: Middleware
allowCors = addHeaders [
    ("Access-Control-Allow-Origin", "*"),
    ("Access-Control-Allow-Headers", "Accept, Content-Type"),
    ("Access-Control-Allow-Methods", "GET, HEAD, POST, DELETE, OPTIONS, PUT, PATCH")
  ]

runDb :: Sqlite.SqlPersistT (ResourceT (NoLoggingT IO)) a -> IO a
runDb = runNoLoggingT . runResourceT . Sqlite.withSqliteConn "dev.sqlite3" . Sqlite.runSqlConn


main :: IO ()
main = do
  runDb $ Sqlite.runMigration migrateAll
  port <- read <$> getEnv "PORT"
  scotty port $ do
    middleware allowCors
    get "/todos" $ do
      todos <- liftIO readTodos
      json todos
    get "/todos/:id" $ do
      pid <- param "id"
      actionOr404 pid (\tid -> do
                todo <- liftIO $ readTodo tid
                json todo)
    patch "/todos/:id" $ do
      pid <- param "id"
      actionOr404 pid (\tid -> do
                          todoAct <- jsonData
                          let todo = actionToTodo todoAct
                          liftIO $ replaceTodo tid todo
                          json todoAct)
    delete "/todos/:id" $ do
      pid <- param "id"
      actionOr404 pid (liftIO . deleteTodo)
    post "/todos" $ do
      todoAct <- jsonData
      let todo = actionToTodo todoAct
      tid <- liftIO $ insertTodo todo
      todo' <- liftIO $ readTodo tid
      json todo'
    delete "/todos" $ liftIO $ runDb $ DB.deleteWhere ([] :: [Sqlite.Filter Todo])
    matchAny "/todos" $ text "ok"
  where
    readTodos :: IO [Sqlite.Entity Todo]
    readTodos =  runDb $ DB.selectList [] []

    readTodo :: Sqlite.Key Todo -> IO (Maybe Todo)
    readTodo tid = runDb $ DB.get tid

    replaceTodo :: Sqlite.Key Todo -> Todo -> IO ()
    replaceTodo tid todo = runDb $ DB.replace tid todo

    deleteTodo :: Sqlite.Key Todo -> IO ()
    deleteTodo tid = runDb $ DB.delete tid

    insertTodo :: Todo -> IO (Sqlite.Key Todo)
    insertTodo todo = runDb $ DB.insert todo

    actionOr404 pid action = case fromPathPiece pid of
            Nothing -> status status404
            Just tid -> action tid
