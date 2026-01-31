{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Prelude hiding (readFile, writeFile)
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON(..), ToJSON(..), (.=), object)
import Data.ByteString (readFile, writeFile)
import Data.Foldable (for_)
import Data.List (isSuffixOf)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (catMaybes)
import Data.Org
import Data.Traversable (for)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Text.IO qualified as Text
import GHC.Conc
import GHC.Generics
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.JS
import System.Directory (listDirectory)
import System.Environment (getArgs)
import System.FilePath ((</>))

main :: IO ()
main = do
  command : args <- getArgs
  case command of
    "serve-todos" -> do
      let dir : staticPath : _ = args
      orgFiles <- newTVarIO Map.empty
      _pollerThreadId <- pollFromDisk dir orgFiles
      run 8014 (app orgFiles staticPath dir)
    "generate-api-javascript" -> do
      let targetFile : _ = args
      Text.writeFile targetFile apiJavascript
    c -> do
      putStrLn ("Unrecognised command: " <> c)

type ToDoAPI = "todos" :> (
    Get '[JSON] (Map Text OrgFile)
    :<|> ("add" :> ReqBody '[JSON] NewTodo :> Post '[JSON] ())
  )
type API = ToDoAPI :<|> Raw

todoServer :: FilePath -> TVar (Map Text OrgFile) -> Server ToDoAPI
todoServer todoDirectory orgFiles =
  readTodos orgFiles
  :<|> addTodo todoDirectory orgFiles

readTodos :: TVar (Map Text OrgFile) -> Handler (Map Text OrgFile)
readTodos orgFiles = do
  liftIO (atomically (readTVar orgFiles))

data NewTodo = NewTodo
  { contents :: Section
  , file :: Text
  } deriving (Show, Eq, Generic)

instance FromJSON NewTodo
instance ToJSON NewTodo

addTodo :: FilePath -> TVar (Map Text OrgFile) -> NewTodo -> Handler ()
addTodo todoDirectory orgFiles NewTodo{file, contents} = do
  liftIO do
    newFiles <- atomically do
      fs <- readTVar orgFiles
      let update = \case
            Nothing -> Just (OrgFile {
                                orgMeta = Map.empty,
                                orgDoc = OrgDoc {
                                    docBlocks = [],
                                    docSections = [contents]
                                  }
                             })
            Just OrgFile{orgMeta, orgDoc = OrgDoc{docBlocks, docSections}} ->
              Just OrgFile{orgMeta, orgDoc = OrgDoc{docBlocks, docSections = docSections <> [contents]}}
      let newFiles = Map.alter update file fs
      writeTVar orgFiles newFiles
      return newFiles
    writeFilesToDisk todoDirectory newFiles

server :: TVar (Map Text OrgFile) -> FilePath -> FilePath -> Server API
server files staticPath todoDirectory = todoServer todoDirectory files :<|> serveDirectoryFileServer staticPath

todoAPI :: Proxy ToDoAPI
todoAPI = Proxy

api :: Proxy API
api = Proxy

app :: TVar (Map Text OrgFile) -> FilePath -> FilePath -> Application
app files staticPath todoDirectory = serve api (server files staticPath todoDirectory)

pollFromDisk :: FilePath -> TVar (Map Text OrgFile) -> IO ThreadId
pollFromDisk dir orgFiles = forkIO $ forever $ do
  fs <- readFilesFromDisk dir
  _ <- atomically (writeTVar orgFiles fs)
  threadDelay 10_000_000

readFilesFromDisk :: FilePath -> IO (Map Text OrgFile)
readFilesFromDisk dir = do
  filePaths <- listDirectory dir
  let orgFilePaths = filter (isSuffixOf ".org") filePaths
  fmap (Map.fromList . catMaybes) $ for orgFilePaths \f -> do
    bs <- readFile (dir </> f)
    return (fmap (Text.pack f,) (org (decodeUtf8 bs)))

writeFilesToDisk :: FilePath -> Map Text OrgFile -> IO ()
writeFilesToDisk dir files =
  for_ (Map.toList files) \(f, org) ->
    writeFile (dir </> Text.unpack f) (encodeUtf8 (prettyOrgFile org))

apiJavascript :: Text
apiJavascript = jsForAPI todoAPI vanillaJS

instance ToJSON OrgFile
instance ToJSON OrgDoc
instance ToJSON Section
deriving instance Generic OrgDateTime
instance ToJSON OrgDateTime where
instance ToJSON Block
instance ToJSON Words
instance ToJSON Language
instance ToJSON URL
instance ToJSON Priority
instance ToJSON ListItems
instance ToJSON Row
instance ToJSON ListType
instance ToJSON Item
instance ToJSON Column
instance ToJSON Todo
deriving instance Generic OrgTime
instance ToJSON OrgTime where
deriving instance Generic Repeater
instance ToJSON Repeater where
deriving instance Generic Delay
instance ToJSON Delay
deriving instance Generic Interval
instance ToJSON Interval
deriving instance Generic RepeatMode
instance ToJSON RepeatMode
deriving instance Generic DelayMode
instance ToJSON DelayMode

instance FromJSON OrgFile
instance FromJSON OrgDoc
instance FromJSON Section
instance FromJSON OrgDateTime where
instance FromJSON Block
instance FromJSON Words
instance FromJSON Language
instance FromJSON URL
instance FromJSON Priority
instance FromJSON ListItems
instance FromJSON Row
instance FromJSON ListType
instance FromJSON Item
instance FromJSON Column
instance FromJSON Todo
instance FromJSON OrgTime where
instance FromJSON Repeater where
instance FromJSON Delay
instance FromJSON Interval
instance FromJSON RepeatMode
instance FromJSON DelayMode
