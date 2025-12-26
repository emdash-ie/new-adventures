{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Prelude hiding (readFile)
import Data.Aeson (ToJSON(..), (.=), object)
import Data.ByteString (readFile)
import Data.List (isSuffixOf)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (catMaybes)
import Data.Org
import Data.Traversable (for)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8)
import Data.Text.IO qualified as Text
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
      filePaths <- listDirectory dir
      let orgFilePaths = filter (isSuffixOf ".org") filePaths
      orgFiles <- fmap (Map.fromList . catMaybes) $ for orgFilePaths \f -> do
        bs <- readFile (dir </> f)
        return (fmap (Text.pack f,) (org (decodeUtf8 bs)))
      run 8014 (app orgFiles staticPath)
    "generate-api-javascript" -> do
      let targetFile : _ = args
      Text.writeFile targetFile apiJavascript
    c -> do
      putStrLn ("Unrecognised command: " <> c)

type ToDoAPI = "todos" :> Get '[JSON] (Map Text OrgFile)
type API = ToDoAPI :<|> Raw

todoServer :: Map Text OrgFile -> Server ToDoAPI
todoServer = return

server :: Map Text OrgFile -> FilePath -> Server API
server files staticPath = todoServer files :<|> serveDirectoryFileServer staticPath

todoAPI :: Proxy ToDoAPI
todoAPI = Proxy

api :: Proxy API
api = Proxy

app :: Map Text OrgFile -> FilePath -> Application
app files staticPath = serve api (server files staticPath)

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

