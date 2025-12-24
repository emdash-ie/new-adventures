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
import GHC.Generics
import Network.Wai.Handler.Warp (run)
import Servant
import System.Directory (listDirectory)
import System.Environment (getArgs)

main :: IO ()
main = do
  dir : _ <- getArgs
  filePaths <- listDirectory dir
  let orgFilePaths = filter (isSuffixOf ".org") filePaths
  orgFiles <- fmap (Map.fromList . catMaybes) $ for orgFilePaths \f -> do
    bs <- readFile f
    return (fmap (Text.pack f,) (org (decodeUtf8 bs)))
  run 8014 (app orgFiles)

type ToDoAPI = "todos" :> Get '[JSON] (Map Text OrgFile)

server :: Map Text OrgFile -> Server ToDoAPI
server = return

todoAPI :: Proxy ToDoAPI
todoAPI = Proxy

app :: Map Text OrgFile -> Application
app = serve todoAPI . server

exampleTodos :: Map Text OrgFile
exampleTodos = Map.singleton "test.org" (OrgFile Map.empty (OrgDoc [Paragraph (Plain "hello" :| [])] []))

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

