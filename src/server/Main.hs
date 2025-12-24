{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Servant
import Data.Aeson (ToJSON(..), (.=), object)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Org
import Data.Text
import GHC.Generics
import Network.Wai.Handler.Warp (run)

type ToDoAPI = "todos" :> Get '[JSON] (Map Text OrgFile)

server :: Server ToDoAPI
server = return exampleTodos

todoAPI :: Proxy ToDoAPI
todoAPI = Proxy

app :: Application
app = serve todoAPI server

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


main :: IO ()
main = run 8014 app
