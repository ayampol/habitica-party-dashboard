{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (MVar, modifyMVar, modifyMVar_, newMVar, readMVar)
import Control.Exception (finally)
import Control.Monad (forM_, forever)
import Data.Char (isPunctuation, isSpace)
import Data.Monoid (mappend)
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.IO as T
import qualified Network.WebSockets as WS

-- Start off by accepting the connection
-- Perhaps check the path and headers to be sure?
application :: MVar ServerState -> WS.ServerApp
application state pending = do
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30
  msg <- WS.receiveData conn
  clients <- readMVar state
  case msg of
    _
      | not (prefix `T.isPrefixOf` msg) ->
        WS.sendTextData conn ("Bad announcement" :: Text)
      | any ($ fst client) [T.null, T.any isPunctuation, T.any isSpace] ->
        WS.sendTextData conn ("Bad username" :: Text)
      | clientExists client clients ->
        WS.sendTextData conn ("User already exists" :: Text)
      | otherwise ->
        flip finally disconnect $ do
          modifyMVar_ state $ \s -> do
            let s' = addClient client s
            WS.sendTextData conn $
              "User: " `mappend` T.intercalate "," (map fst s)
            broadcast (fst client `mappend` "joined") s'
            return s'
          talk client state
      where prefix = "UserID: "
            client = (T.drop (T.length prefix) msg, conn)
            disconnect
                    -- Remove client and return new state
             = do
              s <-
                modifyMVar state $ \s ->
                  let s' = removeClient client s
                   in return (s', s')
              broadcast (fst client `mappend` "disconnected") s

main :: IO ()
main = do
  state <- newMVar newServerState
  WS.runServer "127.0.0.1" 9876 $ application state

-- Represent a client by their UserID 
type Client = (Text, WS.Connection)

-- The state of the server is a list of connected clients. A
-- All messages come in through the webhook.
type ServerState = [Client]

-- Initialize server state to empty list. 
newServerState :: ServerState
newServerState = []

numClients :: ServerState -> Int
numClients = length

-- Check if the user has already joined (do not allow multi-tab usage)
clientExists :: Client -> ServerState -> Bool
clientExists client = any ((== fst client) . fst)

-- Add a client, assumes they do not already exist
addClient :: Client -> ServerState -> ServerState
addClient client clients = client : clients

-- Remove a client 
removeClient :: Client -> ServerState -> ServerState
removeClient client = filter ((/= fst client) . fst)

-- Send a message to all clients and log it on stdout
broadcast :: Text -> ServerState -> IO ()
broadcast message clients = do
  T.putStrLn message
  forM_ clients $ \(_, conn) -> WS.sendTextData conn message

talk :: Client -> MVar ServerState -> IO ()
talk (user, conn) state =
  forever $ do
    msg <- WS.receiveData conn
    readMVar state >>= broadcast (user `mappend` ": " `mappend` msg)
