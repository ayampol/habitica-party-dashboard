{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Control.Concurrent as Concurrent
import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import Data.Aeson
import Data.Aeson.Types
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import Debug.Trace
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as WS
import qualified Network.WebSockets as WS
import qualified Safe

main :: IO ()
main = do
  state <- Concurrent.newMVar []
  putStrLn "Running WS server..."
  Warp.run 3000 $
    WS.websocketsOr
      WS.defaultConnectionOptions
      (wsApp state)
      (trace "response received" httpApp state)

httpApp :: Concurrent.MVar State -> Wai.Application
httpApp state request respond =
  case Wai.requestMethod request of
    "POST" -> do
      broadcastHabitica state "bleh"
      respond (traceShow request Wai.responseLBS Http.status200 [] "")
    _ -> respond $ Wai.responseLBS Http.status400 [] "Not a websocket request"

type ClientId = Int

type Client = (ClientId, WS.Connection)

type State = [Client]

type Chat = [ChatMsg]

testMsg :: Text.Text -> ChatMsg
testMsg txt = ChatMsg "Me" ("Some message I wrote, there's words wow " <> txt)

data Env = Env
  { envMessageQueue :: [ChatMsg]
  , clients :: [Client]
  }

data ChatMsg = ChatMsg
  { author :: Text.Text
  , text :: Text.Text
  } deriving (Show)

parseChatMsg :: Value -> Parser ChatMsg
parseChatMsg _ = fail "expected data"

instance FromJSON ChatMsg where
  parseJson =
    withObject "ChatMsg" $ \v ->
      ChatMsg <$> ((v .: "chat") >>= (.: "user")) <*>
      ((v .: "chat") >>= (.: "text"))

nextId :: State -> ClientId
nextId = Maybe.maybe 0 ((+) 1) . Safe.maximumMay . List.map fst

connectClient :: WS.Connection -> Concurrent.MVar State -> IO ClientId
connectClient conn stateRef =
  Concurrent.modifyMVar stateRef $ \state -> do
    let clientId = nextId state
    return ((clientId, conn) : state, clientId)

withoutClient :: ClientId -> State -> State
withoutClient clientId = List.filter ((/=) clientId . fst)

disconnectClient :: ClientId -> Concurrent.MVar State -> IO ()
disconnectClient clientId stateRef =
  Concurrent.modifyMVar_ stateRef $ \state ->
    return $ withoutClient clientId state

listen :: WS.Connection -> ClientId -> Concurrent.MVar State -> IO ()
listen conn clientId stateRef =
  Monad.forever $ do WS.receiveData conn >>= broadcast clientId stateRef

broadcastHabitica :: Concurrent.MVar State -> Text.Text -> IO ()
broadcastHabitica stateRef msg = do
  clients <- Concurrent.readMVar stateRef
  Monad.forM_ clients $ \(_, conn) -> WS.sendTextData conn msg

broadcast :: ClientId -> Concurrent.MVar State -> Text.Text -> IO ()
broadcast clientId stateRef msg = do
  clients <- Concurrent.readMVar stateRef
  let otherClients = withoutClient clientId clients
  Monad.forM_ otherClients $ \(_, conn) -> WS.sendTextData conn msg

wsApp :: Concurrent.MVar State -> WS.ServerApp
wsApp stateRef pendingConn = do
  conn <- WS.acceptRequest pendingConn
  clientId <- connectClient conn stateRef
  WS.forkPingThread conn 30
  Exception.finally
    (listen conn clientId stateRef)
    (disconnectClient clientId stateRef)
