module Chat (chat) where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Data.IORef
import           Data.List          (isPrefixOf)
import           Network
import           Parser             (readCommand)
import           System.IO

import           Command
import           Data
import qualified Logger as L
import           System.Log.FastLogger (LoggerSet)

-- TODO: restructure file in multiple files
-- TODO: properly test chatServer

-- Infrastructure
-- 1 thread that will register/close new clients
-- 1 thread that will listen to all clients
-- 1 thread that will push messages to clients

handleClients :: LoggerSet -> ChatServerRef -> Socket -> IO ()
handleClients loggerSet chatServerRef socket =
  bracket (accept socket) (\(h, _, _) ->  hClose h) $
    \(h1, _, _) -> do
      chatGreeting h1
      clientId <- registerClient chatServerRef h1
      L.log loggerSet $ L.formatBroadcastedMessage clientId defaultChatRoom (userJoinMessage defaultChatRoom clientId)
      broadCastChatRoomMessage chatServerRef clientId defaultChatRoom (userJoinMessage defaultChatRoom clientId)
      clients <- chatRoomClients chatServerRef defaultChatRoom
      hPutStrLn h1 $ showChatRoom defaultChatRoom clients
      -- TODO: clean up resources
      -- Kill Thread
      -- Better handling of resources
      _ <- forkIO $ handleClientInput loggerSet chatServerRef clientId h1
      handleClients loggerSet chatServerRef socket

-- TODO: use Haskeline instead
handleClientInput :: LoggerSet -> ChatServerRef -> ClientId -> Handle -> IO ()
handleClientInput loggerSet chatServerRef clientId h = forever $ do
  input <- hGetLine h
  chatRoom <- clientIdChatRoom clientId <$> readIORef chatServerRef
  L.log loggerSet $ L.formatClientInput clientId chatRoom input
  if (isPrefixOf "/" input)
  then handleCommand loggerSet chatServerRef clientId h input
  else do
    broadCastChatRoomMessage chatServerRef clientId chatRoom input
    L.log loggerSet $ L.formatBroadcastedMessage clientId chatRoom input

performCommand :: ChatServerRef -> ClientId -> Handle -> ChatCommand -> IO ()
performCommand chatServerRef from h command =
  case command of
    ChatRooms     -> chatRoomsCommand h
    Help          -> helpCommand h
    Join chatRoom -> joinChatRoomCommand chatServerRef from h chatRoom
    Msg to msg    -> messageCommand chatServerRef from to msg
    Quit          -> quitCommand chatServerRef from h
    Who           -> whoCommand chatServerRef h
    Whoami        -> whoAmiCommand h from

handleCommand :: LoggerSet -> ChatServerRef -> ClientId -> Handle -> String -> IO ()
handleCommand loggerSet chatServerRef clientId h input = do
  chatRoom <- clientIdChatRoom clientId <$> readIORef chatServerRef
  if (isPrefixOf "/" input)
  then
    case readCommand input of
      Left err      -> do
        hPutStrLn h (show err)
        L.log loggerSet (L.formatCommandError clientId chatRoom err)
      Right command -> do
        performCommand chatServerRef clientId h command
        L.log loggerSet (L.formatCommandPerformed clientId chatRoom command)
  else return ()

chatGreeting :: Handle -> IO ()
chatGreeting h = do
  logo <- readFile "logo.txt"
  hPutStrLn h logo

-- | Chat server entry point
chat :: LoggerSet -> PortID -> IO ()
chat loggerSet chatPortNumber = do
  chatServerRef <- emptyChatServerRef
  bracket (listenOn chatPortNumber) sClose (handleClients loggerSet chatServerRef)
