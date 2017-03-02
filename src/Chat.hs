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

-- TODO: restructure file in multiple files
-- TODO: properly test chatServer

-- Infrastructure
-- 1 thread that will register/close new clients
-- 1 thread that will listen to all clients
-- 1 thread that will push messages to clients

handleClients :: ChatServerRef -> Socket -> IO ()
handleClients chatServerRef socket =
  bracket (accept socket) (\(h, _, _) ->  hClose h) $
    \(h1, _, _) -> do
      chatGreeting h1
      clientId <- registerClient chatServerRef h1
      broadCastChatRoomMessage chatServerRef clientId defaultChatRoom (userJoinMessage defaultChatRoom clientId)
      clients <- chatRoomClients chatServerRef defaultChatRoom
      hPutStrLn h1 $ showChatRoom defaultChatRoom clients
      -- TODO: clean up resources
      -- Kill Thread
      -- Better handling of resources
      _ <- forkIO $ handleClientInput chatServerRef clientId h1
      handleClients chatServerRef socket

-- TODO: use Haskeline instead
handleClientInput :: ChatServerRef -> ClientId -> Handle -> IO ()
handleClientInput chatServerRef clientId h = forever $ do
  input <- hGetLine h
  -- TODO: add proper logging
  putStrLn "Logging" >> putStrLn input

  if (isPrefixOf "/" input)
  then handleCommand chatServerRef clientId h input
  else do
    chatRoom <- clientIdChatRoom clientId <$> readIORef chatServerRef
    broadCastChatRoomMessage chatServerRef clientId chatRoom input

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

handleCommand :: ChatServerRef -> ClientId -> Handle -> String -> IO ()
handleCommand chatServerRef clientId h input =
  if (isPrefixOf "/" input)
  then
    case readCommand input of
      Left err      -> hPutStrLn h (show err)
      Right command -> performCommand chatServerRef clientId h command
  else return ()

chatGreeting :: Handle -> IO ()
chatGreeting h = do
  logo <- readFile "logo.txt"
  hPutStrLn h logo

-- | Chat server entry point
chat :: PortID -> IO ()
chat chatPortNumber = do
  chatServerRef <- emptyChatServerRef
  bracket (listenOn chatPortNumber) sClose (handleClients chatServerRef)
