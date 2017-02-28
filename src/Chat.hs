module Chat (chat) where

import Control.Concurrent
-- import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
-- import Data.Typeable
import Network
import System.IO
import Data.IORef
import Data.List (isPrefixOf)
import System.Exit (exitWith, ExitCode(ExitSuccess))

type ClientId = Integer
type Clients = [(ClientId, Handle)]
type ChatServerRef = IORef Clients
type Message = String

printChatServerRef :: ChatServerRef -> Handle -> IO ()
printChatServerRef chatServerRef h = join $ ((hPutStrLn h) . show) <$> (readIORef chatServerRef)

nextClientId :: Clients -> ClientId
nextClientId []      = 0
nextClientId clients = 1 + (maximum $ fmap fst clients)

addClient :: Handle -> Clients -> (Clients, ClientId)
addClient h clients = ((clientId, h) : clients, clientId)
  where clientId = nextClientId clients

removeClient :: ClientId -> Clients -> Clients
removeClient clientId = filter (\(cid, _) -> cid /= clientId)

registerClient :: ChatServerRef -> Handle -> IO ClientId
registerClient chatServerRef h = atomicModifyIORef chatServerRef (addClient h)

newChatServerRef :: IO ChatServerRef
newChatServerRef = newIORef []

-- Infrastructure
-- 1 thread that will register/close new clients
-- 1 thread that will listen to all clients
-- 1 thread that will push messages to clients

formatMessage :: ClientId -> Message -> Message
formatMessage clientId message = show clientId ++ ": " ++ message

publishMessage :: ChatServerRef -> ClientId -> Message -> IO ()
publishMessage chatServerRef from message = do
  allClients <- readIORef chatServerRef
  let targetClients = allClients  -- filter (\(clientId, _) -> (clientId /= from)) allClients
  forM_ targetClients $ \(_, h) -> hPutStrLn h (formatMessage from message)

handleClients :: ChatServerRef -> Socket -> IO ()
handleClients chatServerRef socket =
  bracket (accept socket) (\(h, _, _) ->  hClose h) $
    \(h1, _, _) -> do
      chatGreeting h1
      printChatServerRef chatServerRef h1
      clientId <- registerClient chatServerRef h1
      printChatServerRef chatServerRef stdout
      -- TODO: clean up resources
      -- Kill Thread
      -- Better handling of resources
      _ <- forkIO $ handleClientInput chatServerRef clientId h1
      handleClients chatServerRef socket

clientExit :: ChatServerRef -> ClientId -> Handle -> IO ()
clientExit chatServerRef clientId h = do
  modifyIORef chatServerRef (removeClient clientId)
  (hPutStrLn h "See you soon...")
  hClose h

-- TODO: use Haskeline instead
handleClientInput :: ChatServerRef -> ClientId -> Handle -> IO ()
handleClientInput chatServerRef clientId h = forever $ do
  input <- hGetLine h
  -- TODO: add proper logging
  putStrLn "Logging" >> putStrLn input
  if (isPrefixOf ":q" input)
  then clientExit chatServerRef clientId h >> exitWith ExitSuccess
  else publishMessage chatServerRef clientId input

chatGreeting :: Handle -> IO ()
chatGreeting h = do
  logo <- readFile "logo.txt"
  hPutStrLn h logo

-- | Chat server entry point
chat :: IO ()
chat = do
  chatServerRef <- newChatServerRef
  chatPortNumber <- return 1234  -- TODO read from ENV
  bracket (listenOn (PortNumber chatPortNumber)) sClose (handleClients chatServerRef)
