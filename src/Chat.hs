module Chat (chat) where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Data.IORef
import           Data.List          (isPrefixOf)
import           Network
import           Parser             (readCommand)
import           System.Exit        (ExitCode (ExitSuccess), exitWith)
import           System.IO

import           Data

printChatServerRef :: ChatServerRef -> Handle -> IO ()
printChatServerRef chatServerRef h = do
  allClients <- readIORef chatServerRef
  let clientsNumber = length allClients
      clientIds = fmap fst allClients
  hPutStrLn h $ show clientsNumber ++ " clients are currently connected"
  hPutStrLn h $ "Client Ids: " ++ show clientIds

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

showMessage :: ClientId -> Message -> String
showMessage clientId message = show clientId ++ ": " ++ message

broadCastMessage :: ChatServerRef -> ClientId -> Message -> IO ()
broadCastMessage chatServerRef from msg = do
  clientIds <- fmap fst <$> readIORef chatServerRef
  sendMessageTo chatServerRef from clientIds msg

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
  exitWith ExitSuccess

-- TODO: use Haskeline instead
handleClientInput :: ChatServerRef -> ClientId -> Handle -> IO ()
handleClientInput chatServerRef clientId h = forever $ do
  input <- hGetLine h
  -- TODO: add proper logging
  putStrLn "Logging" >> putStrLn input

  if (isPrefixOf "/" input)
  then handleCommand chatServerRef clientId h input
  else broadCastMessage chatServerRef clientId input

performCommand :: ChatServerRef -> ClientId -> Handle -> ChatCommand -> IO ()
performCommand chatServerRef from h command =
  case command of
    Help          -> help h
    Join chatroom -> hPutStrLn h ("Joining chatroom: " ++ show chatroom)
    Msg to msg    -> sendMessageTo chatServerRef from [to] msg
    Quit          -> clientExit chatServerRef from h
    Who           -> printChatServerRef chatServerRef h
    Whoami        -> hPutStrLn h $ "Client id: " ++ show from

-- TODO: move this to a config file
commandHelp :: [(String, String)]
commandHelp =
  [ ("/help",                 "displays this help menu")
  , ("/join <chatroom>",      "joins the chatroom <chatroom>")
  , ("/msg <clientId> <msg>", "sends a private message <msg> to client <clientId>")
  , ("/quit",                 "exits the chat server")
  , ("/who",                  "displays who is in the current chatroom")
  , ("/whoami",               "displays your clientid")
  ]

help :: Handle -> IO ()
help h = do
  hPutStrLn h "HaskChat help menu"
  hPutStrLn h "------------------"
  hPutStrLn h ""
  mapM_ (\(cmd, dsc) -> hPutStrLn h (cmd ++ "\t\t" ++ dsc) ) commandHelp

sendMessageTo :: ChatServerRef -> ClientId -> [ClientId] -> Message -> IO ()
sendMessageTo chatServerRef fromId toIds msg = do
  allClients <- readIORef chatServerRef
  let toClients = filter (\(cid, _) -> cid `elem` toIds) allClients
  forM_ toClients $ \(_, h) -> hPutStrLn h (showMessage fromId msg)

handleCommand :: ChatServerRef -> ClientId -> Handle -> String -> IO ()
handleCommand chatServerRef clientId h input =
  if (isPrefixOf "/" input)
  then
    case readCommand input of
      Left _        -> hPutStrLn h "Cannot understand command"
      Right command -> performCommand chatServerRef clientId h command
  else return ()

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
