module Chat (chat) where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Data.IORef
import           Data.List          (find, isPrefixOf)
import           Data.Maybe         (fromMaybe)
import           Network
import           Parser             (readCommand)
import           System.Exit        (ExitCode (ExitSuccess), exitWith)
import           System.IO

import           Data

-- TODO: restructure file in multiple files
-- TODO: properly test chatServer

printChatServerRef :: ChatServerRef -> Handle -> IO ()
printChatServerRef chatServerRef h = do
  allClients <- readIORef chatServerRef
  let clientsNumber = length allClients
      clientIds = fmap (\(cid, _, _) -> cid) allClients
  hPutStrLn h $ show clientsNumber ++ " clients are currently connected"
  hPutStrLn h $ "Client Ids: " ++ show clientIds

-- TODO: display info when joining chatRoom
chatRoomInfo :: ChatRoom -> Clients -> String
chatRoomInfo chatRoom clients =
  show (length cids) ++ " clients are in chatroom " ++ show chatRoom
  where cids = filter (\(_, cr, _) -> cr == chatRoom) clients

nextClientId :: Clients -> ClientId
nextClientId []      = 0
nextClientId clients = 1 + (maximum $ fmap (\(cid, _, _) -> cid) clients)

addClient :: Handle -> Clients -> (Clients, ClientId)
addClient h clients = ((clientId, defaultChatRoom, h) : clients, clientId)
  where clientId = nextClientId clients

removeClient :: ClientId -> Clients -> Clients
removeClient clientId = filter (\(cid, _, _) -> cid /= clientId)

registerClient :: ChatServerRef -> Handle -> IO ClientId
registerClient chatServerRef h = atomicModifyIORef chatServerRef (addClient h)

changeChatRoom :: ChatRoom -> ClientId -> Clients -> Clients
changeChatRoom chatRoom clientId =
  fmap (\(cid, cr, h) ->
    if cid == clientId
    then (cid, chatRoom, h)
    else (cid, cr, h))

userJoinMessage :: ChatRoom -> ClientId -> Message
userJoinMessage chatRoom clientId =
  show clientId ++ " has joined chatroom " ++ show chatRoom

-- Infrastructure
-- 1 thread that will register/close new clients
-- 1 thread that will listen to all clients
-- 1 thread that will push messages to clients

showMessage :: ClientId -> Message -> String
showMessage clientId message = show clientId ++ ": " ++ message

chatRoomClients :: ChatServerRef -> ChatRoom -> IO Clients
chatRoomClients chatServerRef chatRoom =
    filter (\(_, cr, _) -> cr == chatRoom) <$> readIORef chatServerRef

chatRoomClientIds :: ChatServerRef -> ChatRoom -> IO [ClientId]
chatRoomClientIds chatServerRef chatRoom =
  fmap (\(cid, _, _) -> cid) <$> chatRoomClients chatServerRef chatRoom

clientIdChatRoom :: ClientId -> Clients -> ChatRoom
clientIdChatRoom clientId =
  (fromMaybe defaultChatRoom) . fmap (\(_, chatRoom, _) -> chatRoom) . find (\(cid, _, _) -> cid == clientId)

broadCastChatRoomMessage :: ChatServerRef -> ClientId -> ChatRoom -> Message -> IO ()
broadCastChatRoomMessage chatServerRef from chatRoom msg = do
  clientIds <- chatRoomClientIds chatServerRef chatRoom
  sendMessageTo chatServerRef from clientIds msg

handleClients :: ChatServerRef -> Socket -> IO ()
handleClients chatServerRef socket =
  bracket (accept socket) (\(h, _, _) ->  hClose h) $
    \(h1, _, _) -> do
      chatGreeting h1
      printChatServerRef chatServerRef h1
      clientId <- registerClient chatServerRef h1
      broadCastChatRoomMessage chatServerRef clientId defaultChatRoom (userJoinMessage defaultChatRoom clientId)
      clients <- chatRoomClients chatServerRef defaultChatRoom
      hPutStrLn h1 $ chatRoomInfo defaultChatRoom clients
      -- TODO: clean up resources
      -- Kill Thread
      -- Better handling of resources
      _ <- forkIO $ handleClientInput chatServerRef clientId h1
      handleClients chatServerRef socket

clientExitMessage :: ChatRoom -> ClientId -> Message
clientExitMessage chatRoom clientId =
  show clientId ++ " has left chatroom " ++ show chatRoom

clientExit :: ChatServerRef -> ClientId -> Handle -> IO ()
clientExit chatServerRef clientId h = do
  clients <- readIORef chatServerRef
  let chatRoom = clientIdChatRoom clientId clients
  modifyIORef chatServerRef (removeClient clientId)
  broadCastChatRoomMessage chatServerRef clientId chatRoom (clientExitMessage chatRoom clientId)
  (hPutStrLn h "See you soon...")
  hClose h
  exitWith ExitSuccess

joinChatRoomCommand :: ChatServerRef -> ClientId -> ChatRoom -> IO ()
joinChatRoomCommand chatServerRef clientId chatRoom = do
  modifyIORef chatServerRef (changeChatRoom chatRoom clientId)
  broadCastChatRoomMessage chatServerRef clientId chatRoom (userJoinMessage defaultChatRoom clientId)

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
    ChatRooms     -> hPutStrLn h showChatRooms
    Help          -> hPutStrLn h "" >> help h >> hPutStrLn h ""
    Join chatRoom -> hPutStrLn h ("joining chatroom " ++ show chatRoom) >> joinChatRoomCommand chatServerRef from chatRoom
    Msg to msg    -> sendMessageTo chatServerRef from [to] msg
    Quit          -> clientExit chatServerRef from h
    Who           -> printChatServerRef chatServerRef h
    Whoami        -> hPutStrLn h $ "Client id: " ++ show from

showChatRooms :: String
showChatRooms = "default, haskell"

-- TODO: move this to a config file
commandHelp :: [(String, String)]
commandHelp =
  [ ("/chatrooms",            "displays the list of all chatrooms")
  , ("/help",                 "displays this help menu")
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
  let toClients = filter (\(cid, _, _) -> cid `elem` toIds) allClients
  forM_ toClients $ \(_, _, h) -> hPutStrLn h (showMessage fromId msg)

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
chat :: IO ()
chat = do
  chatServerRef <- emptyChatServerRef
  chatPortNumber <- return 1234  -- TODO read from ENV
  bracket (listenOn (PortNumber chatPortNumber)) sClose (handleClients chatServerRef)
