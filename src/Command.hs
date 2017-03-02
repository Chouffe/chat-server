module Command
  ( chatRoomsCommand
  , helpCommand
  , joinChatRoomCommand
  , messageCommand
  , quitCommand
  , whoCommand
  , whoAmiCommand

  -- TODO: move to somewhere else?
  , broadCastChatRoomMessage
  , chatRoomClients
  )
  where

import           Control.Monad
import           Data.IORef
import           Data
import           System.Exit        (ExitCode (ExitSuccess), exitWith)
import           System.IO

-- TODO: move to Data.API?
sendMessageTo :: ChatServerRef -> ClientId -> [ClientId] -> Message -> IO ()
sendMessageTo chatServerRef fromId toIds msg = do
  allClients <- readIORef chatServerRef
  let toClients = filter (\(cid, _, _) -> cid `elem` toIds) allClients
  forM_ toClients $ \(_, _, h) -> hPutStrLn h (showMessage fromId msg)

chatRoomClients :: ChatServerRef -> ChatRoom -> IO Clients
chatRoomClients chatServerRef chatRoom =
    filter (\(_, cr, _) -> cr == chatRoom) <$> readIORef chatServerRef

chatRoomClientIds :: ChatServerRef -> ChatRoom -> IO [ClientId]
chatRoomClientIds chatServerRef chatRoom =
  fmap (\(cid, _, _) -> cid) <$> chatRoomClients chatServerRef chatRoom

broadCastChatRoomMessage :: ChatServerRef -> ClientId -> ChatRoom -> Message -> IO ()
broadCastChatRoomMessage chatServerRef from chatRoom msg = do
  clientIds <- chatRoomClientIds chatServerRef chatRoom
  sendMessageTo chatServerRef from clientIds msg

commandHelpData :: [(String, String)]
commandHelpData =
  [ ("/chatrooms",            "displays the list of all chatrooms")
  , ("/help",                 "displays this help menu")
  , ("/join <chatroom>",      "joins the chatroom <chatroom>")
  , ("/msg <clientId> <msg>", "sends a private message <msg> to client <clientId>")
  , ("/quit",                 "exits the chat server")
  , ("/who",                  "displays who is in the current chatroom")
  , ("/whoami",               "displays your clientid")
  ]

-- Commands

joinChatRoomCommand :: ChatServerRef -> ClientId -> Handle -> ChatRoom -> IO ()
joinChatRoomCommand chatServerRef clientId h chatRoom = do
  hPutStrLn h ("joining chatroom " ++ show chatRoom)
  modifyIORef chatServerRef (changeChatRoom chatRoom clientId)
  broadCastChatRoomMessage chatServerRef clientId chatRoom (userJoinMessage defaultChatRoom clientId)

messageCommand :: ChatServerRef -> ClientId -> ClientId -> Message -> IO ()
messageCommand chatServerRef from to msg = sendMessageTo chatServerRef from [to] msg

chatRoomsCommand :: Handle -> IO ()
chatRoomsCommand h = hPutStrLn h showChatRooms

quitCommand :: ChatServerRef -> ClientId -> Handle -> IO ()
quitCommand chatServerRef clientId h = do
  clients <- readIORef chatServerRef
  let chatRoom = clientIdChatRoom clientId clients
  modifyIORef chatServerRef (removeClient clientId)
  broadCastChatRoomMessage chatServerRef clientId chatRoom (clientExitMessage chatRoom clientId)
  (hPutStrLn h "See you soon...")
  hClose h
  exitWith ExitSuccess

showWhoAmi :: ClientId -> String
showWhoAmi clientId = "Client id: " ++ show clientId

whoAmiCommand :: Handle -> ClientId -> IO ()
whoAmiCommand h clientId = hPutStrLn h $ showWhoAmi clientId

whoCommand :: ChatServerRef -> Handle -> IO ()
whoCommand chatServerRef h = do
  clients <- readIORef chatServerRef
  hPutStrLn h $ showClients clients

helpCommand :: Handle -> IO ()
helpCommand h = do
  hPutStrLn h ""
  hPutStrLn h "HaskChat help menu"
  hPutStrLn h "------------------"
  hPutStrLn h ""
  mapM_ (\(cmd, dsc) -> hPutStrLn h (cmd ++ "\t\t" ++ dsc) ) commandHelpData
  hPutStrLn h ""
