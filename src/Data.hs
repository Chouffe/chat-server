module Data
  ( emptyChatServerRef
  , changeChatRoom
  , nextClientId
  , clientExitMessage
  , defaultChatRoom
  , userJoinMessage
  , removeClient
  , registerClient
  , clientIdChatRoom
  , showMessage
  , showClients
  , showChatRoom
  , showChatRooms
  , ClientId
  , Clients
  , ChatCommand(..)
  , ChatServerRef
  , CommandError(..)
  , Message
  , ChatRoom(..)
  )
  where

import           Data.IORef                    (IORef, newIORef, atomicModifyIORef)
import           Data.List                     (find)
import           Data.Maybe                    (fromMaybe)
import           System.IO                     (Handle)
import           Text.ParserCombinators.Parsec (ParseError)

type ClientId = Integer
type Clients = [(ClientId, ChatRoom, Handle)]
type ChatServerRef = IORef Clients
type Message = String

data ChatRoom =
    Default
  | Haskell
  deriving (Eq, Show)

data CommandError = CommandParseError ParseError String
  deriving (Eq)

instance Show CommandError where
  show (CommandParseError _ input) = "server: invalid command: " ++ show input ++ " - use /help to display the help menu"

data ChatCommand =
    ChatRooms
  | Join ChatRoom
  | Msg ClientId Message
  | Quit
  | Who
  | Whoami
  | Help
  deriving (Eq, Show)

-- API

-- TODO: document API
-- TODO: only export public API

defaultChatRoom :: ChatRoom
defaultChatRoom = Default

emptyChatServerRef :: IO ChatServerRef
emptyChatServerRef = newIORef []

showMessage :: ClientId -> Message -> String
showMessage clientId message = show clientId ++ ": " ++ message

showClients :: Clients -> String
showClients clients = show n ++ " clients are connected | " ++ show clientIds
  where n = length clients
        clientIds = fmap (\(cid, _, _) -> cid) clients

showChatRoom :: ChatRoom -> Clients -> String
showChatRoom chatRoom clients =
  show (length cids) ++ " clients are in chatroom " ++ show chatRoom
  where cids = filter (\(_, cr, _) -> cr == chatRoom) clients

showChatRooms :: String
showChatRooms = "default, haskell"

userJoinMessage :: ChatRoom -> ClientId -> Message
userJoinMessage chatRoom clientId =
  show clientId ++ " has joined chatroom " ++ show chatRoom

clientExitMessage :: ChatRoom -> ClientId -> Message
clientExitMessage chatRoom clientId =
  show clientId ++ " has left chatroom " ++ show chatRoom

changeChatRoom :: ChatRoom -> ClientId -> Clients -> Clients
changeChatRoom chatRoom clientId =
  fmap (\(cid, cr, h) ->
    if cid == clientId
    then (cid, chatRoom, h)
    else (cid, cr, h))

nextClientId :: Clients -> ClientId
nextClientId []      = 0
nextClientId clients = 1 + (maximum $ fmap (\(cid, _, _) -> cid) clients)

removeClient :: ClientId -> Clients -> Clients
removeClient clientId = filter (\(cid, _, _) -> cid /= clientId)

clientIdChatRoom :: ClientId -> Clients -> ChatRoom
clientIdChatRoom clientId =
  (fromMaybe defaultChatRoom) . fmap (\(_, chatRoom, _) -> chatRoom) . find (\(cid, _, _) -> cid == clientId)

addClient :: Handle -> Clients -> (Clients, ClientId)
addClient h clients = ((clientId, defaultChatRoom, h) : clients, clientId)
  where clientId = nextClientId clients

registerClient :: ChatServerRef -> Handle -> IO ClientId
registerClient chatServerRef h = atomicModifyIORef chatServerRef (addClient h)
