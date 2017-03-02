module Data
  ( emptyChatServerRef
  , defaultChatRoom
  , ClientId
  , Clients
  , ChatCommand(..)
  , ChatServerRef
  , CommandError(..)
  , Message
  , ChatRoom(..)
  )
  where

import           Data.IORef                    (IORef, newIORef)
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

defaultChatRoom :: ChatRoom
defaultChatRoom = Default

emptyChatServerRef :: IO ChatServerRef
emptyChatServerRef = newIORef []
