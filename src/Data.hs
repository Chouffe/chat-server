module Data
  ( ClientId
  , Clients
  , ChatCommand(..)
  , ChatServerRef
  , CommandError(..)
  , Message
  , ChatRoom(..)
  )
  where

import Text.ParserCombinators.Parsec (ParseError)
import Data.IORef (IORef)
import System.IO (Handle)

type ClientId = Integer
type Clients = [(ClientId, Handle)]
type ChatServerRef = IORef Clients
type Message = String

data ChatRoom =
    Default
  | Haskell
  deriving (Eq, Show)

data CommandError = CommandParseError ParseError
  deriving (Eq, Show)

data ChatCommand =
    Join ChatRoom
  | Msg ClientId Message
  | Quit
  | Who
  | Whoami
  | Help
  deriving (Eq, Show)
