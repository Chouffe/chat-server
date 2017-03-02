-- | Runner for Chat server. We put Main separately so that we can keep chat as
-- a library for testing.
module Server (main) where

import           Data.Maybe         (fromMaybe)
import           Network
import           System.Environment (lookupEnv)

import           Chat               (chat)

-- | Run our chat server.
main :: IO ()
main = do
  chatServerPort <- fmap read <$> lookupEnv "CHAT_SERVER_PORT"
  chat $ PortNumber $ fromMaybe 12345 chatServerPort
