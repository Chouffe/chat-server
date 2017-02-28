-- | Runner for Chat server. We put Main separately so that we can keep chat as
-- a library for testing.
module Server (main) where

import Chat (chat)

-- | Run our chat server.
main :: IO ()
main = chat
