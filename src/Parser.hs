module Parser
  (readCommand)
  where


import           Text.ParserCombinators.Parsec

import           Data

clientId :: Parser ClientId
clientId = read <$> many1 digit

commandPrefix :: Parser Char
commandPrefix = char '/'

commandQuit :: Parser ChatCommand
commandQuit = string "quit" >> return Quit

commandMsg :: Parser ChatCommand
commandMsg = do
  _ <- string "msg"
  spaces
  cid <- clientId
  spaces
  msg <- many1 anyChar
  return $ Msg cid msg

chatRoomHaskell :: Parser ChatRoom
chatRoomHaskell = string "haskell" >> return Haskell

chatRoomDefault :: Parser ChatRoom
chatRoomDefault = string "default" >> return Default

chatRoom :: Parser ChatRoom
chatRoom = chatRoomHaskell <|> chatRoomDefault

commandJoin :: Parser ChatCommand
commandJoin = do
  _ <- string "join"
  spaces
  cr <- chatRoom
  spaces
  return $ Join cr

command :: Parser ChatCommand
command = commandPrefix *> (commandQuit <|> commandMsg <|> commandJoin)

readCommand :: String -> Either CommandError ChatCommand
readCommand input =
  case parse command "chatCommand" input of
    Left err  -> Left $ CommandParseError err
    Right val -> Right val
