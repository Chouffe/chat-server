module Parser
  (readCommand)
  where


import           Text.ParserCombinators.Parsec

import           Data

clientId :: Parser ClientId
clientId = read <$> many1 digit

commandPrefix :: Parser Char
commandPrefix = char '/'

commandMsg :: Parser ChatCommand
commandMsg = do
  _ <- string "msg"
  spaces
  cid <- clientId
  spaces
  msg <- many1 anyChar
  return $ Msg cid msg

commandString :: String -> ChatCommand -> Parser ChatCommand
commandString str chatCommand = string str *> pure chatCommand

commandQuit :: Parser ChatCommand
commandQuit = commandString "quit" Quit

commandWhoami :: Parser ChatCommand
commandWhoami = commandString "whoami" Whoami

commandWho :: Parser ChatCommand
commandWho = commandString "who" Who

commandHelp :: Parser ChatCommand
commandHelp = commandString "help" Help

chatRoomHaskell :: Parser ChatRoom
chatRoomHaskell = string "haskell" *> return Haskell

chatRoomDefault :: Parser ChatRoom
chatRoomDefault = string "default" *> return Default

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
command =
  commandPrefix *>
    (     commandQuit
      <|> commandMsg
      <|> commandJoin
      <|> commandHelp
      <|> (try commandWhoami <|> commandWho)
    )

readCommand :: String -> Either CommandError ChatCommand
readCommand input =
  case parse command "chatCommand" input of
    Left err  -> Left $ CommandParseError err
    Right val -> Right val
