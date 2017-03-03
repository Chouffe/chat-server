module Logger
  ( newChatLoggerSet
  , removeChatLoggerSet
  , log
  , formatBroadcastedMessage
  , formatClientInput
  , formatCommandError
  , formatCommandPerformed
  )
  where

import           Prelude               hiding (log)
import Data
import           System.Log.FastLogger (LoggerSet, defaultBufSize,
                                        newStdoutLoggerSet, pushLogStrLn,
                                        rmLoggerSet, toLogStr)

import Data.Time.Clock (getCurrentTime)

newChatLoggerSet :: IO LoggerSet
newChatLoggerSet = newStdoutLoggerSet defaultBufSize

removeChatLoggerSet :: LoggerSet -> IO ()
removeChatLoggerSet = rmLoggerSet

log :: LoggerSet -> String -> IO ()
log loggerSet msg = do
  currentTime <- getCurrentTime
  pushLogStrLn loggerSet $ toLogStr $ prependTime msg currentTime
  where prependTime s time = "["++ show time ++ "]" ++ s

formatClientInput :: ClientId -> ChatRoom -> String -> String
formatClientInput clientId chatRoom input = "[client-input][client-id:" ++ show clientId ++ "]" ++ "[chatRoom:" ++ show chatRoom ++ "][" ++ "input:" ++ show input ++ "]"

formatCommandError :: ClientId -> ChatRoom -> CommandError -> String
formatCommandError clientId chatRoom err = "[command-error][client-id:" ++ show clientId ++ "]" ++ "[chatRoom:" ++ show chatRoom ++ "]" ++ "[error:" ++ show err ++ "]"

formatCommandPerformed :: ClientId -> ChatRoom -> ChatCommand -> String
formatCommandPerformed clientId chatRoom command = "[command-performed][client-id:" ++ show clientId ++ "]" ++ "[chatRoom:" ++ show chatRoom ++ "]" ++ "[command:" ++ show command ++ "]"

formatBroadcastedMessage :: ClientId -> ChatRoom -> String -> String
formatBroadcastedMessage clientId chatRoom input = "[broadcast][client-id:" ++ show clientId ++ "]" ++ "[chatRoom:" ++ show chatRoom ++ "]" ++ "[input:" ++ show input ++ "]"
