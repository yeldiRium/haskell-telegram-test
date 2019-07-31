{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent
import           Control.Monad           (forever)
import           Data.Either
import           Data.List               (intercalate)
import           Data.Maybe
import           Data.Text               (pack)
import           Network.HTTP.Client     (Manager, newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           System.Environment
import           System.Random
import           Web.Telegram.API.Bot

import           FoodSuggestionBot       (foodSuggestionBot)

pollingTimeout = 5

main :: IO ()
main = do
  tokenEnv <- lookupEnv "TELEGRAM_API_TOKEN"
  if isNothing tokenEnv
    then putStrLn "No bot token found. Stopping."
    else do
      let token = Token (pack $ "bot" ++ (fromJust tokenEnv))
      manager <- newManager tlsManagerSettings
      mainLoop token manager 0

mainLoop :: Token -> Manager -> Int -> IO ()
mainLoop token manager lastUpdate = do
  randomGenerator <- newStdGen
  putStrLn $ "Retrieving Updates for offset " ++ (show lastUpdate)
  -- get a new load of updates from the API
  eitherUpdates <-
    getUpdates token (Just lastUpdate) Nothing (Just pollingTimeout) manager
  -- either return an empty list when the request fails or retrieve the response's result
  updates <-
    case eitherUpdates of
      Left e -> do
        putStrLn "Fetching Updates Failed:"
        print e
        return []
      Right update -> do
        putStrLn "Fetching Updates succeeded."
        return $ result update
  incomingLogger updates
  -- generate SendMessageRequests from the updates to send back to the API
  let newMessages =
        (catMaybes $ map (foodSuggestionBot randomGenerator) updates)
  outgoingLogger newMessages
  -- send all new messages to the API
  sentMessages <- mapM (\x -> sendMessage token x manager) newMessages
  -- log if any of the outgoing requests failed
  mapM_
    (either
       (\e -> do
          putStrLn "Sending Messages failed:"
          print e)
       (const mempty))
    sentMessages
  -- next loop with new update id
  mainLoop token manager (nextUpdateId lastUpdate updates)

incomingLogger :: [Update] -> IO ()
incomingLogger updates =
  putStrLn $
  "Retrieved Updates " ++ (intercalate " " (map (show . update_id) updates))

outgoingLogger :: [SendMessageRequest] -> IO ()
outgoingLogger messages =
  putStrLn $
  "Sendings messages:\n" ++
  (intercalate
     "\n"
     (map
        (\SendMessageRequest {message_chat_id = cid, message_text = text} ->
           show cid ++ " - " ++ show text)
        messages))

nextUpdateId :: Int -> [Update] -> Int
nextUpdateId i [] = i + 1
nextUpdateId _ us = (update_id (last us)) + 1
