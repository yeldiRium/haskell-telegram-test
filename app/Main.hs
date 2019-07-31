{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent
import           Control.Monad           (forever)
import           Data.List               (intercalate)
import           Network.HTTP.Client     (Manager, newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Web.Telegram.API.Bot

import           Lib                     (handleMessage, isTextMessage)

pollingTimeout = 5

updatesRequestForOffset :: Int -> GetUpdatesRequest
updatesRequestForOffset 0 = getUpdatesRequest
updatesRequestForOffset i =
  GetUpdatesRequest (Just i) Nothing (Just pollingTimeout) (Just ["message"])

nextUpdateId :: Int -> [Update] -> Int
nextUpdateId i [] = i + 1
nextUpdateId _ us = (update_id (last us)) + 1

main :: IO ()
main = do
  let token = Token "bot726215439:AAGjlImqCFoaN0WKpyDXtUnRT8KpS-y_p2U"
  manager <- newManager tlsManagerSettings
  mainLoop token manager 0

mainLoop :: Token -> Manager -> Int -> IO ()
mainLoop token manager lastUpdate = do
  putStrLn $ "Retrieving Updates for offset " ++ (show lastUpdate)
  eitherResponseOrError <-
    getUpdates token (Just lastUpdate) Nothing (Just pollingTimeout) manager
  handleUpdateResponse token manager lastUpdate eitherResponseOrError

handleUpdateResponse ::
     Show a => Token -> Manager -> Int -> Either a (Response [Update]) -> IO ()
handleUpdateResponse token manager lastUpdate eitherResponseOrError =
  case eitherResponseOrError of
    Left e -> do
      putStrLn "Retrieving Updates failed."
      print e
      mainLoop token manager (nextUpdateId lastUpdate [])
    Right response -> do
      let updates = result response
      putStrLn
        ("Retrieved Updates " ++
         (intercalate " " (map (show . update_id) updates)))
      sendRequests newSendRequests $ map handleMessage updates
      mainLoop token manager (nextUpdateId lastUpdate updates)

sendRequests :: Token -> Manager -> [SendMessageRequest] -> IO ()
sendRequests token manager = mapM_ (sendRequest token manager)

sendRequest :: Token -> Manager -> SendMessageRequest -> IO ()
sendRequest token manager request = do
  sendResponse <- sendMessage token request manager
  case sendResponse of
    Left e -> do
      putStrLn "Sending Message failed."
      print e
    Right Response {result = m} -> do
      putStrLn "Sent Message successfully."
      print $ message_id m
      print $ text m
