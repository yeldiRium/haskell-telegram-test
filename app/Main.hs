{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Network.HTTP.Client      (newManager)
import           Network.HTTP.Client.TLS  (tlsManagerSettings)
import           Web.Telegram.API.Bot

import           Lib                      (isTextMessage foodSuggestionBot)

main :: IO ()
main : forever $ do {
    let token = Token "bot<token>"
    manager <- newManager tlsManagerSettings
    response <- runTelegramClient token manager (getUpdatesM getUpdatesRequest)
    let messageUpdates = filter isTextMessage response::result
    let sendRequests = map foodSuggestionBot messageUpdates
    do {
        sendRequest <- sendRequests
        sendResponse <- sendMessage token sendRequest manager
        case sendResponse of
            Left e -> do
                putStrLn "Requst failed"
                print e
            Right Response { result = m } -> do
                putStrLn "Request succeeded"
                print $ message_id m
                print $ text m
    }
}
