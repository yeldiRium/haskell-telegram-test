module Lib
    ( isTextMessage, foodSuggestionBot
    ) where


        isTextMessage :: Update -> Bool
        isTextMessage Update{message = Just Message{text = Just text}} = True
        isTextMessage _ = False

        foodSuggestionBot :: Update -> SendMessageRequest
        foodSuggestionBot Update{message = Just Message{chat = Chat{ chat_id = chat_id }, text = Just text}} = sendMessageRequest chat_id test
