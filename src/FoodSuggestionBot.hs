module FoodSuggestionBot
  ( foodSuggestionBot
  ) where

import           Data.List
import           Data.Text            (pack, unpack)
import           System.Random
import           Web.Telegram.API.Bot

foods =
  [ "Salamipizza"
  , "Salat"
  , "Noodlez mit Pesto"
  , "1 @MeisterRados ping höhö"
  , "Grießknödel"
  , "Brezel-Emoji (benutz deine Vorstellungskraft, ich kann kein UTF-8)"
  , "Riesendöner"
  ]

foodCount = length foods

foodSuggestion i = foods !! i

foodSuggestionBot :: StdGen -> Update -> Maybe SendMessageRequest
foodSuggestionBot randomGenerator Update {message = Just Message { chat = Chat {chat_id = chat_id}
                                                                 , text = Just text
                                                                 }} =
  case response of
    Just responseText ->
      Just $ sendMessageRequest (ChatId chat_id) $ pack responseText
    Nothing -> Nothing
  where
    response = (respond randomGenerator (unpack text))
foodSuggestionBot _ _ = Nothing

respond :: StdGen -> String -> Maybe String
respond randomGenerator text
  | isPrefixOf "/food" text = Just (suggestFood randomGenerator)
  | isPrefixOf "/start" text = Just (helpText)
  | isPrefixOf "/help" text = Just (helpText)
  | otherwise = Nothing

helpText =
  "Hi du bob. Ich schlage dir Essen vor. Aber nur mieses. Mach mal /food"

suggestFood :: StdGen -> String
suggestFood randomGenerator =
  foodSuggestion (fst $ randomR (0, foodCount - 1) randomGenerator :: Int)
