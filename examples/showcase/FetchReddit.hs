{-# LANGUAGE OverloadedStrings #-}

module FetchReddit where

import Concur.Core (Widget, orr)
import Concur.Replica (HTML, MouseEvent, button, h4, li, onClick, text, ul)
import Control.Applicative ((<|>))
import Control.Lens ((^..))
import Control.Monad.IO.Class (liftIO)
import Data.Aeson.Lens (_String, key, values)
import Data.Text (Text, unpack)
import qualified Network.Wreq as Wreq
import Prelude hiding (div)

subreddits :: [Text]
subreddits = ["haskell", "purescript", "elm"]

redditWidgets :: Widget HTML a
redditWidgets = orr $ map redditWidget subreddits

redditWidget :: Text -> Widget HTML a
redditWidget sub = do
  _ <- h4 [] [text ("/r/" <> sub)] <|> fetchWidget sub
  redditWidget sub

fetchWidget :: Text -> Widget HTML MouseEvent
fetchWidget sub = do
  _ <- button [onClick] [text "Fetch posts"]
  posts <- liftIO (fetchPosts sub) <|> text "Loading..."
  viewPosts posts <|> button [onClick] [text "Close"]

viewPosts :: [Text] -> Widget HTML a
viewPosts =
  ul [] . map (li [] . pure . text)

fetchPosts :: Text -> IO [Text]
fetchPosts sub = do
  let url = "https://www.reddit.com/r/" <> sub <> ".json"
  result <- Wreq.get (unpack url)
  pure . take 5 $
    result
      ^.. Wreq.responseBody
        . key "data"
        . key "children"
        . values
        . key "data"
        . key "title"
        . _String
