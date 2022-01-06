{-# LANGUAGE OverloadedStrings #-}

module Main where

import Concur.Core (Widget, orr)
import Concur.Replica (HTML, checked, div, h2, hr, input, onClick, pre, runDefault, style, text, type_)
import Control.Monad.Fix (fix)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Counter (counterWidget)
import Data.Function ((&))
import Data.Functor (($>))
import qualified Data.List as List
import Data.Text (Text, pack, unpack)
import FetchReddit (redditWidgets)
import Prelude hiding (div)

main :: IO ()
main = do
  runDefault 8080 "Showcase" $ \_ ->
    widgets
      & List.intersperse (hr [])
      & orr

widgets :: [Widget HTML a]
widgets =
  [ showcase (counterWidget 0) "Counter"
  , showcase redditWidgets "FetchReddit"
  ]

showcase :: Widget HTML (Bool, Bool) -> Text -> Widget HTML b
showcase widget name = go True False
  where
    go showCode showWidget = do
      source <- liftIO . readFile $ "./examples/showcase/" ++ unpack name ++ ".hs"
      (showCode', showWidget') <-
        div
          []
          [ h2 [] [text name]
          , input [type_ "checkbox", checked showCode, onClick] $> (not showCode, showWidget)
          , input [type_ "checkbox", checked showWidget, onClick $> (showCode, not showWidget)]
          , viewIf showCode widget
          , viewIf showWidget $ viewSource (pack source)
          ]
      go showCode' showWidget'

viewSource :: Text -> Widget HTML a
viewSource source =
  pre
    [style [("background", "aliceblue")]]
    [text source]

viewIf True w = w
viewIf False _ = text ""
