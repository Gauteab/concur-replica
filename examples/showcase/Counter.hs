{-# LANGUAGE OverloadedStrings #-}

module Counter where

import Concur.Core (Widget)
import Concur.Replica (HTML, button, div, onClick, text)
import Data.Functor (($>))
import qualified Data.Text as Text
import Prelude hiding (div)

counterWidget :: Int -> Widget HTML a
counterWidget count = do
  newCount <-
    div
      []
      [ button [onClick] [text "-"] $> count - 1
      , text $ Text.pack (show count)
      , button [onClick] [text "+"] $> count + 1
      ]
  counterWidget newCount
