{-# LANGUAGE OverloadedStrings #-}

module CeqRobot.Util
( format
, tread
, treadMaybe
, tshow
)
where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Format as F
import Data.Text.Format.Params (Params)
import Text.Read

format :: Params ps => F.Format -> ps -> Text
format f ps = LT.toStrict $ F.format f ps

tread :: Read a => Text -> a
tread = read . T.unpack

treadMaybe :: Read a => Text -> Maybe a
treadMaybe = readMaybe . T.unpack

tshow :: Show a => a -> Text
tshow = T.pack . show
