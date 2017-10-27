{-# LANGUAGE OverloadedStrings #-}

module CeqRobot.Util
( format
)
where

import qualified Data.Text.Lazy as LT
import qualified Data.Text.Format as F

--format
format f ps = LT.toStrict $ F.format f ps
