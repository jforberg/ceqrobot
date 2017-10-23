module SudoSatirical.Article
( Article(..)
)
where

import qualified Data.Text as Text
import Data.Text (Text)
import Data.Time.Clock

data Article = Article
   { articleUrl :: Text
   , articleName :: Text
   , articlePublished :: UTCTime
   }
   deriving (Eq, Show)

instance Ord Article where
   a `compare` b = articlePublished a `compare` articlePublished b
