module Domain.Model.Work where

import Data.Aeson
import Data.Time
import Data.UUID
import GHC.Generics

data Work = Work {
        workId :: UUID
        , name :: String
        , released :: UTCTime
        , genre :: Genre
        , medium :: Medium
    } deriving (Generic, Show, FromJSON)

data Genre = Action | Comedy | Horror deriving (Generic, Show, FromJSON)

data Medium = Medium | TvShow | Radio deriving (Generic, Show, FromJSON)