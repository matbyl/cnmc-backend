module Domain.Model.Work where

import           Data.Aeson
import           Data.Time
import           Data.UUID
import           GHC.Generics
import           Data.Swagger                   ( ToSchema )


data WorkForm = WorkForm
  {
    workFormName :: String,
    workFormReleaseDate :: UTCTime,
    workFormGenre :: [Genre],
    workFormMedium :: Medium
  } deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data Work = Work
  { workId :: UUID,
    name :: String,
    released :: UTCTime,
    genre :: [Genre],
    medium :: Medium
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data Genre
  = Comedy
  | Action
  | SciFi
  | Superhero
  | Crime
  | Fantasy
  | Animated
  | Horror
  | Thriller
  | Supernatural
  | Drama
  | Dystopian
  | Zombie
  | Historic
  deriving (Generic, Show, FromJSON, ToJSON,  ToSchema)

data Medium
  = Television
  | Film
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)
