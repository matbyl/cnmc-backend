module Domain.Model.Actor where

import Data.Aeson
import Data.Time
import Data.UUID
import GHC.Generics

data ActorForm = ActorForm
  { actorFormFirstname :: String,
    actorFormLastname :: String,
    actorFormBirthday :: UTCTime
  }
  deriving (Generic, Show)

instance FromJSON ActorForm

data Actor = Actor
  { actorId :: UUID,
    actorFirstname :: String,
    actorLastname :: String,
    actorBirthday :: UTCTime
  }
  deriving (Generic, Show)

instance ToJSON Actor
