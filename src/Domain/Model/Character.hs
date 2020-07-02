module Domain.Model.Character where

import           Data.Aeson
import           Data.Time
import           Data.UUID
import           GHC.Generics
import           Data.Swagger                   ( ToSchema )

data CharacterForm = CharacterForm
  { characterFormFirstname :: String,
    characterFormLastname :: String,
    characterFormBirthday :: UTCTime,
    characterFormDescription :: String
  }
  deriving (Generic, Show, ToSchema)

instance FromJSON CharacterForm

data Character = Character
  { characterId :: UUID,
    characterDescription :: String,
    characterFirstname :: String,
    characterLastname :: String,
    characterBirthday :: UTCTime
  }
  deriving (Generic, Show, ToSchema)

instance ToJSON Character
