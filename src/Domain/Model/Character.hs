module Domain.Model.Character where

import           Data.Aeson
import           Data.Time
import           Data.UUID
import           GHC.Generics
import           Data.Swagger                   ( ToSchema )

data CharacterForm = CharacterForm
  { characterFormName :: String
  }
  deriving (Generic, Show, ToSchema)

instance FromJSON CharacterForm

data Character = Character
  { characterId :: UUID,
    characterName :: String
  }
  deriving (Generic, Show, ToSchema)

instance ToJSON Character
