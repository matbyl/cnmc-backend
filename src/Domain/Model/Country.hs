module Domain.Model.Country where

import           Data.Aeson
import           GHC.Generics
import           Data.Swagger                   ( ToSchema )

data Country = Country { countryId :: String, countryName :: String } deriving (Generic, Show, ToSchema)

instance ToJSON Country
