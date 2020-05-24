module Domain.Model.Country where

import Data.Aeson
import GHC.Generics

data Country = Country { countryId :: String, value :: String } deriving (Generic, Show)
  
instance ToJSON Country
