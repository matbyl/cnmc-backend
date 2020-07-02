module DAL.Country
  ( listCountriesFromDB
  )
where

import           Control.Arrow
import           Control.Monad
import           Data.Aeson
import           Data.Coerce
import           Data.Profunctor.Product        ( p2 )
import           Data.Time
import           Data.UUID
import           Database.PostgreSQL.Simple
import           GHC.Generics
import           GHC.Int                        ( Int64 )
import           Opaleye

import           Domain.Model.Country

type CountryTable
  = Table (Column PGText, Column PGText) (Column PGText, Column PGText)

type WriteField
  = ( Maybe (Column PGUuid)
    , Column PGText
    , Column PGText
    , Column PGText
    , Column PGTimestamptz
    )

type ReadField
  = ( Column PGUuid
    , Column PGText
    , Column PGText
    , Column PGText
    , Column PGTimestamptz
    )

countryTable :: CountryTable
countryTable = Table "countries" (p2 (required "id", required "value"))

country :: (String, String) -> Country
country = uncurry Country

listCountriesFromDB :: Connection -> IO [Country]
listCountriesFromDB conn =
  map country <$> (runQuery conn $ queryTable countryTable)
