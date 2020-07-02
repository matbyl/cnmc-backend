{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module DAL.Work
  ( listWorkFromDB
  )
where

import           Control.Arrow
import           Control.Monad
import           DAL.Genre
import           DAL.Medium
import           Data.Aeson
import           Data.Coerce
import           Data.Profunctor.Product        ( p5 )
import           Data.Profunctor.Product.Default
import           Data.Time
import           Data.Typeable
import           Data.UUID
import           Database.PostgreSQL.Simple
import qualified Database.PostgreSQL.Simple.FromField
                                               as PG
import           Database.PostgreSQL.Simple.FromField
                                         hiding ( Field
                                                , tableColumn
                                                )
import           Domain.Model.Work
import           GHC.Generics
import           GHC.Int                        ( Int64 )
import           Opaleye                 hiding ( FromField )
import           Opaleye.EnumMapper

type WorkTable
  = Table
      ( Column PGUuid
      , Column PGText
      , Column PGTimestamptz
      , Column PGGenre
      , Column PGMedium
      )
      ( Column PGUuid
      , Column PGText
      , Column PGTimestamptz
      , Column PGGenre
      , Column PGMedium
      )

workTable :: WorkTable
workTable = Table
  "work"
  (p5
    ( required "id"
    , required "name"
    , required "released"
    , required "genre"
    , required "medium"
    )
  )

work :: (UUID, String, UTCTime, Genre, Medium) -> Work
work (id, name, released, genre, medium) = Work id name released genre medium

listWorkFromDB :: Connection -> IO [Work]
listWorkFromDB conn = map work <$> (runQuery conn $ queryTable workTable)
