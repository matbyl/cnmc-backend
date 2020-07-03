{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module DAL.Work
  ( listWorkFromDB
  , addWorkToDB
  )
where

import           Control.Arrow
import           Control.Monad
import           DAL.Genre
import           DAL.Medium
import           Data.Aeson
import           Data.Coerce
import           Data.Profunctor.Product        ( p4
                                                , p2
                                                )
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
      ( Maybe (Column PGUuid)
      , Column PGText
      , Column PGTimestamptz
      , Column PGMedium
      )
      (Column PGUuid, Column PGText, Column PGTimestamptz, Column PGMedium)
type WorkGenreTable
  = Table (Column PGUuid, Column PGGenre) (Column PGUuid, Column PGGenre)

workTable :: WorkTable
workTable = Table
  "work"
  (p4 (optional "id", required "name", required "released", required "medium"))

workGenreTable :: WorkGenreTable
workGenreTable = Table "workGenre" (p2 (required "workId", required "genre"))

work :: [Genre] -> (UUID, String, UTCTime, Medium) -> Work
work genres (id, name, released, medium) = Work id name released genres medium

addWorkToDB :: Connection -> WorkForm -> IO Work
addWorkToDB conn (WorkForm name releaseDate genres medium) = do
  work       <- addWork
  workGenres <- forM_ genres $ addWorkGenreToDB conn work
  return work
 where
  uuid :: Maybe UUID
  uuid    = Nothing
  addWork = (work [] . head) <$> runInsert_
    conn
    (Insert workTable
            [toFields (uuid, name, releaseDate, medium)]
            (rReturning (\res@(id, name, released, medium) -> res))
            Nothing
    )

addWorkGenreToDB :: Connection -> Work -> Genre -> IO Genre
addWorkGenreToDB conn (Work workId _ _ _ _) genre =
  selectGenre . head <$> runInsert_
    conn
    (Insert workGenreTable [toFields (workId, genre)] (rReturning id) Nothing)
 where
  selectGenre :: (UUID, Genre) -> Genre
  selectGenre = snd

listWorkFromDB :: Connection -> IO [Work]
listWorkFromDB conn = map (work []) <$> (runQuery conn $ queryTable workTable)
