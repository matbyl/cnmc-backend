{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Arrows #-}

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
                                                , p1
                                                , p5
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
import Opaleye.Aggregate (groupBy, aggregate, arrayAgg)


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
workGenreTable = Table "work_genre" (p2 (required "work_id", required "work_genre"))

work :: [Genre] -> (UUID, String, UTCTime, Medium) -> Work
work genres (id, name, released, medium) = Work id name released genres medium

addWorkToDB :: Connection -> [WorkForm] -> IO [Work]
addWorkToDB conn work = do
  (rawWork :: [(UUID, String, UTCTime, Medium, [Genre])]) <- forM work $ \w -> do
    (id, name, releaseDate ,medium) <- addWork w
    return (id, name, releaseDate, medium, workFormGenre w)
  forM rawWork $ \(id, name, releaseDate, medium, genres) -> do
      rawGenres <- forM genres $ addWorkGenreToDB conn id
      return $ Work id name releaseDate rawGenres medium
 where
  uuid :: Maybe UUID
  uuid    = Nothing
  addWork (WorkForm name releaseDate genres medium) = head <$> runInsert_
    conn
    (Insert workTable
            [toFields (uuid, name, releaseDate, medium)]
            (rReturning id)
            Nothing
    )

addWorkGenreToDB :: Connection -> UUID -> Genre -> IO Genre
addWorkGenreToDB conn workId genre = selectGenre . head <$> runInsert_
  conn
  (Insert workGenreTable [toFields (workId, genre)] (rReturning id) Nothing)
 where
  selectGenre :: (UUID, Genre) -> Genre
  selectGenre = snd

listWorkFromDB :: Connection -> IO [Work]
listWorkFromDB conn = (map (\(id, name, releaseDate, medium, genre) -> Work id name releaseDate genre medium)) <$> runQuery conn selectWork


selectWork :: Select ( Column PGUuid
      , Column PGText
      , Column PGTimestamptz
      , Column PGMedium
      , Column (SqlArray PGGenre)
      )
selectWork = aggregate (p5 (groupBy, groupBy, groupBy, groupBy, arrayAgg)) $ proc () -> do
      (workId', name, releaseDate, medium) <- queryTable workTable -< ()
      (workId, workGenre) <- selectTable workGenreTable -< () 

      restrict -< workId' .== workId
      
      returnA -< (workId', name, releaseDate, medium, workGenre)
