
{-# LANGUAGE Arrows #-}

module DAL.Actor (getActorFromDB, addActorToDB, listActorsFromDB) where

import Control.Arrow
import Control.Monad
import Data.Aeson
import Data.Coerce
import Data.Profunctor.Product (p4)
import Data.Time
import Data.UUID
import Database.PostgreSQL.Simple
import GHC.Generics
import GHC.Int (Int64)
import Opaleye

import Domain.Model.Actor

type ActorTable = Table WriteField ReadField

type WriteField =
  ( Maybe (Column PGUuid),
    Column PGText,
    Column PGText,
    Column PGTimestamptz
  )

type ReadField =
  ( Column PGUuid,
    Column PGText,
    Column PGText,
    Column PGTimestamptz
  )

actorTable ::
  ActorTable
actorTable =
  Table
    "actors"
    ( p4
        ( optional "id",
          required "firstname",
          required "lastname",
          required "birthday"
        )
    )

selectAllRows :: Connection -> IO [(UUID, String, String, UTCTime)]
selectAllRows conn = runQuery conn $ queryTable actorTable

addActorToDB :: Connection -> ActorForm -> IO Actor
addActorToDB conn (ActorForm firstname lastname birthday) =
  (actor . head)
    <$> runInsert_
      conn
      ( Insert
          actorTable
          [toFields (uuid, firstname, lastname, birthday)]
          (rReturning id)
          Nothing
      )
  where
    uuid :: Maybe UUID
    uuid = Nothing

getActorFromDB :: Connection -> UUID -> IO Actor
getActorFromDB conn id = (actor . head) <$> selectActorByIdFromDB
  where
    selectActorByIdFromDB = runSelect conn $
      proc () -> do
        row@(id', _, _, _) <- queryTable actorTable -< ()
        restrict -< (id' .== constant id)
        returnA -< row

uncurry4 :: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e
uncurry4 f ~(a, b, c, d) = f a b c d

actor :: (UUID, String, String, UTCTime) -> Actor
actor = uncurry4 Actor

listActorsFromDB :: Connection -> IO [Actor]
listActorsFromDB conn = map actor <$> selectAllRows conn

updateRow :: Connection -> (Maybe UUID, String, String, UTCTime) -> IO ()
updateRow conn row@(key, firstname, lastname, birthday) = do
  runUpdate
    conn
    actorTable
    (\_ -> constant row)
    ( \(k, _, _, _) -> case key of
        Just key' -> k .== constant key'
        Nothing -> pgBool False
    )
  return ()
