
{-# LANGUAGE Arrows #-}

module DAL.Character where

import Control.Arrow
import Control.Monad
import Data.Aeson
import Data.Coerce
import Data.Profunctor.Product (p5)
import Data.Time
import Data.UUID
import Database.PostgreSQL.Simple
import GHC.Generics
import GHC.Int (Int64)
import Opaleye

import Domain.Model.Character

type CharacterTable = Table WriteField ReadField

type WriteField =
  ( Maybe (Column PGUuid),
    Column PGText,
    Column PGText,
    Column PGText,
    Column PGTimestamptz
  )

type ReadField =
  ( Column PGUuid,
    Column PGText,
    Column PGText,
    Column PGText,
    Column PGTimestamptz
  )

characterTable ::
  CharacterTable
characterTable =
  Table
    "characters"
    ( p5
        ( optional "id",
          required "description",
          required "firstname",
          required "lastname",
          required "birthday"
        )
    )

selectAllRows :: Connection -> IO [(UUID, String, String, String, UTCTime)]
selectAllRows conn = runQuery conn $ queryTable characterTable

addCharacterToDB :: Connection -> CharacterForm -> IO Character
addCharacterToDB conn (CharacterForm firstname lastname birthday desc) =
  (character . head)
    <$> runInsert_
      conn
      ( Insert
          characterTable
          [toFields (uuid, desc, firstname, lastname, birthday)]
          (rReturning id)
          Nothing
      )
  where
    uuid :: Maybe UUID
    uuid = Nothing

selectCharacterByIdFromPSQL ::
  Connection ->
  UUID ->
  IO [(UUID, String, String, String, UTCTime)]
selectCharacterByIdFromPSQL conn key = runSelect conn $
  proc () -> do
    row@(id, _, _, _, _) <- queryTable characterTable -< ()
    restrict -< (id .== constant key)
    returnA -< row

getCharacterFromDB :: Connection -> UUID -> IO Character
getCharacterFromDB conn id = (character . head) <$> selectCharacterByIdFromPSQL conn id

character :: (UUID, String, String, String, UTCTime) -> Character
character (id, desc, firstname, lastname, birthday) = Character id desc firstname lastname birthday

listCharactersFromDB :: Connection -> IO [Character]
listCharactersFromDB conn = map character <$> selectAllRows conn

updateRow :: Connection -> (Maybe UUID, String, String, String, UTCTime) -> IO ()
updateRow conn row@(key, desc, firstname, lastname, birthday) = do
  runUpdate
    conn
    characterTable
    (\_ -> constant row)
    ( \(k, _, _, _, _) -> case key of
        Just key' -> k .== constant key'
        Nothing -> pgBool False
    )
  return ()
