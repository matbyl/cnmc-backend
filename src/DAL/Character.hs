
{-# LANGUAGE Arrows #-}

module DAL.Character (getCharacterFromDB, addCharacterToDB, listCharactersFromDB, addCharactersToDB) where

import Control.Arrow
import Control.Monad
import Data.Aeson
import Data.Coerce
import Data.Profunctor.Product (p2)
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
    Column PGText
  )

type ReadField =
  ( Column PGUuid,
    Column PGText
  )

characterTable ::
  CharacterTable
characterTable =
  Table
    "characters"
    ( p2
        ( optional "id",
          required "name"
        )
    )

selectAllRows :: Connection -> IO [(UUID, String)]
selectAllRows conn = runQuery conn $ queryTable characterTable

addCharacterToDB :: Connection -> CharacterForm -> IO Character
addCharacterToDB conn (CharacterForm name) =
  (character . head)
    <$> runInsert_
      conn
      ( Insert
          characterTable
          [toFields (uuid, name)]
          (rReturning id)
          Nothing
      )
  where
    uuid :: Maybe UUID
    uuid = Nothing

addCharactersToDB :: Connection -> [CharacterForm] -> IO [Character]
addCharactersToDB conn characters =
    map character
    <$> runInsert_
      conn
      ( Insert
          characterTable
          ((\(CharacterForm name) -> toFields (uuid, name)) <$> characters)
          (rReturning id)
          Nothing
      )
  where
    uuid :: Maybe UUID
    uuid = Nothing

selectCharacterByIdFromPSQL ::
  Connection ->
  UUID ->
  IO [(UUID,  String)]
selectCharacterByIdFromPSQL conn key = runSelect conn $
  proc () -> do
    row@(id, _) <- queryTable characterTable -< ()
    restrict -< (id .== constant key)
    returnA -< row

getCharacterFromDB :: Connection -> UUID -> IO Character
getCharacterFromDB conn id = (character . head) <$> selectCharacterByIdFromPSQL conn id

character :: (UUID, String) -> Character
character (id, name) = Character id name

listCharactersFromDB :: Connection -> IO [Character]
listCharactersFromDB conn = map character <$> selectAllRows conn

updateRow :: Connection -> (Maybe UUID, String) -> IO ()
updateRow conn row@(key, name) = do
  runUpdate
    conn
    characterTable
    (\_ -> constant row)
    ( \(k, _) -> case key of
        Just key' -> k .== constant key'
        Nothing -> pgBool False
    )
  return ()
