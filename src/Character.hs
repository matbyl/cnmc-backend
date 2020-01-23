{-# LANGUAGE Arrows #-}

module Character (listCharacters, addCharacter, Character, CharacterForm) where

import           Control.Arrow
import           Control.Monad
import           Data.Aeson
import           Data.Coerce
import           Data.Profunctor.Product    (p3)
import           Data.UUID
import           Database.PostgreSQL.Simple
import           GHC.Generics
import           Opaleye

data CharacterForm =
  CharacterForm
  { characterFormName        :: String
  , characterFormDescription :: String
  }
  deriving (Generic, Show)

instance FromJSON CharacterForm

data Character =
  Character
    { characterId          :: UUID
    , characterName        :: String
    , characterDescription :: String
    }
  deriving (Generic, Show)

instance ToJSON Character

characterTable ::
     Table (Maybe (Column PGUuid), Column PGText, Column PGText) -- read type
      (Column PGUuid, Column PGText, Column PGText) -- write type
characterTable =
  Table
    "characters"
    (p3 (optional "id", required "name", required "description"))

selectAllRows :: Connection -> IO [(UUID, String, String)]
selectAllRows conn = runQuery conn $ queryTable characterTable

addCharacter :: Connection -> CharacterForm -> IO ()
addCharacter conn (CharacterForm name desc) = do
  runInsertMany conn characterTable $ [toFields (uuid, name, desc)]
  return ()
  where
      uuid :: Maybe UUID
      uuid = Nothing


selectById :: Connection -> UUID -> IO [(UUID, String, String)]
selectById conn key =
  runQuery conn $
  proc () ->
  do row@(id, _, _) <- queryTable characterTable -< ()
     restrict -< (id .== constant key)
     returnA -< row

listCharacters :: Connection -> IO [Character]
listCharacters conn = do
  rows <- selectAllRows conn
  return $ map (\(id, name, desc) -> Character id name desc) rows

updateRow :: Connection -> (Maybe UUID, String, String) -> IO ()
updateRow conn row@(key, name, email) = do
  runUpdate
    conn
    characterTable
    (\_ -> constant row) -- what should the matching row be updated to
    (\(k, _, _) -> case key of
        Just key' -> k .== constant key'
        Nothing   -> pgBool False ) -- which rows to update?
  return ()
