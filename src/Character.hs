{-# LANGUAGE Arrows #-}

module Character (listCharacters, addCharacter, Character, CharacterForm, getCharacter, CharacterEff, runCharacterEff) where

import           Control.Arrow
import           Control.Monad
import           Data.Aeson
import           Data.Coerce
import           Data.Profunctor.Product    (p3)
import           Data.UUID
import           Database.PostgreSQL.Simple
import           GHC.Generics
import           Opaleye
import qualified Polysemy                      as P
import qualified Polysemy.Input                      as PI
import  Polysemy (interpret, Sem, Members, )
import Polysemy.Embed (Embed, embed)
import GHC.Int (Int64)
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


data CharacterEff m a where
  AddCharacter :: CharacterForm -> CharacterEff m Character
  GetCharacter :: UUID -> CharacterEff m Character
  ListCharacters :: CharacterEff m [Character]


P.makeSem ''CharacterEff

runCharacterEff :: (Members [Embed IO, PI.Input Connection] r) => Sem ( CharacterEff : r) a -> Sem r a
runCharacterEff = interpret $ \case
          AddCharacter form -> do
            conn <- PI.input 
            embed $ addCharacterToPSQL conn form
          GetCharacter id -> do
            conn <- PI.input
            embed $ getCharacterFromPSQL conn id
          ListCharacters -> do
            conn <- PI.input
            embed $ listCharactersFromPSQL conn
          
      

type CharacterTable = Table (Maybe (Column PGUuid), Column PGText, Column PGText) (Column PGUuid, Column PGText, Column PGText)

characterTable ::
      CharacterTable
characterTable =
  Table
    "characters"
    (p3 (optional "id", required "name", required "description"))

selectAllRows :: Connection -> IO [(UUID, String, String)]
selectAllRows conn = runQuery conn $ queryTable characterTable

addCharacterToPSQL :: Connection -> CharacterForm -> IO Character
addCharacterToPSQL conn (CharacterForm name desc) =
  (character . head) <$> runInsert_ conn (Insert characterTable [toFields (uuid, name, desc)] (rReturning (\(id, name, desc) -> (id,name,desc))) Nothing)
  where
      uuid :: Maybe UUID
      uuid = Nothing


selectCharacterByIdFromPSQL :: Connection -> UUID -> IO [(UUID, String, String)]
selectCharacterByIdFromPSQL conn key =  runSelect conn $
  proc () ->
  do row@(id, _, _) <- queryTable characterTable -< ()
     restrict -< (id .== constant key)
     returnA -< row

getCharacterFromPSQL :: Connection -> UUID -> IO Character
getCharacterFromPSQL conn id = (character . head) <$> selectCharacterByIdFromPSQL conn id

character :: (UUID, String, String) -> Character
character (id, name, desc) = Character id name desc

listCharactersFromPSQL :: Connection -> IO [Character]
listCharactersFromPSQL conn =  map character <$> selectAllRows conn
  
updateRow :: Connection -> (Maybe UUID, String, String) -> IO ()
updateRow conn row@(key, name, email) = do
  runUpdate
    conn
    characterTable
    (\_ -> constant row)
    (\(k, _, _) -> case key of
        Just key' -> k .== constant key'
        Nothing   -> pgBool False )
  return ()
