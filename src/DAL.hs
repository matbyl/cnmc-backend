module DAL where

import qualified Polysemy as P
import Polysemy (Members, Sem, interpret)
import Polysemy.Embed (Embed, embed)
import qualified Polysemy.Input as PI
import Data.UUID
import Database.PostgreSQL.Simple

import Domain.Model

import DAL.Actor
import DAL.Character
import DAL.Country

data DAL m a where
  -- Actor
  AddActor :: ActorForm -> DAL m Actor
  GetActor :: UUID -> DAL m Actor
  -- Character
  AddCharacter :: CharacterForm -> DAL m Character
  GetCharacter :: UUID -> DAL m Character
  ListCharacters :: DAL m [Character]
  -- Country
  ListCountries :: DAL m [Country]
  -- Work
  ListWork :: DAL m [Work]

P.makeSem ''DAL

runDAL :: (Members [Embed IO, PI.Input Connection] r) => Sem (DAL : r) a -> Sem r a
runDAL = interpret $ \case
  AddActor form -> do
    conn <- PI.input
    embed $ addActorToDB conn form
  GetActor id -> do
    conn <- PI.input
    embed $ getActorFromDB conn id
  AddCharacter form -> do
    conn <- PI.input
    embed $ addCharacterToDB conn form
  GetCharacter id -> do
    conn <- PI.input
    embed $ getCharacterFromDB conn id
  ListCharacters -> do
    conn <- PI.input
    embed $ listCharactersFromDB conn
  ListCountries -> do
    conn <- PI.input
    embed $ listCountriesFromDB conn
  ListWork -> undefined