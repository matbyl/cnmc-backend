module Api
  ( app
  )
where

import DAL
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Text
import           Data.Time                      ( UTCTime )
import           Database.PostgreSQL.Simple
import           Servant
import           Servant.API
import           Data.UUID
import           Data.Function                  ( (&) )
import qualified Polysemy                      as P
import           Polysemy                       ( Embed
                                                , Member
                                                , Members
                                                , Sem
                                                )
import qualified Polysemy.Input                as PI
import           Polysemy.Embed                 ( runEmbedded )


import Domain.Model


type GetCharacter = "characters" :> Capture "id" UUID :> Get '[JSON] Character
type ListCharacters = "characters" :> Get '[JSON] [Character]
type AddCharacter
  = "characters" :> ReqBody '[ JSON] CharacterForm :> Post '[JSON] Character

type ListCountries = "countries" :> Get '[JSON] [Country]

type API = ListCharacters :<|> AddCharacter :<|> GetCharacter :<|> ListCountries

server :: Connection -> Server API
server conn = listH :<|> addCharacterH :<|> getCharacterH :<|> listCountriesH
 where
  listH = liftIO $ runAllEffects conn listCharacters
  getCharacterH id = liftIO $ runAllEffects conn $ getCharacter id
  addCharacterH form = liftIO $ runAllEffects conn $ addCharacter form
  listCountriesH = liftIO $ runAllEffects conn $ listCountries 

characterAPI :: Proxy API
characterAPI = Proxy

app :: Connection -> Application
app = serve characterAPI . server

runAllEffects
  :: Connection -> (forall  r . Member DAL r => Sem r a) -> IO a
runAllEffects conn program =
  program
    & runDAL       -- [Input Connection, Embed IO]
    & PI.runInputConst conn -- [Ember IO]
    & P.runM
