module Api
  ( app
  )
where

import           Character
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Text
import           Data.Time                  (UTCTime)
import           Database.PostgreSQL.Simple
import           Servant
import           Servant.API


type ListCharacters = "characters" :> Get '[JSON] [Character]
type AddCharacter = "characters" :> ReqBody '[ JSON] CharacterForm :> Post '[JSON] ()
type API = ListCharacters :<|> AddCharacter

server :: Connection -> Server API
server conn = listH :<|> addCharacterH
  where
    listH = liftIO $ listCharacters conn
    addCharacterH form = liftIO $ addCharacter conn form

characterAPI :: Proxy API
characterAPI = Proxy

app :: Connection -> Application
app = serve characterAPI . server
