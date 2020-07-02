module Api
  ( app
  )
where

import           DAL
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Text
import           Data.Time                      ( UTCTime )
import           Database.PostgreSQL.Simple
import           Servant
import           Servant.API
import           Servant.Swagger                ( toSwagger )
import           Data.UUID
import           Data.Function                  ( (&) )
import qualified Data.ByteString.Lazy.Char8    as BL8
import           Domain.Model
import           Data.Swagger

import           Control.Lens
import           Data.Aeson.Encode.Pretty       ( encodePretty )

type GetCharacter = "characters" :> Capture "id" UUID :> Get '[JSON] Character
type ListCharacters = "characters" :> Get '[JSON] [Character]
type AddCharacter
  = "characters" :> ReqBody '[ JSON] CharacterForm :> Post '[JSON] Character

type ListCountries = "countries" :> Get '[JSON] [Country]

type ListWork = "work" :> Get '[JSON] [Work]

type ListMedium = "medium" :> Get '[JSON] [Medium]

type ListGenre = "genre" :> Get '[JSON] [Genre]

type CNMCAPI
  = ListCharacters :<|> AddCharacter :<|> GetCharacter :<|> ListCountries :<|> ListWork :<|> ListMedium :<|> ListGenre

type SwaggerAPI = "swagger.json" :> Get '[JSON] Swagger

type API = SwaggerAPI :<|> CNMCAPI

server :: Connection -> Server API
server conn =
  (return cnmcSwagger)
    :<|> listCharactersH
    :<|> addCharacterH
    :<|> getCharacterH
    :<|> listCountriesH
    :<|> listWorkH
    :<|> listMediumH
    :<|> listGenreH
 where
  listCharactersH = liftIO $ listCharactersFromDB conn
  getCharacterH id = liftIO $ getCharacterFromDB conn id
  addCharacterH form = liftIO $ addCharacterToDB conn form
  listCountriesH = liftIO $ listCountriesFromDB conn
  listWorkH      = liftIO $ listWorkFromDB conn
  listMediumH    = liftIO $ listMediumFromDB conn
  listGenreH     = liftIO $ listGenreFromDB conn


-- | Swagger spec for Todo API.
cnmcSwagger :: Swagger
cnmcSwagger =
  toSwagger cnmcAPI
    &  info
    .  title
    .~ "CNMC API"
    &  info
    .  version
    .~ "1.0"
    &  info
    .  description
    ?~ "This is an API over all competent non male characters"
    &  info
    .  license
    ?~ ("MIT" & url ?~ URL "http://mit.com")


cnmcAPI :: Proxy CNMCAPI
cnmcAPI = Proxy

api :: Proxy API
api = Proxy

app :: Connection -> Application
app = serve api . server

-- | Output generated @swagger.json@ file for the @'TodoAPI'@.
writeSwaggerJSON :: IO ()
writeSwaggerJSON =
  BL8.writeFile "example/swagger.json" (encodePretty cnmcSwagger)
