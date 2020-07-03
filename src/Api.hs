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
--
-- Actors
--
type AddActor = "actors" :> ReqBody '[JSON] ActorForm :> Post '[JSON] Actor

type GetActor = "actors" :> Capture "id" UUID :> Get '[JSON] Actor

type ListActors = "actors" :> Get '[JSON] [Actor]

--
-- Characters
--
type GetCharacter = "characters" :> Capture "id" UUID :> Get '[JSON] Character

type ListCharacters = "characters" :> Get '[JSON] [Character]

type AddCharacter
  = "characters" :> ReqBody '[JSON] [CharacterForm] :> Post '[JSON] [Character]
--
-- Countries
--
type ListCountries = "countries" :> Get '[JSON] [Country]

--
-- Work
--
type AddWork = "work" :> ReqBody '[JSON] [WorkForm] :> Post '[JSON] [Work]

type ListWork = "work" :> Get '[JSON] [Work]


type ListMedium = "medium" :> Get '[JSON] [Medium]

type ListGenre = "genre" :> Get '[JSON] [Genre]

type CNMCAPI
  = AddActor :<|> GetActor :<|> ListActors :<|> ListCharacters :<|> AddCharacter :<|> GetCharacter :<|> ListCountries :<|> AddWork :<|> ListWork :<|> ListMedium :<|> ListGenre

type SwaggerAPI = "swagger.json" :> Get '[JSON] Swagger

type API = SwaggerAPI :<|> CNMCAPI

server :: Connection -> Server API
server conn =
  (return cnmcSwagger)
    :<|> addActorH
    :<|> getActorH
    :<|> listActorsH
    :<|> listCharactersH
    :<|> addCharacterH
    :<|> getCharacterH
    :<|> listCountriesH
    :<|> addWorkH
    :<|> listWorkH
    :<|> listMediumH
    :<|> listGenreH
 where
  addActorH form = liftIO $ addActorToDB conn form
  getActorH id = liftIO $ getActorFromDB conn id
  listActorsH     = liftIO $ listActorsFromDB conn
  listCharactersH = liftIO $ listCharactersFromDB conn
  getCharacterH id = liftIO $ getCharacterFromDB conn id
  addCharacterH form = liftIO $ addCharactersToDB conn form
  listCountriesH = liftIO $ listCountriesFromDB conn
  addWorkH form = liftIO $ addWorkToDB conn form
  listWorkH   = liftIO $ listWorkFromDB conn
  listMediumH = liftIO $ listMediumFromDB conn
  listGenreH  = liftIO $ listGenreFromDB conn


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
    ?~ "An API over all competent non male characters"
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
