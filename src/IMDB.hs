module IMDB where

import           Character                      ( Character )
import           Polysemy                       ( interpret
                                                , Sem
                                                , Member
                                                )
import qualified Polysemy                      as P
import           Polysemy.Embed                 ( Embed
                                                , embed
                                                )
import           Data.Aeson
import           Network.HTTP.Req
import           Control.Monad.IO.Class
import           Network.HTTP.Req
import           Control.Monad.IO.Class
import           Network.HTTP.Req.Extra         ( jsonPResponse )
import           Data.Aeson
import           GHC.Generics

data IMDBMovie = IMDBMovie
  { name :: String }
  deriving (Eq, Show, Generic, FromJSON)

data IMDB m a where
  ListCharacterMovies ::IMDB m [IMDBMovie]

P.makeSem ''IMDB

runIMDB :: Member (Embed IO) r => Sem (IMDB : r) a -> Sem r a
runIMDB = interpret $ \case
  ListCharacterMovies -> undefined
    -- runReq defaultHttpConfig $ do
    -- r <- req
    --   GET
    --   (https "sg.media-imdb.com" /: "suggests" /: "s" /: "star-wars.json")
    --   NoReqBody
    --   jsonPResponse
    --   mempty
    -- return $ (responseBody r :: [IMDBMovie])
