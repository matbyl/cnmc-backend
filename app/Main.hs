module Main where

import           Api
import           Control.Monad.IO.Class
import           Database.PostgreSQL.Simple
import           Debug.Trace
import           Network.Wai.Handler.Warp
import           Network.Wai.Logger             ( ApacheLogger
                                                , withStdoutLogger
                                                )
import           System.Environment
import           Heroku
import qualified Data.Map.Strict               as Map
import           Data.Maybe
import           Text.Read
main :: IO ()
main = withStdoutLogger $ \aplogger -> do
  port <- read <$> getEnv "PORT"
  let
    settings = setLogger aplogger . setPort port . setBeforeMainLoop
      (do
        putStrLn ""
        putStrLn "--------------------------------------------------------"
        putStrLn $ "\t Running CNMC-DB on: http://localhost:" ++ show port
        putStrLn "--------------------------------------------------------"
        putStrLn ""
      )
  params <- dbConnParams

  conn   <- connect ConnectInfo
    { connectHost     = fromMaybe "localhost" $ Map.lookup "host" params
    , connectPort     = maybe 5432 read $ Map.lookup "port" params
    , connectDatabase = fromMaybe "localhost" $ Map.lookup "dbname" params
    , connectPassword = fromMaybe "localhost" $ Map.lookup "password" params
    , connectUser     = fromMaybe "localhost" $ Map.lookup "user" params
    }
  runSettings (settings defaultSettings) (app conn)
