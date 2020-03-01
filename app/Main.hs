module Main where

import           Api
import           Control.Monad.IO.Class
import           Database.PostgreSQL.Simple
import           Debug.Trace
import           Network.Wai.Handler.Warp
import           Network.Wai.Logger             ( ApacheLogger
                                                , withStdoutLogger
                                                )
import           Heroku
import           Control.Exception
import qualified Data.Map.Strict               as Map
import           Data.Maybe
import           Text.Read
import           Config


main :: IO ()
main = withStdoutLogger $ \aplogger -> do
  envConfig <- getEnvironment
  let
    settings =
      setLogger aplogger . setPort (appPort envConfig) . setBeforeMainLoop
        (do
          putStrLn ""
          putStrLn "--------------------------------------------------------"
          putStrLn $ "\t Running CNMC-DB on: http://localhost:" ++ show
            (appPort envConfig)
          putStrLn "--------------------------------------------------------"
          putStrLn ""
        )
  let params = parseDatabaseUrl (pgDatabaseUrl envConfig)

  conn <- connect ConnectInfo
    { connectHost     = fromMaybe "localhost" $ Map.lookup "host" params
    , connectPort     = maybe 5432 read $ Map.lookup "port" params
    , connectDatabase = fromMaybe "localhost" $ Map.lookup "dbname" params
    , connectPassword = fromMaybe "localhost" $ Map.lookup "password" params
    , connectUser     = fromMaybe "localhost" $ Map.lookup "user" params
    }
  runSettings (settings defaultSettings) (app conn)
