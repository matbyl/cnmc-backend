module Main where

import           Api
import           Control.Monad.IO.Class
import           Database.PostgreSQL.Simple
import           Debug.Trace
import           Network.Wai.Handler.Warp
import           Network.Wai.Logger         (ApacheLogger, withStdoutLogger)
import           System.Environment

main :: IO ()
main =
  withStdoutLogger $ \aplogger -> do
    port <- read <$> getEnv "PORT"
    let settings =
          setLogger aplogger .
          setPort port .
          setBeforeMainLoop
            (do putStrLn ""
                putStrLn
                  "--------------------------------------------------------"
                putStrLn $
                  "\t Running CNMC-DB on: http://localhost:" ++ show port
                putStrLn
                  "--------------------------------------------------------"
                putStrLn "")
    conn <- connect ConnectInfo
        { connectHost = "localhost"
        , connectPort = 5432
        , connectDatabase = "cnmc"
        , connectPassword = "changeme"
        , connectUser = "postgres"
        }
    runSettings (settings defaultSettings) (app conn)
