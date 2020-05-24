  module Config
  ( getEnvironment
  , PGConfig(..)
  )
where

import           System.Envy                   as Envy
import           GHC.Generics
import           Control.Monad.IO.Class

data PGConfig = PGConfig {
    appPort :: Int -- "APP_PORT"
  , pgDatabaseUrl :: String -- "PG_DATABASE_URL"

  } deriving (Generic, Show)

instance DefConfig PGConfig where
  defConfig = PGConfig
    { pgDatabaseUrl = "postgres://postgres:postgres@localhost:8321/cnmc"
    , appPort       = 4242
    }


instance FromEnv PGConfig

getEnvironment :: IO PGConfig
getEnvironment = decodeWithDefaults defConfig
