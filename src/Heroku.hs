module Heroku
  ( dbConnParams
  , parseDatabaseUrl
  )
where

import           Data.Map.Strict
import           Data.Text
import           System.Environment
import           Network.URI
import           Data.Tuple.Extra

dbConnParams :: IO (Map String String)
dbConnParams = dbConnParams' "DATABASE_URL" parseDatabaseUrl

parseDatabaseUrl :: String -> Map String String
parseDatabaseUrl = fromList . parseDatabaseUrl' "postgres:"

dbConnParams'
  :: String -> (String -> Map String String) -> IO (Map String String)
dbConnParams' envVar parse = getEnv envVar >>= return . parse

parseDatabaseUrl' :: String -> String -> [(String, String)]
parseDatabaseUrl' scheme durl =
  let muri         = parseAbsoluteURI durl
      (auth, path) = case muri of
        Nothing  -> error "couldn't parse absolute uri"
        Just uri -> if uriScheme uri /= scheme
          then schemeError uri
          else case uriAuthority uri of
            Nothing -> invalid
            Just a  -> (a, uriPath uri)
      (user, password) = userAndPassword auth
  in  [ ( "user"
        , user
        )
           -- tail not safe, but should be there on Heroku
      , ("password", Prelude.tail password)
      , ("host"    , uriRegName auth)
      , ( "port"
        , removeColon $ uriPort auth
        )
         -- tail not safe but path should always be there
      , ("dbname", Prelude.tail $ path)
      ]
 where
  removeColon (':' : port) = port
  removeColon port         = port

  -- init is not safe, but should be there on Heroku
  userAndPassword :: URIAuth -> (String, String)
  userAndPassword =
    both unpack . (breakOn $ pack ":") . pack . Prelude.init . uriUserInfo

  schemeError uri =
    error
      $  "was expecting a postgres scheme, not: "
      ++ (uriScheme uri)
      ++ "\n"
      ++ (show uri)
  -- should be an error
  invalid = error "could not parse heroku DATABASE_URL" scheme
