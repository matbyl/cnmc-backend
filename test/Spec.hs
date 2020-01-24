import           Test.Hspec
import           Heroku
import           Data.Map.Strict               as M

main :: IO ()
main = hspec $ describe "Heroku.parseDatabaseUrl" $ do
  it "Returns the host of the url"
    $          (M.lookup "host" params)
    `shouldBe` (Just "host")
  it "Returns the port of the url"
    $          (M.lookup "port" params)
    `shouldBe` (Just "5432")
  it "Returns the password of the url"
    $          (M.lookup "password" params)
    `shouldBe` (Just "password")
  it "Returns the dbname of the url"
    $          (M.lookup "dbname" params)
    `shouldBe` (Just "dbname")
  it "Returns the user of the url"
    $          (M.lookup "user" params)
    `shouldBe` (Just "user")
  where params = parseDatabaseUrl "postgres://user:password@host:5432/dbname"
