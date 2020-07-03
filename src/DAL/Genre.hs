{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module DAL.Genre
  ( listGenreFromDB
  , PGGenre
  )
where

import           Control.Arrow
import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString               as BS
import           Data.Char                      ( chr )
import           Data.Coerce
import           Data.Profunctor.Product        ( p5 )
import           Data.Profunctor.Product.Default
import           Data.Time
import           Data.Typeable
import           Data.UUID
import           Database.PostgreSQL.Simple
import qualified Database.PostgreSQL.Simple.FromField
                                               as PG
import           Database.PostgreSQL.Simple.FromField
                                         hiding ( Field
                                                , tableColumn
                                                )
import           Database.PostgreSQL.Simple.FromField.Extra
import qualified Database.PostgreSQL.Simple.FromRow
                                               as PG
import           Domain.Model.Work
import           GHC.Generics
import           GHC.Int                        ( Int64 )
import           Opaleye                 hiding ( FromField )
import           Opaleye.EnumMapper

data PGGenre

genreMapper :: EnumMapper Genre PGGenre
genreMapper = EnumMapper
  { enumToHaskell = genreEnumtoHaskell
  , haskellToEnum = \s -> case s of
                      Action -> "action"
                      Comedy -> "comedy"
  }

instance Default Constant Genre (Column PGGenre) where
  def = constantEnum genreMapper "genre"

instance FromField Genre where
  fromField = fromFieldEnum genreMapper

instance QueryRunnerColumnDefault PGGenre Genre where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

instance FromRow Genre where
  fromRow =
    PG.fieldWith
      $ enumPGFromField "genre"
      $ genreEnumtoHaskell
      . map (chr . fromEnum)
      . BS.unpack

genreEnumtoHaskell :: String -> Maybe Genre
genreEnumtoHaskell s = case s of
  "comedy"       -> Just Comedy
  "action"       -> Just Action
  "sci-fi"       -> Just SciFi
  "superhero"    -> Just Superhero
  "crime"        -> Just Crime
  "fantasy"      -> Just Fantasy
  "animated"     -> Just Animated
  "horror"       -> Just Horror
  "thriller"     -> Just Thriller
  "supernatural" -> Just Supernatural
  "drama"        -> Just Drama
  "dystopian"    -> Just Dystopian
  "zombie"       -> Just Zombie
  "historic"     -> Just Historic
  _              -> Nothing

listGenreFromDB :: Connection -> IO [Genre]
listGenreFromDB conn = query_ conn "SELECT unnest(enum_range(NULL::genre))"
