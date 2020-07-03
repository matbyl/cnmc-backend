{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module DAL.Medium
  ( listMediumFromDB
  , PGMedium
  )
where

import           Control.Arrow
import           Control.Monad
import           Data.Aeson
import           Data.Coerce
import           Data.Profunctor.Product        ( p5 )
import           Data.Time
import           Data.UUID
import           Database.PostgreSQL.Simple
import           GHC.Generics
import           GHC.Int                        ( Int64 )
import           Opaleye                 hiding ( FromField )
import           Opaleye.EnumMapper
import qualified Database.PostgreSQL.Simple.FromField
                                               as PG
import qualified Database.PostgreSQL.Simple.FromRow
                                               as PG
import           Data.Typeable
import           Data.Profunctor.Product.Default
import           Domain.Model.Work
import           Database.PostgreSQL.Simple.FromField
                                         hiding ( tableColumn
                                                , Field
                                                , field
                                                )
import qualified Data.ByteString               as BS
import           Data.Char                      ( chr )
import           Database.PostgreSQL.Simple.FromField.Extra

data PGMedium

mediumMapper :: EnumMapper Medium PGMedium
mediumMapper = EnumMapper
  { enumToHaskell = mediumEnumtoHaskell
  , haskellToEnum = \s -> case s of
                      Television -> "television"
                      Film       -> "film"
  }


instance Default Constant Medium (Column PGMedium) where
  def = constantEnum mediumMapper "medium"

instance FromField Medium where
  fromField = fromFieldEnum mediumMapper

instance QueryRunnerColumnDefault PGMedium Medium where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

instance FromRow Medium where
  fromRow =
    PG.fieldWith
      $ enumPGFromField "medium"
      $ mediumEnumtoHaskell
      . map (chr . fromEnum)
      . BS.unpack

mediumEnumtoHaskell :: String -> Maybe Medium
mediumEnumtoHaskell bs = case bs of
  "television" -> Just Television
  "film"       -> Just Film
  _            -> Nothing


listMediumFromDB :: Connection -> IO [Medium]
listMediumFromDB conn = query_ conn "SELECT unnest(enum_range(NULL::medium))"
