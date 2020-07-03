module Opaleye.EnumMapper where

import           Data.Typeable
import           Opaleye
import           Database.PostgreSQL.Simple.FromField
                                         hiding ( tableColumn )
import           Database.PostgreSQL.Simple     ( Connection )
import           Data.Profunctor
import           Data.Profunctor.Product.Default
import           Data.Profunctor.Product.TH
import           Data.Int                       ( Int64 )
import           Data.ByteString.Char8          ( unpack )

data EnumMapper a sqlType = EnumMapper {
    enumToHaskell :: String -> Maybe a
  , haskellToEnum :: a -> String
}

fromFieldEnum :: Typeable a => EnumMapper a pgType -> FieldParser a
fromFieldEnum s2h f mdata = case mdata of
  Just bStr ->
    let str = unpack bStr
    in  case enumToHaskell s2h str of
          Just h -> return h
          Nothing ->
            returnError ConversionFailed f ("Unexpected enum value: " ++ str)
  Nothing -> returnError ConversionFailed f "Unexpected empty value"

constantEnum :: EnumMapper a sqlType -> String -> Constant a (Column sqlType)
constantEnum h2s enumType = dimap (haskellToEnum h2s)
                                  (unsafeCast enumType)
                                  def_
 where
  def_ :: Constant String (Column PGText)
  def_ = def
