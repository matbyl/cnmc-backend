module Database.PostgreSQL.Simple.FromField.Extra where

import qualified Data.ByteString as BS
import Database.PostgreSQL.Simple.FromField
import Data.Typeable

enumPGFromField :: Typeable a => BS.ByteString -> (BS.ByteString -> Maybe a) -> FieldParser a
enumPGFromField name mapper f mdata = do
    n <- typename f
    if n /= name
    then returnError Incompatible f ""
    else case mdata of
        Nothing -> returnError UnexpectedNull f ""
        Just bs -> case mapper bs of
            Nothing -> returnError ConversionFailed f (show bs)
            Just x  -> return x
