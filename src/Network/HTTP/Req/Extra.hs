module Network.HTTP.Req.Extra where

import           Data.ByteString.Lazy           ( ByteString )
import qualified Data.ByteString.Lazy          as BL
import           Data.Char
import           Network.HTTP.Req
import           Data.Aeson                    as A
import qualified Network.HTTP.Client           as L
import qualified Network.HTTP.Client.TLS       as L
import           Data.Proxy
import           Control.Exception

newtype JsonPResponse a = JsonPResponse (L.Response a)

instance FromJSON a => HttpResponse (JsonPResponse a) where
  type HttpResponseBody (JsonPResponse a) = a
  toVanillaResponse (JsonPResponse r) = r
  getHttpResponse r = do
    jsonResponse <- (convertJSONPToJSON . BL.fromChunks)
      <$> L.brConsume (L.responseBody r)
    case A.eitherDecode jsonResponse of
      Left  e -> throwIO (JsonHttpException e)
      Right x -> return $ JsonPResponse (x <$ r)
   where
    convertJSONPToJSON :: ByteString -> ByteString
    convertJSONPToJSON =
      BL.tail . BL.init . BL.dropWhile (\x -> x /= (fromIntegral (ord '(')))
  acceptHeader Proxy = Just "application/json"


-- | Use this as the fourth argument of 'req' to specify that you want it to
-- return the 'JsonResponse' interpretation.

jsonPResponse :: Proxy (JsonPResponse a)
jsonPResponse = Proxy
