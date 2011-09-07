module Download where

import Data.ByteString.Lazy
import Data.Text.Lazy
import Data.Text.Lazy.Encoding
import Network.HTTP.Enumerator

download :: String -> IO Text
download url = do
  page <- simpleHttp url
  return (decodeUtf8 page)
