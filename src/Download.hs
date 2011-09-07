module Download where

import Network.HTTP.Enumerator

download url = simpleHttp url
