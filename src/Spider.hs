import Download
import Links
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set

type URL = String

spider :: Int -> URL -> IO (Map URL [URL])
spider count url0 = go 0 Map.empty (Set.singleton url0)
  where
    go k seen queue0
        | k >= count = return seen
        | otherwise  =
      case Set.minView queue0 of
        Nothing -> return seen
        Just (url, queue) -> do
          page <- download url
          let ls = links url page
              newSeen = Map.insert url ls seen
              newQueue = queue `Set.union`
                         Set.fromList (filter (`Map.notMember` newSeen) ls)
          go (k+1) newSeen newQueue
