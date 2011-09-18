{-# LANGUAGE BangPatterns, OverloadedStrings, RecordWildCards #-}

import Data.Bits ((.&.))
import Data.Function (on)
import MailRank.Functions (every)
import Data.Hashable (Hashable(..))
import qualified Data.HashMap.Strict as H
import Data.List (foldl')

data Link = Link {
      sender :: {-# UNPACK #-} !Int
    , recipient :: {-# UNPACK #-} !Int
    } deriving (Eq, Show)

instance Hashable Link where
    hash Link{..} = hash sender `hashWithSalt` recipient
    {-# INLINE hash #-}
    hashWithSalt s Link{..} =
        s `hashWithSalt` sender `hashWithSalt` recipient
    {-# INLINE hashWithSalt #-}

-- | This matrix maps pages to the pages they've linked to.  The outer
-- vector is indexed by page ID, and the inner contains the ID of
-- every page they've linked to.
type OutgoingLinks = [[Int]]

-- | This matrix maps pages to the pages they've been linked from.
-- The outer vector is indexed by page ID, and the inner contains
-- the ID of every page they've received from.
type IncomingLinks = [[Int]]

-- | Map from page ID to the reciprocal of the number of pages
-- they've linked to.
type LinkFactors = [Double]

-- | Indices of silent pages (those that have incoming links, but no
-- outgoing links).
type Silents = [Int]

transpose :: OutgoingLinks -> (IncomingLinks, LinkFactors, Silents)
transpose outgoingLinks = (incomingLinks, linkFactors, silent)
  where
    linkFactors = map (recip . fromIntegral . length) $
                     outgoingLinks
    silent = map fst . filter (null . snd) . imap (,) $
             outgoingLinks
    incomingLinks = generate outgoingLinks $ \i ->
                       maybe [] id $ H.lookup i incoming
      where incoming = ifoldl' step H.empty outgoingLinks
            step m0 i = foldl' (\m j -> H.insertWith (++) j [i] m) m0

data Rank = Rank {
      rankIter   :: {-# UNPACK #-} !Int
    , rankVector :: [Double]
    }

ranks :: IncomingLinks -> LinkFactors -> Silents -> Double
     -> [Rank]
ranks incoming factors silent alpha =
    iterate iter $ Rank 0 (replicate count (1/n))
  where
    iter (Rank k old0) = Rank (k+1) (map step incoming)
      where
        step link = h + a + i
          where
            h | null link = 0
              | otherwise   = alpha * backpermute old link `dot`
                                      backpermute factors link
        i = (1 - alpha) * sum old / n
        a | null silent = 0
          | otherwise     = alpha * sum (backpermute old silent) / n
        old | k .&. 16 == 15 = map (/ sum old0) old0
            | otherwise      = old0
    count = length factors
    n = fromIntegral count

rank :: OutgoingLinks -> Double -> Double -> Rank
rank outgoing alpha epsilon = snd . head . filter ((< epsilon * n) . fst) .
                              take 8 . every 10 . zipWith dist xs . tail $ xs
  where
    (incoming, factors, silent) = transpose outgoing
    dist a b = ((distance `on` rankVector) b a, b)
    xs = ranks incoming factors silent alpha
    n  = fromIntegral (length incoming)

distance :: [Double] -> [Double] -> Double
distance a b = sqrt (d `dot` d)
    where d = zipWith (-) a b

dot :: [Double] -> [Double] -> Double
dot a b = sum (zipWith (*) a b)

backpermute :: [a] -> [Int] -> [a]
backpermute xs is = map (xs!!) is

imap :: (Int -> a -> b) -> [a] -> [b]
imap f = go 0
    where go _  []     = []
          go !i (x:xs) = f i x : go (i+1) xs

generate :: [b] -> (Int -> a) -> [a]
generate xs f = imap (\i _ -> f i) xs

ifoldl' :: (a -> Int -> b -> a) -> a -> [b] -> a
ifoldl' f z0 = go z0 0
    where go z !i (x:xs) = let !z' = f z i x
                           in go z' (i+1) xs
          go z _ _       = z
