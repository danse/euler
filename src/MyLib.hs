{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE TupleSections #-}
module MyLib

where

import Control.Monad (guard)
import Data.Bits (xor)
import Data.Char (ord, chr)
import Data.Either (isLeft, lefts, rights)
import Data.Function ((&), on)
import Data.List (sort, sortOn, maximumBy, delete, nub, permutations, (\\), foldl', find)
import Data.List.Extra (maximumOn)
import Data.List.NonEmpty (nonEmpty)
import Data.Map (Map)
import Data.Maybe (mapMaybe, isNothing, isJust, fromMaybe)
import Data.Ord (Down(..))
import Data.Ratio (numerator, denominator)
import Data.Set (Set)
import Data.Time.Calendar (fromGregorian, dayOfWeek, DayOfWeek(..), toGregorian)
import Numeric (showBin)
import Safe (headMay)
import Text.ParserCombinators.ReadP (char, munch, between, sepBy, readP_to_S, ReadP)
import Text.Read (readMaybe)

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List.NonEmpty as NonEmpty

cyclicalFigurateNumbers :: [String]
cyclicalFigurateNumbers =
  let
    triangular n = n*(n+1) `div` 2
    square     n = n^(2::Int)
    pentagonal n = n*(3*n-1) `div` 2
    hexagonal  n = n*(2*n-1)
    heptagonal n = n*(5*n-3) `div` 2
    octagonal  n = n*(3*n-2)
    f :: (Int -> Int) -> Map String [String]
    f g =
      let fourDigits = takeWhile (<10000) . dropWhile (<1000)
            . fmap g $ [1..]
          s :: Int -> (String, String)
          s = splitAt 2 . show
          i :: (String, String) -> Map String [String] -> Map String [String]
          i (k, v) m = Map.insertWith (<>) k [v] m
      in foldr i mempty . fmap s $ fourDigits
    functions = [triangular, square, pentagonal, hexagonal, heptagonal, octagonal]
    maps = f <$> functions
    perms = permutations maps
    cycles = cycleMaps <$> perms
  in maximumOn length cycles

cycleMaps :: [Map String [String]] -> [String]
cycleMaps [] = []
cycleMaps (mh':rest) =
  let
    f :: String -> String -> [Map String [String]] -> [String]
    f _ _ [] = [] -- should never occurr
    f base t [aMap] =
      case Map.lookup t aMap of
        Just l -> if base `elem` l
                  then t:[base]
                  else []
        Nothing -> []
    f base t (mh:mt) =
      case Map.lookup t mh of
        Just l ->
          let g e = f base e mt
          in t:(maximumOn length $ fmap g l)
        Nothing -> []
    h :: (String, [String]) -> [[String]]
    h (k, v) = fmap (\x -> f k x rest) v
  in maximumOn length . foldMap h . Map.assocs $ mh'

-- m=2000 is enough
primePairAdjacency :: Int -> Map (Set Integer) (Set Integer)
primePairAdjacency m =
  let p = take m eratosthenes
      r a b = read (show a <> show b)
      t a b = millerRabin (r a b) && millerRabin (r b a)
      f
        :: Map (Set Integer) (Set Integer)
        -> [Integer]
        -> Map (Set Integer) (Set Integer)
      f a [] = a
      f a (ph:pt) =
        let g = filter (t ph) $ dropWhile (<ph) p
        in f (Map.insert (Set.singleton ph) (Set.fromList g) a) pt
  in f mempty p

primePairAggregation
  :: Map (Set Integer) (Set Integer)
  -> Map (Set Integer) (Set Integer)
primePairAggregation adjacency =
  let
    -- lookup is always successful because values are sets of primes
    l :: Integer -> Set Integer
    l j = fromMaybe mempty $ Map.lookup (Set.singleton j) adjacency
    g :: (Set Integer, Set Integer) -> [(Set Integer, Set Integer)]
    g (k, v) =
      let f ve =
            ( Set.insert ve k
            , Set.intersection v (l ve)
            )
      in f <$> Set.toList v
    adjacency' = Map.fromList . foldMap g $ Map.assocs adjacency
  in Map.union adjacency' adjacency

xorDecryption :: IO Int
xorDecryption = do
  c <- readFile "0059_cipher.txt"
  let numbers = fmap read $ parseFile59 c
      key = fmap ord "exp"
      d = zipWith xor numbers (cycle key)
  pure . sum $ d

findKey :: IO [Int]
findKey = do
  c <- readFile "0059_cipher.txt"
  let numbers = fmap read $ parseFile59 c
      keys :: [[Int]]
      keys = do
        k1 <- [97..122]
        k2 <- [97..122]
        k3 <- [97..122]
        pure $ k1:k2:k3:[]
      vowels key =
        let d = fmap chr $ zipWith xor numbers (cycle key)
            p r = r == 'a' || r == 'e' || r == 'i' || r == 'o' || r == 'u'
        in length . filter p $ d
  pure $ maximumBy (compare `on` vowels) keys  

parseFile59 :: String -> [String]
parseFile59 =
  let p :: ReadP [String]
      p = sepBy (munch (/=',')) (char ',')
  in fst . last . readP_to_S p

spiralPrimes :: Int
spiralPrimes =
  let layer :: SpiralPrimes -> SpiralPrimes
      layer s =
        let size = s.size + 2
            step = size - 1
            d1 = s.start + step
            d2 = d1 + step
            d3 = d2 + step
            d4 = d3 + step
            layerPrimes = length
              $ filter (isPrime . fromIntegral) [d1, d2, d3, d4]
        in SpiralPrimes
           (s.primes + layerPrimes)
           (s.total + 4)
           d4
           size
      -- | Iterate until a predicate is met
      i :: (a -> a) -> a -> (a -> Bool) -> a
      i f a p =
        let r = f a
        in if p r
           then r
           else i f r p
      predicate :: SpiralPrimes -> Bool
      predicate s = (fromIntegral s.primes / fromIntegral s.total) < (0.1 :: Double)
  in (\s -> s.size) $ i layer (SpiralPrimes 0 1 1 1) predicate

data SpiralPrimes = SpiralPrimes {
  primes :: Int,
  total :: Int,
  start :: Int,
  size :: Int }

squareRootConvergents :: Int -> Int
squareRootConvergents i =
  let c = approxSquareTwo <$> [1..i]
      f r = length (show (numerator r)) > length (show (denominator r))
  in length $ filter f c

approxSquareTwo :: Int -> Rational
approxSquareTwo = (1 +) . go
  where go 0 = 0
        go i = 1 / (2 + (go $ pred i))

powerfulDigitSum :: Int -> Int
powerfulDigitSum m =
  let c = do
        a <- [1..m]
        b <- [1..m]
        pure (a, b)
  in maximum $ fmap (uncurry powerDigitSum) c

powerDigitSum :: Int -> Int -> Int
powerDigitSum a b =
  let n = (fromIntegral a)^b :: Integer
  in digitSum n

lychrelNumbers :: Integer -> Either Integer Int
lychrelNumbers m =
  let l = lychrel <$> [1..m]
      t = lefts l
      b = nonEmpty t
  in case b of
    Just n -> Left (NonEmpty.head n)
    Nothing -> Right . length . filter id $ rights l

lychrel :: Integer -> Either Integer Bool
lychrel =
  let
    l m1 =
      case readMaybe . reverse $ show m1 of
        Nothing -> Left m1
        Just m2 -> Right (m1 + m2)
    go 0 _ = Right True
    go i m =
      case l m of
        Left m' -> Left m'
        Right b ->
          if isPalindrome b
          then Right False
          else go (i-1) b
  in go (50 :: Int)

pokerHands :: IO Int
pokerHands = do
  c <- readFile "0054_poker.txt"
  pure . length . filter (uncurry poker) $ parsePoker c
  where
    parsePoker :: String -> [([String], [String])]
    parsePoker = fmap f . lines
      where f l =
              let w = words l
              in (take 5 w, drop 5 w)

poker :: [String] -> [String] -> Bool
poker h1 h2 =
  let suit  = takeEnd 1
      value = k . take 1
        where
          k "A" = PA
          k "2" = P2
          k "3" = P3
          k "4" = P4
          k "5" = P5
          k "6" = P6
          k "7" = P7
          k "8" = P8
          k "9" = P9
          k "T" = PT
          k "J" = PJ
          k "Q" = PQ
          k "K" = PK
          k _ = error "value"
      handRank h
        | royalFlush h = (1 :: Int, Nothing)
        | straightFlush h = (2, Nothing)
        | isJust (fourOfAKind h) = (3, fourOfAKind h)
        | isJust (fullHouse h)   = (4, fullHouse h)
        | flush h                = (5, Nothing)
        | straight h             = (6, Nothing)
        | isJust (threeOfAKind h) = (7, threeOfAKind h)
        | twoPairs h              = (8, Nothing)
        | isJust (onePair h)      = (9, onePair h)
        | otherwise               = (10, Nothing)
        where
          flush = (1==) . length . nub . fmap suit
          royalFlush d = flush d && royal
            where royal = PT `elem` v
                    && PJ `elem` v
                    && PQ `elem` v
                    && PK `elem` v
                    && PA `elem` v
                  v = fmap value h
          straight d = (nub $ zipWith (-) v' v) == [1]
            where
              v' = drop 1 v
              v = sort $ fmap (fromEnum . value) d
          straightFlush d = flush d && straight d
          occurs i (_, j) = i == j
          fourOfAKind = find (occurs 4) . tally . fmap value
          fullHouse d =
            let t = tally $ fmap value d
            in if any (occurs 3) t && any (occurs 2) t
               then find (occurs 3) t
               else Nothing
          threeOfAKind = find (occurs 3) . tally . fmap value
          twoPairs = (2==) . length . filter (occurs 2) . tally . fmap value
          onePair = find (occurs 2) . tally . fmap value
      cardRank PA = 13
      cardRank o = fromEnum o
      (hr1, mCard1) = handRank h1
      (hr2, mCard2) = handRank h2
      tie =
        let one = (compare `on` (cardRank . fst)) <$> mCard1 <*> mCard2
        in if one == Just GT then True
           else if one == Just LT then False
                else highCard (cardRank . value <$> h1) (cardRank . value <$> h2)
      highCard hc1 hc2 =
        let
          c1 = maximum hc1
          c2 = maximum hc2
        in if c1 == c2
           then highCard (delete c1 hc1) (delete c2 hc2)
           else c1 > c2
  in if hr1 < hr2
     then True
     else if hr2 < hr1
          then False
          else tie
          
data PokerValue = PA |
  P2 |
  P3 |
  P4 |
  P5 |
  P6 |
  P7 |
  P8 |
  P9 |
  PT |
  PJ |
  PQ |
  PK deriving (Enum, Eq, Ord)

-- | Count occurrences of each unique item in a list.
--
-- >>> tally "abacaba" == [('a', 4), ('b', 2), ('c', 1)]
-- True
tally :: Ord a => [a] -> [(a, Int)]
tally = Map.toList . Map.fromListWith (+) . fmap ((, 1))

combinatoricSelections :: Int
combinatoricSelections = length . filter p $ foldMap f [1..100]
  where
    p = (1000000<)
    f n = fmap (binomialCoefficient n) [1..n]

binomialCoefficient :: Integer -> Integer -> Integer
binomialCoefficient n r = (factorial n) `div` (factorial r * factorial (n-r))

permutedMultiples :: Maybe Int
permutedMultiples =
  let predicate :: Int -> Bool
      predicate x =
        let g y = Set.fromList . show $ x * y
            x1 = g 1
            x2 = g 2
            x3 = g 3
            x4 = g 4
            x5 = g 5
            x6 = g 6
        in x1 == x2
           && x2 == x3
           && x3 == x4
           && x4 == x5
           && x5 == x6
  in headMay $ dropWhile (not . predicate) [1..]

primeDigitsReplacements :: Int -> Maybe [Int]
primeDigitsReplacements m = fmap sort . headMay . filter (not . null) $ fmap p [1..]
  where
    p :: Int -> [Int]
    p i =
      let k = masks (show i)
      in foldr g [] k
    g :: (Char -> String) -> [Int] -> [Int]
    g k b =
      case f k of
        Just b' -> b <> b'
        Nothing -> b
    f :: (Char -> String) -> Maybe [Int]
    f k =
      let allReplacements = fmap k "0123456789"
          replacements = fmap read $ filter c allReplacements
            where c n =
                    let h = headMay n
                    in if h == Just '0'
                       then False
                       else True
          d = filter (isPrime . fromIntegral) $ replacements
      in if length d >= m
         then Just d
         else Nothing

masks :: String -> [Char -> String]
masks s =
  let l = length s
      o :: [[Int]]
      o = foldMap (\h -> combinations [] h [0..l-1]) [1..l]
      g :: [Int] -> Char -> String
      g q c = foldr (update c) s q
  in fmap g o

update :: e -> Int -> [e] -> [e]
update e i l =
  let (l1, t) = case splitAt i l of
        (l1', _:t') -> (l1', t')
        _ -> error "list access"
  in l1 <> (e:t)

consecutivePrimeSum :: Integer -> (Int, Integer)
consecutivePrimeSum m =
  let
    ps = takeWhile (< m) primes
    prefixSums = scanl (+) 0 ps
    check len =
      let
        sums = zipWith (-) (drop len prefixSums) prefixSums
        go (s:ss)
          | s >= m     = Nothing
          | isPrime s  = Just (len, s)
          | otherwise  = go ss
        go [] = Nothing
      in go sums
  in
    case mapMaybe check [length ps, length ps - 1 .. 1] of
      (r:_) -> r
      []    -> (0, 0)

primePermutations :: [[Integer]]
primePermutations =
  let
    f digits =
      let
        p = permutations digits
        q = filter isPrime $ fmap read p
      in if length q > 2
         then Just q
         else Nothing
    e = DifferenceList mempty mempty
    g :: [Integer] -> [[Integer]]
    g = fmap nub . differenceListQuery 3 . foldr differenceListInsert e
  in nub . filter ((2<=) . length) . foldMap g . fmap sort . mapMaybe f
     $ combinations mempty 4 "111122223333444455556666777788889999"

differenceListQuery :: Int -> DifferenceList -> [[Integer]]
differenceListQuery q d = Map.elems $ Map.filterWithKey predicate d.d
  where
    predicate :: Integer -> [a] -> Bool
    predicate 0 = const False
    predicate _ = (q<=) . length
  
-- | Insert from a sorted list
differenceListInsert :: Integer -> DifferenceList -> DifferenceList
differenceListInsert n d =
  -- | Difference is the same, but insert only if the smaller integer
  -- is the same as the previous bigger integer
  let insertion
        :: [Integer] -- ^ new integers from bigger to smaller
        -> [Integer] -- ^ sequence from bigger to smaller
        -> [Integer]
      insertion [b, s] [] = [b, s]
      insertion [b, s] (h:t) =
        if s == h
        then b:h:t
        else h:t
      insertion _ _ = error "two to be added"
      folding
        :: Integer
        -> Map.Map Integer [Integer]
        -> Map.Map Integer [Integer]
      folding m p = Map.insertWith insertion (n-m) [n, m] p
      d' = foldr folding d.d d.l
      l' = n : d.l
  in DifferenceList l' d'

data DifferenceList = DifferenceList
  { l :: [Integer]
  , d :: Map.Map Integer [Integer]
  }

combinations :: [a] -> Int -> [a] -> [[a]]
combinations s n l =
  let
    c = case l of
      [] -> []
      (x:xs) ->
        let s' = x:s
        in combinations s' n xs <> combinations s n xs
  in if length s == n
     then [s]
     else c

takeEnd :: Int -> [a] -> [a]
takeEnd n l = drop (length l - n) l

selfPowers :: Integer -> String
selfPowers m =
  let
    folding :: Integer -> Integer -> Integer
    folding n a = a + n^n
  in takeEnd 10 . show $ foldr folding 0 [1..m]

distinctPrimesFactors :: Int -> Maybe (Maybe Int)
distinctPrimesFactors c =
  let
    hasThem = (c==) . Set.size . Set.fromList . primeFactors . fromIntegral
    countC :: ([Int], Maybe Int) -> Int -> ([Int], Maybe Int)
    countC (having, _) n =
      if hasThem n
      then if length having == c-1
           then ([], headMay having)
           else (having <> [n], Nothing)
      else ([], Nothing)
  in headMay . dropWhile isNothing . fmap snd $ scanl countC ([], Nothing) [1..]

goldbachConjecture :: Maybe (Maybe Int)
goldbachConjecture =
  let
    conjecture :: Int -> [Int] -> Bool
    conjecture n primes' =
      let
        remainder :: Int -> Bool
        remainder = isInt . sqrt . (/2) . fromIntegral
        test :: Int -> Int -> Bool
        test c p = remainder (c-p)
      in any (test n) primes'
    scanning :: (Maybe Int, [Int]) -> Int -> (Maybe Int, [Int])
    scanning (_, primes') n =
      if isPrime (fromIntegral n)
      then (Nothing, n:primes')
      else
        if not $ conjecture n primes'
        then (Just n, primes')
        else (Nothing, primes')
  in headMay . dropWhile isNothing . fmap fst
     . scanl scanning (Nothing, []) $ filter odd [3..]

triangularPentagonalAndExagonal :: Maybe Int
triangularPentagonalAndExagonal =
  let
    penta, exa :: Double -> Double
    penta x = (1/2 + sqrt (1/4+6*x))/3
    exa x = (1 + sqrt (1+8*x))/4
    t n = (n*(n+1)) `div` 2
    p :: Int -> Bool
    p x = (isInt . penta $ fromIntegral x)
      && (isInt . exa $ fromIntegral x)
  in headMay . filter p $ map t [286..]

isInt :: Double -> Bool
isInt n = fromIntegral (floor n :: Int) == n

pentagonNumbers :: Int -> Int
pentagonNumbers m =
  let
    p n = n*(3*n-1) `div` 2
    s = foldr (\n e -> Set.insert (p n) e) Set.empty [1..m]
    sumDiff :: [Int] -> Int -> [Int]
    sumDiff f r =
      let
        d x y =
          if (x-y) `Set.member` s && (x+y) `Set.member` s
          then Just (x-y)
          else Nothing
      in case mapMaybe (d r) (Set.toList s) of
           [] -> f
           t ->
             let h = minimum t
             in h:f
    u = foldl' sumDiff [] $ Set.toList s
  in minimum u

subStringDivisibility :: Int
subStringDivisibility =
  let p = permutations "0123456789"
  in sum . fmap read $ filter predicate43 p

predicate43 :: String -> Bool
predicate43 ('0':_) = False
predicate43 s =
     p 1 2 3 2
  && p 2 3 4 3
  && p 3 4 5 5
  && p 4 5 6 7
  && p 5 6 7 11
  && p 6 7 8 13
  && p 7 8 9 17
  where
    p :: Int -> Int -> Int -> Int -> Bool
    p i1 i2 i3 d = read (s !! i1:s !! i2:s !! i3:[]) `mod` d == 0

codedTriangleNumbers :: IO Int
codedTriangleNumbers =
  let
    triangleWord :: String -> Bool
    triangleWord = flip Set.member s . alphabeticalValue
    s = Set.fromList $ fmap t [1..1000]
      where t n = (n*(n+1)) `div` 2
  in do
    contents <- readFile "0042_words.txt"
    let w = fst . last . parseFile22 $ contents
    pure . length $ filter triangleWord w

pandigitalPrime :: Int -> Maybe Integer
pandigitalPrime m =
  let p = sortOn Down . fmap read . permutations $ foldMap show [1..m]
  in headMay $ filter isPrime p

integerRightTriangles :: Int -> Int
integerRightTriangles m = maximumBy (compare `on` triangleSolutions) [1..m]

champernowneProduct :: Int
champernowneProduct =
  let d = champernowneDigit
  in d 0 *
     d 9 *
     d 99 *
     d 999 *
     d 9999 *
     d 99999 *
     d 999999

champernowneDigit :: Int -> Int
champernowneDigit i = read . pure $ champernowneFractional !! i

champernowneFractional :: String
champernowneFractional =
  let n = [1..1000000] :: [Int]
      f [] = ""
      f (h:t) = show h <> f t
  in f n

triangleSolutions :: Int -> Int
triangleSolutions p =
  let
    predicate a b =
      let a' = fromIntegral a :: Double
          b' = fromIntegral b
          p' = fromIntegral p
      in 2*(a'+b') == p' + 2*a'*b'/p'
    o = do
      a <- [1..p]
      b <- [1..p-a]
      guard $ predicate a b
      pure $ Set.fromList [a, b]
  in length $ nub o

pandigitalMultiples :: Int -> Int -> Int
pandigitalMultiples m1 m2 =
  let c = do
        n1 <- [1..m1]
        m3 <- [1..m2]
        let n2 = takeWhile (<m3) [1..]
            mp = pandigitalConcatenatedProduct n1 n2
        case mp of
          Just p -> pure p
          Nothing -> []
  in maximum c

pandigitalConcatenatedProduct:: Int -> [Int] -> Maybe Int
pandigitalConcatenatedProduct i m =
  let p = foldMap (show . (i*)) m
      d = null (p \\ "123456789")
      l = length p == 9
      s = (Set.size $ Set.fromList p) == 9
  in if l && s && d
     then Just (read p)
     else Nothing

truncatablePrimes :: Int -> [Int]
truncatablePrimes m =
  let
      arePrimes :: (Int, [Int]) -> Bool
      arePrimes (n, t) =  isPrime (fromIntegral n)
        && all (isPrime . fromIntegral) (nub t)
  in fmap fst . filter arePrimes $ fmap (\n -> (n, truncations n)) [11..m]

truncations :: Int -> [Int]
truncations n =
  let s = show n
      v = reverse s
      t [] = []
      t (_:[]) = []
      t (_:l) = l:t l
      a = t s <> (reverse <$> t v)
  in read <$> a

doubleBasePalindromes :: Int -> Int
doubleBasePalindromes m =
  let predicate n =
        isPalindrome n && (isPalindrome $ showBin n "")
  in sum $ filter predicate [1..m]

circularPrimes :: Int -> Int
circularPrimes m =
  let
    cycles :: Int -> [Int]
    cycles n =
      let
        g 0 _ = []
        g _ [] = []
        g l (h:t) = (read $ t<>[h]) : g (l-1) (t<>[h])
        s = show n
      in g (length s) s
    predicate :: Int -> Bool
    predicate = all (isPrime . fromIntegral) . cycles
  in length $ filter predicate [2..(m-1)]

digitFactorials :: Int -> Int
digitFactorials m = sum . filter isDigitFactorial $ [10..m]

isDigitFactorial :: Int -> Bool
isDigitFactorial n =
  let t = fmap (factorial . read . pure) $ show n
  in sum t == fromIntegral n

digitCancellingFractions
  :: forall i
  .  (Eq i, Fractional i, Enum i, Show i, Read i)
  => [(i, i)]
digitCancellingFractions =
  let
    isDigitCancelling :: (i, i) -> Bool
    isDigitCancelling (n, d) =
      let
        f g b =
          let n' = read $ show n \\ pure g
              d' = read $ show d \\ pure g
              s = length (show n') < length (show n)
          in b || (n'/d' == n/d && s)
      in foldr f False "123456789"
    c = do
      n <- [10..98]
      d <- [n+1..99]
      pure (n, d)
  in filter isDigitCancelling c

pandigitalProducts :: Int
pandigitalProducts =
  let
    p = permutations "123456789"
    t = do
      a <- [1..7]
      b <- [1..(8-a)]
      q <- p
      f (a, b) q
    f (a, b) q =
      let (a', q') = splitAt a q
          (b', c') = splitAt b q'
          (x, y, z) = (read a', read b', read c')
      in if (x*y==z)
         then [z]
         else []
  in sum $ nub t

coinSums :: Int -> Int
coinSums m =
  let f (l2, l1, p50, p20, p10, p5, p2, p1) =
        let s = l2 * 200
              + l1 * 100
              + p50 * 50
              + p20 * 20
              + p10 * 10
              + p5 * 5
              + p2 * 2
              + p1
        in s == m
      c = do
        l2 <- [0..(m `div` 200)]
        l1 <- [0..(m `div` 100)]
        p50 <- [0..(m `div` 50)]
        p20 <- [0..(m `div` 20)]
        p10 <- [0..(m `div` 10)]
        p5 <- [0..(m `div` 5)]
        p2 <- [0..(m `div` 2)]
        p1 <- [0..m]
        guard $ f (l2, l1, p50, p20, p10, p5, p2, p1)
        pure ()
  in length c

fifthPowers :: Int -> Int
fifthPowers m = sum $ filter (isPowerOfDigits 5) [2..m]

isPowerOfDigits :: Int -> Int -> Bool
isPowerOfDigits p n =
  let digits :: [Int]
      digits = fmap (read . pure) $ show n
  in sum (fmap (^p) digits) == n

distinctPowers :: Integer -> Int
distinctPowers m =
  let f :: (Integer, Integer) -> Integer
      f (a, b) = a^b
      c = do
        a <- [2..m]
        b <- [2..m]
        pure (a, b)
  in length . nub $ fmap f c

spiralDiagonals :: Int -> [Int]
spiralDiagonals m =
  let go :: Int -> Int -> Int -> [Int]
      go side level start
        | side == 4 = d : go 1 (succ level) d
        | size > m = []
        | otherwise = d : go (succ side) level d
        where
          size = (level-1) * 2 + 1
          d = start+size-1
  in 1:go 1 2 1

quadraticPrimes :: Integer -> Integer
quadraticPrimes m =
  let c = do
        let m' = m-1
        a <- [(-m')..m']
        b <- [(-m)..m]
        pure (a, b)
      (x,y) = maximumBy (compare `on` (uncurry consecutivePrimes)) c
  in x*y

consecutivePrimes :: Integer -> Integer -> Int
consecutivePrimes a b =
  let f n = n^(2::Integer) + a*n + b
  in length . takeWhile (isPrime . abs . f) $ [0..]

reciprocalCycles :: Int -> Int
reciprocalCycles m =
  let c = [1..(m-1)]
      f = fmap (length . extractCycle) . periodicDiv [1] 1
  in maximumBy (compare `on` f) c

extractCycle :: Eq a => [a] -> [a]
extractCycle l =
  let go :: Eq a => [[a]] -> [a] -> [a]
      go _ [] = []
      go seen a@(h:t) =
        if a `elem` seen
        then a
        else go seen' t
        where seen' = [h] : fmap (<>[h]) seen
  in go [] l

periodicDiv :: [Int] -> Int -> Int -> Maybe [Int]
periodicDiv e n m =
  let n' = if n < m
           then n * 10
           else n
      (d, o) = n' `divMod` m
      c _ Nothing = Nothing
      c a (Just b) = Just (a <> b)
      count _ [] = 0
      count l (s:t) =
        if l == s
        then 1 + count l t
        else count l t
      r
        | count o e == (2::Int) = Just [d]
        | o == 0 = Nothing
        | otherwise = c [d] (periodicDiv (e <> [o]) o m)
  in r

fibonacciDigits :: Int -> Maybe (Either (Map.Map Int Integer) Int)
fibonacciDigits s =
  let
    f :: Map.Map Int Integer -> Int -> (Map.Map Int Integer, Integer)
    f m 1 = (m, 1)
    f m 2 = (m, 1)
    f m n = 
      case Map.lookup n m of
        Just v -> (m, v)
        Nothing ->
          let (m1, v1) = f m  (n-1)
              (m2, v2) = f m1 (n-2)
              m3 = Map.insert n (v1+v2) m2
          in (m3, v1+v2)
    p n = (length . show $ n) >= s
    scanning (Right v) _ = (Right v)
    scanning (Left m) i =
      let (m', v) = f m i
      in if p v
         then (Right i)
         else (Left m')
  in headMay . dropWhile isLeft $ scanl scanning (Left mempty) [1..]

lexicographicPermutations :: [Int] -> Int -> String
lexicographicPermutations options index' =
  let
    index = index' - 1
    o = sort $ fmap (chr . (48+)) options
  in perm o !! index

perm :: [Char] -> [String]
perm [] = []
perm [e] = [[e]]
perm ns' =
  let p :: Char -> [String]
      p e =
        let ns = delete e ns'
        in fmap (e:) $ perm ns
  in foldMap p ns'

nonAbundantSums :: Int
nonAbundantSums =
  let isAbundant n = sum (properDivisors n) > n
      m = 28123
      a = filter isAbundant [12..m]
      sums = Set.fromList $ do
        a1 <- a
        a2 <- a
        pure (a1 + a2)
      predicate = not . (`Set.member` sums)
  in sum $ filter predicate [1..m]

namesScores :: IO Int
namesScores = do
  contents <- readFile "0022_names.txt"
  let names = sort . fst . last . parseFile22 $ contents
      score :: (Int, String) -> Int
      score (i, s) = i * alphabeticalValue s
  pure . sum . fmap score $ zip [1..] names

parseFile22 :: String -> [([String], String)]
parseFile22 =
  let name = between (char '"') (char '"') (munch (/= '"'))
      p :: ReadP [String]
      p = sepBy name (char ',')
  in readP_to_S p

alphabeticalValue :: String -> Int
alphabeticalValue = sum . fmap ((subtract 64) . ord)

amicable :: Int -> Int
amicable x =
  let d = sumOfProperDivisors
      isAmicable n =
        let e = d n
        in d e == n && e < x && e /= n
  in sum . filter isAmicable $ [1..(x-1)]

sumOfProperDivisors :: Int -> Int
sumOfProperDivisors = sum . properDivisors

properDivisors :: Int -> [Int]
properDivisors n =
  let predicate m = (n `mod` m) == 0
  in filter predicate [1..(n-1)]

factorialDigitsSum :: Integer -> Int
factorialDigitsSum = digitSum . factorial

factorial :: Integer -> Integer
factorial 0 = 1
factorial 1 = 1
factorial n = n * factorial (pred n)

sundays :: Int
sundays =
  let
    s = fromGregorian 1901  1  1
    e = fromGregorian 2000 12 31
    days = takeWhile (/=e) $ enumFrom s
    firstSunday d =
      (dayOfWeek d == Sunday) &&
      (n (toGregorian d) == 1)
      where n (_, _, c) = c
  in length . filter firstSunday $ days  

maximumPathSum :: Int
maximumPathSum =
  let t :: [[Int]]
      t = [[75],
           [95, 64],
           [17, 47, 82],
           [18, 35, 87, 10],
           [20, 04, 82, 47, 65],
           [19, 01, 23, 75, 03, 34],
           [88, 02, 77, 73, 07, 63, 67],
           [99, 65, 04, 28, 06, 16, 70, 92],
           [41, 41, 26, 56, 83, 40, 80, 70, 33],
           [41, 48, 72, 33, 47, 32, 37, 16, 94, 29],
           [53, 71, 44, 65, 25, 43, 91, 52, 97, 51, 14],
           [70, 11, 33, 28, 77, 73, 17, 78, 39, 68, 17, 57],
           [91, 71, 52, 38, 17, 14, 91, 43, 58, 50, 27, 29, 48],
           [63, 66, 04, 68, 89, 53, 67, 30, 73, 16, 69, 87, 40, 31],
           [04, 62, 98, 27, 23, 09, 70, 98, 73, 93, 38, 53, 60, 04, 23]]
      g (point, paths) = [
        (point, point:paths),
        (point+1, point+1:paths)]
      p :: Int -> [(Int, [Int])]
      p 0 = [(0, [0])]
      p n = foldMap g $ p (n-1)
      allPaths = p 14
      access (_, path) =
        let fullPath = zip (reverse [0..14]) path
            values = fmap (\(x, y) -> (t !! x) !! y) fullPath
        in sum values
  in maximum . fmap access $ allPaths

numberLetterCount :: Int -> Int
numberLetterCount m =
  let n = [1..m]
  in sum . fmap numberLetterCount' $ n

numberLetterCount' :: Int -> Int
numberLetterCount' =
  let
    c :: String -> Int
    c = length . filter ((&&) <$> (/= ' ') <*> (/= '-'))
  in c . numberWords    

numberWords :: Int -> String
numberWords 1000 = "one thousand"
numberWords 1 = "one"
numberWords 2 = "two"
numberWords 3 = "three"
numberWords 4 = "four"
numberWords 5 = "five"
numberWords 6 = "six"
numberWords 7 = "seven"
numberWords 8 = "eight"
numberWords 9 = "nine"
numberWords 10 = "ten"
numberWords 11 = "eleven"
numberWords 12 = "twelve"
numberWords 13 = "thirteen"
numberWords 14 = "fourteen"
numberWords 15 = "fifteen"
numberWords m =
  let (h, r1) = m `divMod` 100
      (t, r2) = r1 `divMod` 10
      f :: String -> Int -> String
      f a b =
        if b == 0
        then a
        else a <> "-" <> numberWords b
      e 1 = \b ->
        if b == 8
        then "eighteen"
        else numberWords b <> "teen"
      e 2 = f "twenty"
      e 3 = f "thirty"
      e 4 = f "forty"
      e 5 = f "fifty"
      e 6 = f "sixty"
      e 7 = f "seventy"
      e 8 = f "eighty"
      e 9 = f "ninety"
      e o = const . error . show $ o
      v = if r1 < 16
          then numberWords r1
          else e t r2
  in if h > 0
     then if r1 > 0
          then numberWords h <> " hundred and " <> v
          else numberWords h <> " hundred"
     else v

digitSum :: Integral i => i -> Int
digitSum = sum . fmap ((subtract 48) . ord) . s . fromIntegral
  where s :: Integer -> String
        s = show

powerDigitSumTwo :: Int -> Int
powerDigitSumTwo = powerDigitSum 2

latticePaths :: Int -> Int -> Int
latticePaths _ 0 = 1
latticePaths 0 _ = 1
latticePaths x y =
  latticePaths (x-1) y + latticePaths x (y-1)

longestCollatz :: Int -> Int
longestCollatz s =
  let
    starting = [1..s]
    comparing = compare `on` (length . collatzSequence)
  in maximumBy comparing starting

collatzSequence :: Int -> [Int]
collatzSequence starting =
  let collatz n
        | even n = n `div` 2
        | otherwise = 3*n + 1
      chain 1 = [1]
      chain n =
        n:chain (collatz n)
  in chain starting

largeSum :: String
largeSum =
  let
    n :: [Integer]
    n = [ 37107287533902102798797998220837590246510135740250,
          46376937677490009712648124896970078050417018260538,
          74324986199524741059474233309513058123726617309629,
          91942213363574161572522430563301811072406154908250,
          23067588207539346171171980310421047513778063246676,
          89261670696623633820136378418383684178734361726757,
          28112879812849979408065481931592621691275889832738,
          44274228917432520321923589422876796487670272189318,
          47451445736001306439091167216856844588711603153276,
          70386486105843025439939619828917593665686757934951,
          62176457141856560629502157223196586755079324193331,
          64906352462741904929101432445813822663347944758178,
          92575867718337217661963751590579239728245598838407,
          58203565325359399008402633568948830189458628227828,
          80181199384826282014278194139940567587151170094390,
          35398664372827112653829987240784473053190104293586,
          86515506006295864861532075273371959191420517255829,
          71693888707715466499115593487603532921714970056938,
          54370070576826684624621495650076471787294438377604,
          53282654108756828443191190634694037855217779295145,
          36123272525000296071075082563815656710885258350721,
          45876576172410976447339110607218265236877223636045,
          17423706905851860660448207621209813287860733969412,
          81142660418086830619328460811191061556940512689692,
          51934325451728388641918047049293215058642563049483,
          62467221648435076201727918039944693004732956340691,
          15732444386908125794514089057706229429197107928209,
          55037687525678773091862540744969844508330393682126,
          18336384825330154686196124348767681297534375946515,
          80386287592878490201521685554828717201219257766954,
          78182833757993103614740356856449095527097864797581,
          16726320100436897842553539920931837441497806860984,
          48403098129077791799088218795327364475675590848030,
          87086987551392711854517078544161852424320693150332,
          59959406895756536782107074926966537676326235447210,
          69793950679652694742597709739166693763042633987085,
          41052684708299085211399427365734116182760315001271,
          65378607361501080857009149939512557028198746004375,
          35829035317434717326932123578154982629742552737307,
          94953759765105305946966067683156574377167401875275,
          88902802571733229619176668713819931811048770190271,
          25267680276078003013678680992525463401061632866526,
          36270218540497705585629946580636237993140746255962,
          24074486908231174977792365466257246923322810917141,
          91430288197103288597806669760892938638285025333403,
          34413065578016127815921815005561868836468420090470,
          23053081172816430487623791969842487255036638784583,
          11487696932154902810424020138335124462181441773470,
          63783299490636259666498587618221225225512486764533,
          67720186971698544312419572409913959008952310058822,
          95548255300263520781532296796249481641953868218774,
          76085327132285723110424803456124867697064507995236,
          37774242535411291684276865538926205024910326572967,
          23701913275725675285653248258265463092207058596522,
          29798860272258331913126375147341994889534765745501,
          18495701454879288984856827726077713721403798879715,
          38298203783031473527721580348144513491373226651381,
          34829543829199918180278916522431027392251122869539,
          40957953066405232632538044100059654939159879593635,
          29746152185502371307642255121183693803580388584903,
          41698116222072977186158236678424689157993532961922,
          62467957194401269043877107275048102390895523597457,
          23189706772547915061505504953922979530901129967519,
          86188088225875314529584099251203829009407770775672,
          11306739708304724483816533873502340845647058077308,
          82959174767140363198008187129011875491310547126581,
          97623331044818386269515456334926366572897563400500,
          42846280183517070527831839425882145521227251250327,
          55121603546981200581762165212827652751691296897789,
          32238195734329339946437501907836945765883352399886,
          75506164965184775180738168837861091527357929701337,
          62177842752192623401942399639168044983993173312731,
          32924185707147349566916674687634660915035914677504,
          99518671430235219628894890102423325116913619626622,
          73267460800591547471830798392868535206946944540724,
          76841822524674417161514036427982273348055556214818,
          97142617910342598647204516893989422179826088076852,
          87783646182799346313767754307809363333018982642090,
          10848802521674670883215120185883543223812876952786,
          71329612474782464538636993009049310363619763878039,
          62184073572399794223406235393808339651327408011116,
          66627891981488087797941876876144230030984490851411,
          60661826293682836764744779239180335110989069790714,
          85786944089552990653640447425576083659976645795096,
          66024396409905389607120198219976047599490197230297,
          64913982680032973156037120041377903785566085089252,
          16730939319872750275468906903707539413042652315011,
          94809377245048795150954100921645863754710598436791,
          78639167021187492431995700641917969777599028300699,
          15368713711936614952811305876380278410754449733078,
          40789923115535562561142322423255033685442488917353,
          44889911501440648020369068063960672322193204149535,
          41503128880339536053299340368006977710650566631954,
          81234880673210146739058568557934581403627822703280,
          82616570773948327592232845941706525094512325230608,
          22918802058777319719839450180888072429661980811197,
          77158542502016545090413245809786882778948721859617,
          72107838435069186155435662884062257473692284509516,
          20849603980134001723930671666823555245252804609722,
          53503534226472524250874054075591789781264330331690 ]
  in take 10 . show . sum $ n

divisibleTriangular :: Int -> Maybe Integer
divisibleTriangular d =
  let
    t = scanl (+) 0 [1..]
    factors n = filter (\x -> n `mod` x == 0) [1..n]
  in headMay . filter (\x -> length (factors x) > d) $ t

productInAGrid :: [(Int, [Int], [Int])]
productInAGrid =
  let g =
        [ 08, 02, 22, 97, 38, 15, 00, 40, 00, 75, 04, 05, 07, 78, 52, 12, 50, 77, 91, 08
        , 49, 49, 99, 40, 17, 81, 18, 57, 60, 87, 17, 40, 98, 43, 69, 48, 04, 56, 62, 00
        , 81, 49, 31, 73, 55, 79, 14, 29, 93, 71, 40, 67, 53, 88, 30, 03, 49, 13, 36, 65
        , 52, 70, 95, 23, 04, 60, 11, 42, 69, 24, 68, 56, 01, 32, 56, 71, 37, 02, 36, 91
        , 22, 31, 16, 71, 51, 67, 63, 89, 41, 92, 36, 54, 22, 40, 40, 28, 66, 33, 13, 80
        , 24, 47, 32, 60, 99, 03, 45, 02, 44, 75, 33, 53, 78, 36, 84, 20, 35, 17, 12, 50
        , 32, 98, 81, 28, 64, 23, 67, 10, 26, 38, 40, 67, 59, 54, 70, 66, 18, 38, 64, 70
        , 67, 26, 20, 68, 02, 62, 12, 20, 95, 63, 94, 39, 63, 08, 40, 91, 66, 49, 94, 21
        , 24, 55, 58, 05, 66, 73, 99, 26, 97, 17, 78, 78, 96, 83, 14, 88, 34, 89, 63, 72
        , 21, 36, 23, 09, 75, 00, 76, 44, 20, 45, 35, 14, 00, 61, 33, 97, 34, 31, 33, 95
        , 78, 17, 53, 28, 22, 75, 31, 67, 15, 94, 03, 80, 04, 62, 16, 14, 09, 53, 56, 92
        , 16, 39, 05, 42, 96, 35, 31, 47, 55, 58, 88, 24, 00, 17, 54, 24, 36, 29, 85, 57
        , 86, 56, 00, 48, 35, 71, 89, 07, 05, 44, 44, 37, 44, 60, 21, 58, 51, 54, 17, 58
        , 19, 80, 81, 68, 05, 94, 47, 69, 28, 73, 92, 13, 86, 52, 17, 77, 04, 89, 55, 40
        , 04, 52, 08, 83, 97, 35, 99, 16, 07, 97, 57, 32, 16, 26, 26, 79, 33, 27, 98, 66
        , 88, 36, 68, 87, 57, 62, 20, 72, 03, 46, 33, 67, 46, 55, 12, 32, 63, 93, 53, 69
        , 04, 42, 16, 73, 38, 25, 39, 11, 24, 94, 72, 18, 08, 46, 29, 32, 40, 62, 76, 36
        , 20, 69, 36, 41, 72, 30, 23, 88, 34, 62, 99, 69, 82, 67, 59, 85, 74, 04, 36, 16
        , 20, 73, 35, 29, 78, 31, 90, 01, 74, 31, 49, 71, 48, 86, 81, 16, 23, 57, 05, 54
        , 01, 70, 54, 71, 83, 51, 54, 69, 16, 92, 33, 48, 61, 43, 52, 01, 89, 19, 67, 48
        ]
      iVertical = [0..399-60] & fmap (\x -> [x, x+20, x+40, x+60])
      hasFourToTheRight x = ((x+1) `mod` 20) < 18
      iHorizontal = [0..399-3]
        & filter hasFourToTheRight
        & fmap (\x -> [x, x+1, x+2, x+3])
      iDiagonal = [0..399-63]
        & filter hasFourToTheRight
        & fmap (\x -> [x, x+21, x+42, x+63])
      iDiagonal2 = [59..399-3]
        & filter hasFourToTheRight
        & fmap (\x -> [x, x-19, x-38, x-57])
      i = iHorizontal <> iVertical <> iDiagonal <> iDiagonal2
      p indexes =
        let values = fmap (g!!) indexes
        in (product values, indexes, values)
  in p <$> i

primeSum :: Integer -> Integer
primeSum m = sum $ takeWhile (m>) primes

pythagoreanTriplets :: Int -> [(Int, Int, Int)]
pythagoreanTriplets s =
  let
    t = do
      a <- [1..s]
      b <- [1..s-a]
      c <- [1..s-a-b]
      guard (a+b+c==s)
      let w = 2 :: Int
      guard (a^w+b^w==c^w)
      pure (a, b, c)
  in t

productInSeries :: Int -> Maybe Int
productInSeries digits =
  let
    n1000 = "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450"
    grouped :: String -> [String]
    grouped s =
      if length s < digits
      then []
      else take digits s : grouped (drop 1 s)
    ints :: [[Char]] -> [[Int]]
    ints = fmap (fmap (read . pure))
    candidates = ints . grouped $ n1000
  in headMay . sortOn Down . fmap product $ candidates

primes :: [Integer]
primes = 2 : sieve [3,5..]
  where sieve [] = [] -- never occurs
        sieve (p:xs) =
          let rest =  do
                x <- xs
                guard (x `mod` p /= 0)
                pure x
          in p : sieve rest

eratosthenes :: [Integer]
eratosthenes = 2 : 3 : sieve (drop 1 eratosthenes) [5,7..]
  where
    sieve [] _ = [] -- never occurs
    sieve (p:ps) xs =
      let (h, ~t) = span (< p*p) xs
          rest = do
            x <- drop 1 t
            guard (x `mod` p /= 0)
            pure x
      in h ++ sieve ps rest

isPrime :: Integer -> Bool
isPrime 0 = False
isPrime 1 = False
isPrime c =
  let w n = fromInteger n <= (sqrt(fromInteger c) :: Double)
  in all (\d -> c `mod` d /= 0) . takeWhile w $ primes

sumSquareDifference :: [Int] -> Int
sumSquareDifference numbers =
  let
    s x = x ^ (2 :: Int)
    squareSum = sum . fmap s $ numbers
    sumSquare = s . sum $ numbers
  in sumSquare - squareSum

evenlyDivisible :: [Int] -> Maybe Int
evenlyDivisible divisors =
  let d = [maximum divisors..]
      f x y = (x `mod` y) == 0
  in headMay . filter (\x -> all (f x) $ reverse divisors) $ d

isPalindrome :: Show i => i -> Bool
isPalindrome i = reverse s == s
  where
    s = show i

largestPalindrome :: [Int] -> [Int]
largestPalindrome = sortOn Down . allMul
  where
    allMul n = do
      x <- n
      y <- n
      let z = x * y
      guard . isPalindrome $ z
      pure z

primeFactors :: Integer -> [Integer]
primeFactors x =
  let
    w n = fromInteger n <= (sqrt(fromInteger x) :: Double)
    t = takeWhile w [2..]
    g 1 _ = []
    g y [] = [y]
    g y (v:vs) =
      let (d, m) = y `divMod` v
      in if m == 0
         then v:g d t
         else g y vs
  in g x t

powMod :: Integer -> Integer -> Integer -> Integer
powMod _ 0 _ = 1
powMod a e m
  | even e    = let r = powMod a (e `div` 2) m in r*r `mod` m
  | otherwise = a * powMod a (e-1) m `mod` m

-- | Deterministic Miller-Rabin.
millerRabin :: Integer -> Bool
millerRabin n
  | n < 2     = False
  | n `elem` [2, 3, 5, 7, 11, 13] = True
  | even n    = False
  | otherwise = all (not . witness n) w
  where
    -- The witnesses are from Jim Sinclair's deterministic set, proven
    -- correct for all n < 2^64. For larger n, use random witnesses or
    -- Baillie-PSW.
    w = [2, 325, 9375, 28178, 450775, 9780504, 1795265022]
    d = until (\x -> odd x || x == 1) (`div` 2) (n-1)
    s = let go x c = if x == n-1 then c else go (x*2) (c+1) in go d (0::Integer)
    witness c a
      | a `mod` c == 0 = False
      | otherwise =
        let go x 0 = x /= 1 && x /= c-1
            go x k = let x' = x*x `mod` c
                     in if x' == 1 && x /= 1 && x /= c-1
                        then True
                        else go x' (k-1)
        in go (powMod a d c) s
