module Main (main) where

import Data.Ratio ((%))
import MyLib
import Safe (headMay)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)

import qualified Data.Map as Map

main :: IO ()
main = hspec $ do
  describe "cycleMaps" $ do
    specify "single chain" $
      let m = [
            Map.fromList [("12", ["34"])],
            Map.fromList [("34", ["56"])],
            Map.fromList [("56", ["12"])]]
      in cycleMaps m `shouldBe` ["34", "56", "12"]
    specify "useless leaves" $
      let m = [
            Map.fromList [("12", ["34", "12"])],
            Map.fromList [("34", ["56", "34"])],
            Map.fromList [("56", ["12", "56"])]]
      in cycleMaps m `shouldBe` ["34", "56", "12"]
    specify "six elements" $
      let m = [
            Map.fromList [("12", ["34", "12"])],
            Map.fromList [("34", ["56", "34"])],
            Map.fromList [("56", ["78", "34"])],
            Map.fromList [("78", ["91", "34"])],
            Map.fromList [("91", ["56", "34"])],
            Map.fromList [("56", ["12", "56"])]]
      in cycleMaps m `shouldBe` ["34", "56", "78", "91", "56", "12"]
  describe "parseFile59" $ do
    it "works" $ parseFile59 "36,22,80" `shouldBe` ["36", "22", "80"]
  describe "approxSquareTwo" $ do
    it "1" $ approxSquareTwo 1 `shouldBe` (3 % 2)
    it "3" $ approxSquareTwo 3 `shouldBe` (17 % 12)
    it "8" $ approxSquareTwo 8 `shouldBe` (1393 % 985)
    it "7" $ approxSquareTwo 7 `shouldBe` (577 % 408)
  describe "squareRootConvergents" $ do
    it "8" $ squareRootConvergents 8 `shouldBe` 1
    it "7" $ squareRootConvergents 7 `shouldBe` 0
    it "6" $ squareRootConvergents 6 `shouldBe` 0
  describe "powerDigitSum" $ do
    it "100 100" $ powerDigitSum 100 100 `shouldBe` 1
  describe "poker" $ do
    it "1" $
      poker ["5H", "5C", "6S", "7S", "KD"] ["2C", "3S", "8S", "8D", "TD"] `shouldBe` False
    it "2" $
      poker ["5D", "8C", "9S", "JS", "AC"] ["2C", "5C", "7D", "8S", "QH"] `shouldBe` True
    it "3" $
      poker ["2D", "9C", "AS", "AH", "AC"] ["3D", "6D", "7D", "TD", "QD"] `shouldBe` False
    it "4" $
      poker ["4D", "6S", "9H", "QH", "QC"] ["3D", "6D", "7H", "QD", "QS"] `shouldBe` True
    it "5" $
      poker ["2H", "2D", "4C", "4D", "4S"] ["3C", "3D", "3S", "9S", "9D"] `shouldBe` True
  describe "primeDigitsReplacements" $ do
    it "6" $ primeDigitsReplacements 6 `shouldBe` Just [13, 23, 43, 53, 73, 83]
  describe "masks" $ do
    it "33 1" $ (masks "33" !! 1) '1' `shouldBe` "31"
  describe "consecutivePrimeSum" $ do
    it "100" $ consecutivePrimeSum 100 `shouldBe` (6, 41)
    it "1000" $ consecutivePrimeSum 1000 `shouldBe` (21, 953)
  describe "combinations" $ do
    it "abc 1" $ combinations mempty 1 "abc" `shouldBe`
      ["a", "b", "c"]
    it "abc 2" $ combinations mempty 2 "abc" `shouldBe`
      ["ba", "ca", "cb"]
    it "abc 3" $ combinations mempty 3 "abc" `shouldBe`
      ["cba"]
    it "aac 3" $ combinations mempty 3 "aac" `shouldBe`
      ["caa"]
  describe "distinctPrimesFactors" $ do
    it "3" $ distinctPrimesFactors 3 `shouldBe` Just (Just 644)
  describe "predicate43" $ do
    it "1406357289" $ predicate43 "1406357289" `shouldBe` True
  describe "champernowneDigit" $ do
    it "12" $ champernowneDigit 11 `shouldBe` 1
  describe "triangleSolutions" $ do
    it "120" $ triangleSolutions 120 `shouldBe` 3
  describe "pandigitalMultiples" $ do
    it "10 10" $ pandigitalMultiples 10 10 `shouldBe` 918273645
  describe "pandigitalConcatenatedProduct" $ do
    it "1 1" $ pandigitalConcatenatedProduct 1 [1] `shouldBe` Nothing
    it "192" $ pandigitalConcatenatedProduct 192 [1, 2, 3] `shouldBe` Just 192384576
    it "9" $ pandigitalConcatenatedProduct 9 [1, 2, 3, 4, 5] `shouldBe` Just  918273645
  describe "circularPrimes" $ do
    it "100" $ circularPrimes 100 `shouldBe` 13
  describe "isDigitFactorial" $ do
    it "145" $ isDigitFactorial 145 `shouldBe` True
  describe "isPowerOfDigits" $ do
    it "4 9474" $ isPowerOfDigits 4 9474 `shouldBe` True
  describe "distinctPowers" $ do
    it "5" $ distinctPowers 5 `shouldBe` 15
  describe "spiralDiagonals" $ do
    it "5" $ spiralDiagonals 5 `shouldBe` [1, 3, 5, 7, 9, 13, 17, 21, 25]
  describe "consecutivePrimes" $ do
    it "80" $ consecutivePrimes (-79) 1601 `shouldBe` 80
  describe "reciprocalCycles" $ do
    it "11" $ reciprocalCycles 11 `shouldBe` 7
  describe "extractCycle" $ do
    it "abcdcd" $ extractCycle "abcdcd" `shouldBe` "cd"
    it "abab" $ extractCycle "abab" `shouldBe` "ab"
  describe "extractCycle <$> periodicDiv" $ do
    it "7" $ (extractCycle <$> periodicDiv [1] 1 7) `shouldBe` Just [1,4,2,8,5,7]
    it "6" $ (extractCycle <$> periodicDiv [1] 1 6) `shouldBe` Just [6]
    it "156" $ (extractCycle <$> periodicDiv [1] 1 156) `shouldBe` Just [6,4,1,0,2,5]
  describe "periodicDiv" $ do
    it "7" $ periodicDiv [1] 1 7 `shouldBe` Just [1,4,2,8,5,7,1,4,2,8,5,7]
    it "6" $ periodicDiv [1] 1 6 `shouldBe` Just [1,6,6]
    it "156" $ periodicDiv [1] 1 156 `shouldBe` Just [0,0,6,4,1,0,2,5,6,4,1,0,2,5]
    it "5" $ periodicDiv [1] 1 5 `shouldBe` Nothing
  describe "fibonacciDigits" $ do
    it "3" $ fibonacciDigits 3 `shouldBe` Just (Right 12)
  describe "perm" $ do
    it "0" $ perm "0" `shouldBe` ["0"]
    it "01" $ perm "01" `shouldBe` ["01", "10"]
  describe "lexicographicPermutations" $ do
    it "201" $ lexicographicPermutations [0..2] 5 `shouldBe` "201"
  describe "parseFile22" $ do
    it "works" $ parseFile22 "\"A\",\"B\",\"C\"" `shouldBe` [([],"\"A\",\"B\",\"C\""),(["A"],",\"B\",\"C\""),(["A","B"],",\"C\""),(["A","B","C"],"")]
  describe "alphabeticalValue" $ do
    it "COLIN" $ alphabeticalValue "COLIN" `shouldBe` 53
  describe "sumOfProperDivisors" $ do
    it "220" $ sumOfProperDivisors 220 `shouldBe` 284
    it "284" $ sumOfProperDivisors 284 `shouldBe` 220
  describe "factorialDigitsSum" $ do
    it "10" $ factorialDigitsSum 10 `shouldBe` 27
  describe "maximumPathSum" $ do
    it "results" $ maximumPathSum `shouldBe` 1074
  describe "numberLetterCount" $ do
    it "5" $ numberLetterCount 5 `shouldBe` 19
  describe "numberLetterCount'" $ do
    it "115" $ numberLetterCount' 115 `shouldBe` 20
    it "342" $ numberLetterCount' 342 `shouldBe` 23
  describe "numberWords" $ do
    it "100" $ numberWords 100 `shouldBe` "one hundred"
    it "20" $ numberWords 20 `shouldBe` "twenty"
    it "115" $ numberWords 115 `shouldBe` "one hundred and fifteen"
    it "342" $ numberWords 342 `shouldBe` "three hundred and forty-two"
  describe "powerDigitSumTwo" $ do
    it "15" $ powerDigitSumTwo 15 `shouldBe` 26
  describe "latticePaths" $ do
    it "3" $ latticePaths 3 3 `shouldBe` 20
    it "2" $ latticePaths 2 2 `shouldBe` 6
    it "1" $ latticePaths 1 1 `shouldBe` 2
  describe "collatz" $ do
    it "seq" $
      collatzSequence 13 `shouldBe` [13, 40, 20, 10, 5, 16, 8, 4, 2, 1]
  describe "divisibleTriangular" $ do
    it "five" $
      divisibleTriangular 5 `shouldBe` Just 28
  describe "productInAGrid" $ do
    it "contains the known one" $
      (1788696 `elem` (fmap (\(x,_,_)->x) productInAGrid)) `shouldBe` True
  describe "primeSum" $ do
    it "10" $
      primeSum 10 `shouldBe` 17
  describe "pythagoreanTriplets" $ do
    it "12" $
      pythagoreanTriplets 12 `shouldBe` [(3, 4, 5), (4, 3, 5)]
  describe "productInSeries" $ do
    it "four" $
      productInSeries 4 `shouldBe` Just 5832
  describe "primes" $ do
    it "sixth" $
      primes !! 5 `shouldBe` 13
  describe "sumSquareDifference" $ do
    it "ten" $
      sumSquareDifference [1..10] `shouldBe` 2640
  describe "evenlyDivisible" $ do
    it "ten" $
      evenlyDivisible [1..10] `shouldBe` Just 2520
  describe "largestPalindrome" $ do
    it "two" $
      (headMay . largestPalindrome . reverse $ [1..99]) `shouldBe` Just 9009
    it "three" $
      (headMay . largestPalindrome . reverse $ [1..998]) `shouldNotBe` Just 90909
  describe "primeFactors" $ do
    it "352" $
      primeFactors 352 `shouldBe` [2, 2, 2, 2, 2, 11]
    it "351" $
      primeFactors 351 `shouldBe` [3, 3, 3, 13]
    it "13195" $
      primeFactors 13195 `shouldBe` [5, 7, 13, 29]
    prop "product" $
      \n -> product (primeFactors $ abs n) `shouldBe` abs n
