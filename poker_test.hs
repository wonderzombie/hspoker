module Poker_Test where

import qualified Poker as P
import Test.HUnit
import Text.Printf

testIsDescending = TestCase $
  let b = P.isDescending [3, 2, 1] in
  assertBool "Expected this to be descending." b

testIsDescending_Dupes = TestCase $
  let b = not $ P.isDescending [8, 7, 7] in
  assertBool "Expected this NOT to be descending." b

testIsDescending_Ascending = TestCase $
  let l = [1, 2, 3]
      b = not $ P.isDescending l in
  assertBool ("Expected list not to be descending: " ++ show l) b

testOnlyGroups = TestCase $
  let x  = [1, 2, 2, 3, 3]
      x' = P.onlyGroups x
      y  = x' !! 0
  in do
    assertEqual ("List only has two duplicated sets: " ++ show x) 2 (length x')
    assertEqual ("Processed list should have two items: " ++ show x') 2 (length y)

-- Hand types.
testStraightFlush = TestCase $ 
  let sf        = P.parseCards ["6C", "7C", "8C", "9C", "TC"]
      sfInfo    = sf    >>= P.straightFlush
      -- A flush but not a straight; a four.
      notS      = P.parseCards ["7C", "4C", "8C", "9C", "TC"]
      notSInfo  = notS  >>= P.straightFlush
      -- A straight but not a flush; first card is Diamonds.
      notF      = P.parseCards ["6D", "7C", "8C", "9C", "TC"]
      notFInfo  = notF  >>= P.straightFlush
  in do 
    assertBool ("Expected a straight flush: " ++ show sf) (sfInfo /= Nothing)
    assertBool ("Hand is not a straight: " ++ show notS) (notSInfo == Nothing)
    assertBool ("Hand is not a flush: " ++ show notF) (notFInfo == Nothing)

testStraight = TestCase $
  -- All different suits, sequential, but out of order.
  let s     = P.parseCards ["QC", "KD", "AH", "JS", "TH"]
      sInfo = s >>= P.straight
  in do
    assertBool ("Expected straight: " ++ show s) (sInfo /= Nothing)

testFlush = TestCase $
  -- All same suits.
  let f     = P.parseCards ["7C", "7C", "8C", "9C", "AC"]
      fInfo = f >>= P.flush
  in do
    assertBool ("Expected flush: " ++ show f) (fInfo /= Nothing)

testFullHouse = TestCase $
  let fh      = P.parseCards ["3C", "3H", "3D", "TH", "TS"]
      fhInfo  = fh >>= P.fullHouse
  in do
    assertBool ("Expected full house: " ++ show fh) (fhInfo /= Nothing)

testFourKind = TestCase $
  let fk     = P.parseCards ["6C", "6H", "6D", "6S", "KD"]
      fkInfo = fk >>= P.fourKind
  in do
    assertBool ("Expected four of a kind: " ++ show fk) (fkInfo /= Nothing)

testThreeKind = TestCase $
  let tk      = P.parseCards ["4C", "4D", "4S", "JH", "TD"]
      tkInfo  = tk >>= P.threeKind
  in do
    assertBool ("Expected three of a kind: " ++ show tk) (tkInfo /= Nothing)

testTwoKind = TestCase $
  let tk      = P.parseCards ["6C", "8C", "9D", "TS", "6D"]
      tkInfo  = tk >>= P.twoKind
  in do
    assertBool ("Expected two of a kind: " ++ show tk) (tkInfo /= Nothing)

makeTest :: (String, Maybe P.Card) -> Test
makeTest (string, result) = string ~: result ~=? P.parseCard string

parseCardCases :: [(String, Maybe P.Card)]
parseCardCases = [ ("TD", Just (P.Diamonds, 10))
                 , ("AS", Just (P.Spades, 14))
                 , ("9H", Just (P.Hearts, 9))
                 , ("2C", Just (P.Clubs, 2))
                 , ("XC", Nothing)
                 , ("2F", Nothing)
                 ]

testParseCard :: Test
testParseCard = TestList $ map makeTest parseCardCases

tests = TestList [ TestLabel "testIsDescending"           testIsDescending
                 , TestLabel "testIsDescending_Dupes"     testIsDescending_Dupes
                 , TestLabel "testIsDescending_Ascending" testIsDescending_Ascending
                 , TestLabel "testOnlyGroups"             testOnlyGroups
                 -- Hand types
                 , TestLabel "testStraightFlush"          testStraightFlush
                 , TestLabel "testFullHouse"              testFullHouse
                 , TestLabel "testStraight"               testStraight
                 , TestLabel "testFlush"                  testFlush
                 , TestLabel "testFourKind"               testFourKind
                 , TestLabel "testThreeKind"              testThreeKind
                 , TestLabel "testTwoKind"                testTwoKind
                 , TestLabel "testParseCard"              testParseCard
                 ]