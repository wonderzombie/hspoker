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

-- Hand types.
testStraightFlush = TestCase $ 
  let sf        = P.parseCards ["6C", "7C", "8C", "9C", "TC"]
      notSf     = P.parseCards ["7C", "7C", "8C", "9C", "TC"]
      sfInfo    = sf >>= P.straightFlush
      notSfInfo = notSf >>= P.straightFlush
  in do 
    assertBool ("Expected a straight flush: " ++ show sf) (sfInfo /= Nothing)
    assertBool ("Expected NOT a straight flush: " ++ show notSf) (notSfInfo == Nothing)

testFullHouse = TestCase $
  let fh      = P.parseCards ["3C", "3H", "3D", "TH", "TS"]
      fhInfo  = fh >>= P.fullHouse
  in do
    assertBool ("Expected full house: " ++ show fh) (fhInfo /= Nothing)

testTwoKind = TestCase $
  let tk      = P.parseCards ["6C", "8C", "9D", "TS", "6D"]
      tkInfo  = tk >>= P.twoKind
  in do
    assertBool ("Expected two of a kind: " ++ show tk) (tkInfo /= Nothing)

testFourKind = TestCase $
  let fk     = P.parseCards ["6C", "6H", "6D", "6S", "KD"]
      fkInfo = fk >>= P.fourKind
  in do
    assertBool ("Expected four of a kind: " ++ show fk) (fkInfo /= Nothing)

--parseCardCases :: [(String, Maybe P.Card)]
--parseCardCases = [ ("TD", Just (P.Diamonds, 10))
--                 , ("AS", Just (P.Spades, 14))
--                 , ("9H", Just (P.Hearts, 9))
--                 , ("2C", Just (P.Clubs, 2))
--                 , ("XC", Nothing)
--                 , ("2F", Nothing)
--                 ]

--parsesOK :: (String, Maybe P.Card) -> (String, Bool)
--parsesOK (s, ex) = (out, ex == act)
--  where act = P.parseCard s 
--        out = "Wanted " ++ show ex ++ ", Got " ++ show act

--testParseCard = TestCase $ do
--  let results = map parsesOK parseCardCases
--      failed  = filter (not . snd) results

tests = TestList [ TestLabel "testIsDescending"           testIsDescending
                 , TestLabel "testIsDescending_Dupes"     testIsDescending_Dupes
                 , TestLabel "testIsDescending_Ascending" testIsDescending_Ascending
                 -- Hand types
                 , TestLabel "testStraightFlush"          testStraightFlush
                 , TestLabel "testTwoKind"                testTwoKind
                 , TestLabel "testFourKind"               testFourKind
                 , TestLabel "testFullHouse"              testFullHouse
                 --, TestLabel "testParseCard"       testParseCard
                 ]