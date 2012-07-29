module Poker_Test where

import qualified Poker as P
import Test.HUnit
import Text.Printf

testIsDescending = TestCase $
  let b = P.isDescending [3, 2, 1] in
  assertBool "Expected this to be descending." b

testIsNotDescending = TestCase $
  let l = [1, 2, 3]
      b = not $ P.isDescending l in
  assertBool ("Expected list not to be descending: " ++ show l) b

parseCardCases :: [(String, Maybe P.Card)]
parseCardCases = [ ("TD", Just (P.Diamonds, 10))
                 , ("AS", Just (P.Spades, 14))
                 , ("9H", Just (P.Hearts, 9))
                 , ("2C", Just (P.Clubs, 2))
                 , ("XC", Nothing)
                 , ("2F", Nothing)
                 ]

testStraightFlush = TestCase $ 
  let sf    = ["6C", "7C", "8C", "9C", "TC"]
      info  = P.parseCards sf >>= P.straightFlush
  in do 
    assertBool ("Expected a straight flush: " ++ show sf) (info /= Nothing)

--parsesOK :: (String, Maybe P.Card) -> (String, Bool)
--parsesOK (s, ex) = (out, ex == act)
--  where act = P.parseCard s 
--        out = "Wanted " ++ show ex ++ ", Got " ++ show act

--testParseCard = TestCase $ do
--  let results = map parsesOK parseCardCases
--      failed  = filter (not . snd) results

tests = TestList [ TestLabel "testIsDescending"    testIsDescending
                 , TestLabel "testIsNotDescending" testIsNotDescending
                 , TestLabel "testStraightFlush"   testStraightFlush
                 --, TestLabel "testParseCard"       testParseCard
                 ]