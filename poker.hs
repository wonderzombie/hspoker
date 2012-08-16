
module Poker where

import Data.Char
import qualified Data.List as L

data Suit = Clubs | Diamonds | Hearts | Spades
  deriving (Show, Read, Eq)

type Card = (Suit, Integer)
type Hand = [Card]

type HandInfo = (Integer, [Integer])

hands :: [(Hand -> Maybe HandInfo)]
hands = [ straightFlush -- 8
        , fourKind
        , fullHouse
        , flush
        , straight -- 4
        , threeKind
        , twoPair
        , twoKind
        , highCard -- 0
        ]

getHandRank :: Hand -> Maybe HandInfo
getHandRank h = highestRank hands h
    where highestRank [] _     = Nothing
          highestRank (r:rs) h = case (r h) of Nothing -> highestRank rs h
                                               Just x  -> Just x

ranks :: [Integer]
ranks = [2..14] -- up to 10, and then J, Q, K, A.

suits :: [Suit]
suits = [Clubs, Diamonds, Hearts, Spades]

showCard :: Card -> String
showCard (s,r) = show s ++ showRank r

showRank :: Integer -> String
showRank 11 = "J"
showRank 12 = "Q"
showRank 13 = "K"
showRank 14 = "A"
showRank r  = show r

getRank :: Char -> Maybe Integer
getRank x
    | isDigit x  = let v = read [x] in 
                   if v > 1 then Just v else Nothing
    | isLetter x = lookup x ranks
    | otherwise  = Nothing
    where ranks = [ ('T', 10)
                  , ('J', 11)
                  , ('Q', 12)
                  , ('K', 13)
                  , ('A', 14)
                  ]

getSuit :: Char -> Maybe Suit
getSuit 'C' = Just Clubs
getSuit 'D' = Just Diamonds
getSuit 'H' = Just Hearts
getSuit 'S' = Just Spades
getSuit _   = Nothing

sortHand :: Hand -> Hand
sortHand h = L.sortBy cardOrdering h
  where cardOrdering (_,r1) (_,r2) = compare r1 r2

--- Hands.

straightFlush :: Hand -> Maybe HandInfo
straightFlush h = flush h >> straight h >> return (8, [highCard])
    where highCard = head $ getRanks h

flush :: Hand -> Maybe HandInfo
flush h = if uniform then Just (5, [maximum $ getRanks h])
          else Nothing
  where suits   = getSuits h
        first   = head suits
        uniform = all (\x -> x == first) suits

straight :: Hand -> Maybe HandInfo
straight h = case (isDescending ranks) of False -> Nothing
                                          True  -> Just (4, [highCard])
  where ranks    = getRanks h
        highCard = head $ getRanks h



fullHouse :: Hand -> Maybe HandInfo
fullHouse h = kind h 3 >> kind h 2 >> return (6, [highest, secondHighest])
  where ranks         = getRanks h
        highest       = maximum ranks
        secondHighest = minimum ranks

twoPair :: Hand -> Maybe HandInfo
twoPair h = case hiLo of Nothing        -> Nothing
                         Just (hi, lo)  -> Just (2, [hi, lo])
  where gs   = L.group $ getRanks h
        hiLo = let (x:y:_) = gs in 
               case (allLengthTwo [x,y]) of False -> Nothing
                                            True  -> Just (head x, head y)
        allLengthTwo = all (\x -> length x == 2)

kind :: Hand -> Integer -> (Maybe Integer)
kind h n = case matches of Nothing    -> Nothing
                           Just (x:_) -> Just x
  where kinds    = L.sort $ onlyGroups $ getRanks h
        matches  = L.find ((==) (fromIntegral n) . length) kinds

fourKind :: Hand -> Maybe HandInfo
fourKind h = case (kind h 4) of Nothing -> Nothing
                                Just x  -> Just (7, [x])

threeKind :: Hand -> Maybe HandInfo
threeKind h = case (kind h 3) of Nothing -> Nothing
                                 Just x  -> Just (3, [x])

twoKind :: Hand -> Maybe HandInfo
twoKind h = case (kind h 2) of Nothing -> Nothing
                               Just x  -> Just (1, [x])

highCard :: Hand -> Maybe HandInfo
highCard h = Just (0, ranks)
    where ranks = getRanks h

--- Utility methods.

-- Sort in descending order.
desc :: Ord a => [a] -> [a]
desc = reverse . L.sort

-- Group the list of Integers and return only those which have multiples.
-- Example: [1, 2, 2, 3, 3] yields [[2, 2,], [3, 3]] as opposed to 
-- [[1], [2, 2], [3, 3]].
onlyGroups :: [Integer] -> [[Integer]]
onlyGroups = filter f . L.group . L.sort
    where f = \x -> length x > 1

isDescending :: [Integer] -> Bool
isDescending xs = xs == expected
    where mx       = maximum xs
          mn       = minimum xs
          expected = desc [mn..mx]

getRanks :: Hand -> [Integer]
getRanks h = ranks'
    where ranks  = desc $ map snd h
          ranks' = if head ranks == 14 && (isDescending $ tail ranks)
                   then reverse [1..5] else ranks

getSuits :: Hand -> [Suit]
getSuits = map fst

parseCard :: String -> Maybe Card
parseCard (r:s:"") = do
    rank <- getRank r
    suit <- getSuit s
    return (suit, rank)

parseCards :: [String] -> Maybe Hand
parseCards cs = mapM parseCard cs
