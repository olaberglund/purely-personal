{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Poker where

import Control.Applicative ((<|>))
import Control.DeepSeq (NFData, force)
import Control.Monad (guard, (>=>))
import Control.Parallel.Strategies (parEval, parList, parListChunk, parListN, parListSplitAt, parMap, parPair, parTuple2, rdeepseq, rpar, rseq, runEval, runEvalIO, using)
import Data.Aeson (ToJSON)
import Data.Bifunctor (bimap)
import Data.Char (digitToInt)
import Data.Foldable (find, forM_)
import qualified Data.Foldable as F
import Data.Function (on)
import Data.Functor ((<&>))
import Data.List (find, group, groupBy, maximumBy, nub, sort, sortBy, sortOn, tails, (\\))
import Data.List.Split (chunksOf, splitOn, wordsBy)
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.MultiSet as M hiding (concatMap, filter, map, mapMaybe, null, (\\))
import Data.Ord (comparing)
import Debug.Trace (trace)
import GHC.Generics (Generic)
import System.Environment (getArgs)
import System.Random (mkStdGen)
import qualified System.Random.Shuffle as R
import Prelude hiding (maximum)

data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Ord, Eq, Enum, Generic, NFData)

instance ToJSON Rank

data Suit = Diamonds | Spades | Clubs | Hearts deriving (Eq, Enum, Ord, Generic)

instance ToJSON Suit

instance Eq Card where
  (==) = (==) `on` rank

instance Ord Card where
  (<=) = (<=) `on` rank

data Card = Card {rank :: Rank, suit :: Suit} deriving (Generic)

instance ToJSON Card

data TwoPairRank = TwoPairRank Rank Rank deriving (Show, Eq, Generic, NFData)

instance ToJSON TwoPairRank

instance Ord TwoPairRank where
  (TwoPairRank r11 r12) <= (TwoPairRank r21 r22) = sort [r11, r12] <= sort [r21, r22]

type Kicker = Rank

data Hand
  = High Rank [Kicker]
  | Pair Rank [Kicker]
  | TwoPair TwoPairRank (Maybe Kicker)
  | ThreeKind Rank [Kicker]
  | Straight Rank
  | Flush [Rank]
  | FullHouse Rank Rank
  | FourKind Rank
  | StraightFlush Rank
  deriving (Show, Eq, Ord, Generic, NFData)

instance ToJSON Hand

instance Show Rank where
  show Ace = "A"
  show King = "K"
  show Queen = "Q"
  show Jack = "J"
  show Ten = "T"
  show r = show $ fromEnum r + 2

instance Show Card where
  show c = show (rank c) <> show (suit c)

instance Show Suit where
  show Diamonds = "d"
  show Spades = "s"
  show Clubs = "c"
  show Hearts = "h"

go :: [Card] -> [Card] -> [Card] -> (Double, Double, Double)
go cp1 cp2 b = do
  let rem = filter (not . flip any (cp1 ++ cp2 ++ b) . strictEquals) deck
      p1 = possibleHands (cp1 ++ b) rem
      p2 = possibleHands (cp2 ++ b) rem
      (p1Ws, p2Ws) = (countWins (>) p1 p2, countWins (<) p1 p2) `using` parTuple2 rseq rseq
      chops = countWins (==) p1 p2
      totHands = p1Ws + p2Ws + chops
   in (calcWinRatio p1Ws totHands, calcWinRatio p2Ws totHands, calcWinRatio chops totHands)
  where
    countWins comp p1 p2 = length $ filter id $ zipWith comp p1 p2
    calcWinRatio pws totpws = fromIntegral pws / fromIntegral totpws

-- utils

strictEquals :: Card -> Card -> Bool
strictEquals (Card r1 s1) (Card r2 s2) = r1 == r2 && s1 == s2

-- given: player cards, board cards, remaining cards, calculate all best hands for each combination of remaining board cards
possibleHands :: [Card] -> [Card] -> [Hand]
possibleHands hs rem = catMaybes (map (identifyBestHand . (hs ++)) boards `using` parListChunk 3000 rdeepseq)
  where
    boards = subsequencesOfSize (7 - length hs) rem

identifyBestHand [] = error "No cards"
identifyBestHand cs =
  straightFlush cs
    <|> fourKind cs
    <|> fullHouse cs
    <|> flush cs
    <|> straight cs
    <|> threeKind cs
    <|> twoPair cs
    <|> pair cs
    <|> high cs
    <|> Nothing

keepMostFreqSuit :: [Card] -> [Card]
keepMostFreqSuit cs = filter ((== mostFreqSuit cs) . suit) cs
  where
    mostFreqSuit = suit . head . maximumBy (comparing length) . groupBy sameSuit . sortOn suit

sameSuit :: Card -> Card -> Bool
sameSuit = (==) `on` suit

draw :: [Card] -> (Maybe Card, [Card])
draw [] = (Nothing, [])
draw (c : cs) = (Just c, cs)

drawN :: Int -> [Card] -> ([Card], [Card])
drawN = splitAt

deck :: [Card]
deck = Card <$> [Two .. Ace] <*> [Diamonds, Spades, Clubs, Hearts]

shuffle :: [Card] -> Int -> [Card]
shuffle cs n = R.shuffle' cs (length cs) (mkStdGen n)

deal :: ([[Card]], [Card]) -> ([[Card]], [Card])
deal (holes, c1 : c2 : cs) = ([c1, c2] : holes, cs)
deal _ = error "not enough cards"

occuranceKind :: Int -> [Card] -> [Rank]
occuranceKind n = fmap rank . M.foldOccur (\c oc cs -> if oc >= n then c : cs else cs) [] . M.fromList

kickers :: Int -> [Rank] -> [Card] -> [Rank]
kickers n xcs = take n . sortBy (flip compare) . filter (`notElem` xcs) . fmap rank

-- Hands
high :: [Card] -> Maybe Hand
high cs = do
  highest <- rank <$> maximum cs
  return $ High highest (kickers 4 [highest] cs)

pair :: [Card] -> Maybe Hand
pair cs =
  let ps = occuranceKind 2 cs
   in maximum $ Pair <$> ps <*> pure (kickers 3 ps cs)

twoPair :: [Card] -> Maybe Hand
twoPair cs = maximum $ do
  [r1, r2] <- subsequencesOfSize 2 (occuranceKind 2 cs)
  return $ TwoPair (TwoPairRank r1 r2) (safeHead $ kickers 1 [r1, r2] cs)
  where
    safeHead [] = Nothing
    safeHead xs = Just $ head xs

threeKind :: [Card] -> Maybe Hand
threeKind cs =
  let ts = occuranceKind 3 cs
   in maximum $ ThreeKind <$> ts <*> pure (kickers 2 ts cs)

flush :: [Card] -> Maybe Hand
flush = maximum . flushes' [] . sortBy (flip compare) . keepMostFreqSuit
  where
    flushes' fs cs@(c1 : c2 : c3 : c4 : c5 : _)
      | all (sameSuit c1) [c2, c3, c4, c5] = flushes' (Flush (rank <$> take 5 cs) : fs) (tail cs) -- normal flush
      | otherwise = fs -- not a flush
    flushes' fs _ = fs -- not enough cards

straight :: [Card] -> Maybe Hand
straight = maximum . straights' [] . nub . sort
  where
    straights' :: [Hand] -> [Card] -> [Hand]
    straights' ss cs@((Card r1 _) : _ : _ : _ : (Card r5 _) : _)
      | nRanksAsc 4 cs
          && r1 == Two
          && Ace `elem` (rank <$> cs) =
          straights' (Straight Five : ss) (filter ((/= Ace) . rank) cs) -- wheel
      | nRanksAsc 5 cs = straights' (Straight r5 : ss) (tail cs) -- normal
      | otherwise = straights' ss (tail cs) -- not a straight
    straights' ss _ = ss -- not enough cards
    nRanksAsc n = ascending . fmap rank . take n
    ascending xs = and $ zipWith isSuccessor xs (tail xs)
    isSuccessor = (>=) . succ'
    succ' Ace = Two
    succ' r = succ r

fullHouse :: [Card] -> Maybe Hand
fullHouse cs = maximum $ do
  rt <- occuranceKind 3 cs
  rp <- occuranceKind 2 $ filter ((/= rt) . rank) cs
  return $ FullHouse rt rp

fourKind :: [Card] -> Maybe Hand
fourKind cs =
  let qs = occuranceKind 4 cs
   in case qs of
        [r] -> Just $ FourKind r
        _ -> Nothing

straightFlush :: [Card] -> Maybe Hand
straightFlush = straight . keepMostFreqSuit >=> return . toFlushStraight
  where
    toFlushStraight (Straight r) = StraightFlush r

maximum :: (Ord a) => [a] -> Maybe a
maximum [] = Nothing
maximum xs = Just $ F.maximum xs

-- https://stackoverflow.com/a/21288092/13131325

subsequencesOfSize :: Int -> [a] -> [[a]]
subsequencesOfSize n xs =
  let l = length xs
   in if n > l then [] else subsequencesBySize xs !! (l - n)
  where
    subsequencesBySize [] = [[[]]]
    subsequencesBySize (x : xs) =
      let next = subsequencesBySize xs
       in zipWith (++) ([] : next) (map (map (x :)) next ++ [[]])

parseHand :: String -> (Maybe Hand, Int)
parseHand = parseHand' . map read . splitOn ","
  where
    parseHand' [sc1, rc1, sc2, rc2, sc3, rc3, sc4, rc4, sc5, rc5, cl] =
      (identifyBestHand [mkCard rc1 sc1, mkCard rc2 sc2, mkCard rc3 sc3, mkCard rc4 sc4, mkCard rc5 sc5], cl)
    parseHand' _ = error "Invalid input"

mkCard :: Int -> Int -> Card
mkCard 1 s = Card Ace (toEnum $ s - 1)
mkCard r s = Card (toEnum $ r - 2) (toEnum $ s - 1)

toInt :: Hand -> Int
toInt (High _ _) = 0
toInt (Pair _ _) = 1
toInt (TwoPair _ _) = 2
toInt (ThreeKind _ _) = 3
toInt (Straight _) = 4
toInt (Flush _) = 5
toInt (FullHouse _ _) = 6
toInt (FourKind _) = 7
toInt (StraightFlush Ace) = 9
toInt (StraightFlush _) = 8

card :: String -> Maybe Card
card [r, 'h'] = Just $ card' r Hearts
card [r, 'd'] = Just $ card' r Diamonds
card [r, 's'] = Just $ card' r Spades
card [r, 'c'] = Just $ card' r Clubs
card _ = Nothing

card' = Card . rank'

cards :: String -> [Card]
cards = mapMaybe card . words

rank' 'A' = Ace
rank' 'K' = King
rank' 'Q' = Queen
rank' 'J' = Jack
rank' 'T' = Ten
rank' r = toEnum (digitToInt r - 2)
