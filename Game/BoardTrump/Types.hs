module Game.BoardTrump.Types (Player, initialDraw, playerName, hand, deck, playerId, Card, battle, energy, cost, initDeck, motionScope) where

import Lens.Family2
import Lens.Family2.Unchecked
import Lens.Family2.Stock (_1, both)
import qualified Haste.Perch as P
import Data.Monoid (mconcat, mappend)
import System.Random.Shuffle (shuffle')
import System.Random (StdGen)

data Player = Player
  { _hand :: [Card]
  , _deck :: [Card]
  , playerName :: String
  , playerId :: String
  , representation :: Card -> String}

instance P.ToElem Player where
  toElem p = mappend
    (P.forElems ("#" ++ (p & playerId) ++" .deck") $
      mappend P.clear $
        P.toElem $ (p & playerName) ++ "の残り山札: " ++ (show $ length $ p ^. deck))
    (P.forElems ("#" ++ (p & playerId) ++" .hand") $
      mappend P.clear $
        mconcat $ map (P.li . (p & representation)) $ p ^. hand)

hand :: Lens' Player [Card]; hand = lens _hand $ \p x -> p { _hand = x }
deck :: Lens' Player [Card]; deck = lens _deck $ \p x -> p { _deck = x }

initialDraw :: String -> String -> (Card -> String) -> [Card] -> Player
initialDraw name plid rep d = Player (take 3 d) (drop 3 d) name plid rep

newtype Card = Card { cardIndex :: Int } deriving Eq

instance Show Card where
  show (Card 1) = "A"
  show (Card 11) = "J"
  show (Card 12) = "Q"
  show (Card 13) = "K"
  show (Card i)  = show i

battle :: Card -> Card -> Bool
battle (Card i) (Card j) = i > j

energy :: Card -> Int
energy (Card i) = if i == 1 then 2 else 1

cost :: Card -> Int
cost (Card i) = if i > 10 then 2 else if i == 1 then 114514 else 0

initDeck :: StdGen -> [Card]
initDeck g = shuffle' allCards (length allCards) g
  where allCards = concat $ replicate 2 $ map Card $ [1..13]

additionalMotion :: [(Int, Int)] -> [(Int, Int) -> (Int, Int)]
additionalMotion = map $ \(x, y) -> \(i, j) -> (x + i, y + j)

symmetryMotion :: [(Int, Int)] -> Bool -> [(Int, Int) -> (Int, Int)]
symmetryMotion scope p = additionalMotion $ if p then scope else map (_1 %~ negate) scope

motionScope :: Bool -> Card -> [(Int, Int) -> (Int, Int)]
motionScope p card
  | (cardIndex card) == 13 = symmetryMotion [(0, 1), (-1, 0), (0, -1)] p
  | (cardIndex card) == 12 = symmetryMotion [(-1, 0), (0, 0), (1, 0)] p
  | (cardIndex card) == 11 = symmetryMotion [(-1, 0), (-2, 0)] p
  | otherwise              = symmetryMotion [(-1, 0)] p
