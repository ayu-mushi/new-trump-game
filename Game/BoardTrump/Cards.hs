module Game.BoardTrump.Cards
( Card(cardIndex),
    energy,
    cost,
    initDeck,
    motionScope) where
import System.Random.Shuffle (shuffle')
import System.Random (StdGen)
import Control.Arrow ((***))
import Lens.Family2 ((%~))
import Lens.Family2.Stock (_1, both)

newtype Card = Card { cardIndex :: Int } deriving Eq

instance Ord Card where
  compare (Card n) (Card m) = compare n m

instance Show Card where
  show (Card 1) = "A"
  show (Card 11) = "J"
  show (Card 12) = "Q"
  show (Card 13) = "K"
  show (Card i)  = show i

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
  | (cardIndex card) < 10 = symmetryMotion [(-1, 0)] p
  | otherwise             = symmetryMotion [(-1, -1), (-1, 0), (-1, 1)] p
