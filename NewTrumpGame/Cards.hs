module NewTrumpGame.Cards
( Card(cardIndex),
    energy,
    cost,
    initDeck,
    motionScope) where
import System.Random.Shuffle (shuffle')
import System.Random (StdGen)

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

motionScope :: Card -> [(Int, Int)]
motionScope card = [(0, -1)]
