module NewTrumpGame.Cards
  ( Card(fromCard),
    isColored,
    Color,
    energyOfColored,
    energy,
    cost,
    initDeck,
    motionScope,
    ifWhite) where
import System.Random.Shuffle (shuffle')
import System.Random (StdGen)
import Data.Maybe (isJust)

newtype Color = Color { unColor :: Int } deriving Eq

instance Show Color where
  show (Color 1)  = "A"
  show (Color 11) = "J"
  show (Color 12) = "Q"
  show (Color 13) = "K"
  show (Color i)  = show i

instance Ord Color where
  compare (Color x) (Color y) = compare x y

newtype Card = Card { fromCard :: Maybe Color }

instance Show Card where
  show (Card Nothing)      = "w"
  show (Card (Just color)) = show color

isColored :: Card -> Bool
isColored = isJust . fromCard

energyOfColored :: Color -> Int
energyOfColored _ = 1

energy :: Card -> Int
energy (Card Nothing) = 2
energy (Card (Just color)) = energyOfColored color

cost :: Color -> Int
cost (Color i) = if i > 10 then 2 else 0

initDeck :: StdGen -> [Card]
initDeck g = shuffle' allCards (length allCards) g
  where allCards = concat $ replicate 2 $ map Card $ Nothing : (map (Just . Color) [1..13])

motionScope :: Color -> [(Int, Int)]
motionScope card = [(0, -1)]

ifWhite :: a -> (Color -> a) -> Card -> a
ifWhite a f card = case card of
  Card (Just color) -> f color
  Card Nothing -> a
