module NewTrumpGame.Cards
  ( Card(fromCard),
    isColored,
    Color,
    energy,
    cost,
    initDeck,
    motionScope) where
import System.Random.Shuffle (shuffle')
import System.Random (RandomGen)
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

energy :: Card -> Int
energy (Card Nothing) = 2
energy _              = 1

cost :: Color -> Int
cost (Color i) = if i > 10 then 2 else 0

initDeck :: RandomGen g => g -> [Card]
initDeck g = shuffle' allCards (length allCards) g
  where allCards = concat $ replicate 4 $ map Card $ Nothing : (map (Just . Color) [1..13])

motionScope :: Color -> [(Int, Int)]
motionScope card = [(0, -1)]
