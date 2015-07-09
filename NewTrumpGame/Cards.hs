module NewTrumpGame.Cards
  ( Card,
    isColored,
    energy,
    cost,
    initDeck) where
import System.Random.Shuffle (shuffle')
import System.Random (RandomGen)

newtype Color = Color { index :: Int }

newtype Card = Card { fromCard :: Maybe Color }

instance Show Card where
  show (Card Nothing)           = "w"
  show (Card (Just (Color 1)))  = "A"
  show (Card (Just (Color 11))) = "J"
  show (Card (Just (Color 12))) = "Q"
  show (Card (Just (Color 13))) = "K"
  show (Card (Just (Color i)))  = show i

isColored :: Card -> Bool
isColored (Card Nothing)  = False
isColored (Card (Just _)) = True

energy :: Card -> Int
energy (Card Nothing) = 2
energy _              = 1

cost :: Color -> Int
cost (Color i) = if i > 10 then 2 else 0

initDeck :: RandomGen g => g -> [Card]
initDeck g = shuffle' allCards (length allCards) g
  where allCards = concat $ replicate 4 $ map Card $ Nothing : (map (Just . Color) [1..13])
