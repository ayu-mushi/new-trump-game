module NewTrumpGame.Cards
  ( Card(fromCard),
    isColored,
    energy,
    cost,
    initDeck,
    motionScope) where
import System.Random.Shuffle (shuffle')
import System.Random (RandomGen)
import Data.Maybe (isJust)

newtype Card = Card { fromCard :: Maybe Int }

instance Show Card where
  show (Card Nothing)   = "w"
  show (Card (Just 1))  = "A"
  show (Card (Just 11)) = "J"
  show (Card (Just 12)) = "Q"
  show (Card (Just 13)) = "K"
  show (Card (Just i))  = show i

isColored :: Card -> Bool
isColored = isJust . fromCard

energy :: Card -> Int
energy (Card Nothing) = 2
energy _              = 1

cost :: Int -> Int
cost i = if i > 10 then 2 else 0

initDeck :: RandomGen g => g -> [Card]
initDeck g = shuffle' allCards (length allCards) g
  where allCards = concat $ replicate 4 $ map Card $ Nothing : (map Just [1..13])

motionScope :: Int -> [(Int, Int)]
motionScope card = [(0, -1)]
