module NewTrumpGame.Player
  (Player(..), ComputerPlayer, HumanPlayer) where

import Control.Lens
import NewTrumpGame.Cards

class Player a where
  hand :: Lens' a [Card]
  deck :: Lens' a [Card]
  initialDraw :: [Card] -> a

data ComputerPlayer = ComputerPlayer {
  cpHand :: [Card],
  cpDeck :: [Card]
  }

instance Player ComputerPlayer where
  hand = lens cpHand $ \p x -> p { cpHand = x }
  deck = lens cpDeck $ \p x -> p { cpDeck = x }
  initialDraw deck = ComputerPlayer (take 3 deck) (drop 3 deck)

data HumanPlayer = HumanPlayer {
  humanHand :: ([Card], [Card]),
  humanDeck :: [Card]
  }

instance Player HumanPlayer where
  hand = lens (uncurry ((++) . reverse). humanHand) $ \p x -> p { humanHand = ([], x) }
  deck = lens humanDeck $ \p x -> p { humanDeck = x }
  initialDraw deck = HumanPlayer ([], (take 3 deck)) $ drop 3 deck
