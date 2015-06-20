module NewTrumpGame.Player
  (Player, hand, deck, initialDraw) where

import Control.Lens
import NewTrumpGame.Cards

data Player = Player {
  _hand :: [Card],
  _deck :: [Card]
  }
hand :: Lens' Player [Card]; hand = lens _hand $ \p x -> p { _hand = x}
deck :: Lens' Player [Card]; deck = lens _deck $ \p x -> p { _deck = x}

data ComputerPlayer = ComputerPlayer {
  cpHand :: [Card],
  cpDeck :: [Card]
  }
data HumanPlayer = HumanPlayer {
  humanHand :: ([Card], [Card]),
  humanDeck :: [Card]
  }

initialDraw :: [Card] -> Player
initialDraw deck = Player (take 3 deck) (drop 3 deck)
