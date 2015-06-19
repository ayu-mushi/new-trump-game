module NewTrumpGame.Player
  (Player(Player), hand, deck) where

import Control.Lens
import NewTrumpGame.Cards

data Player = Player {
  _hand :: [Card],
  _deck :: [Card]
  }
hand :: Lens' Player [Card]; hand = lens _hand $ \p x -> p { _hand = x}
deck :: Lens' Player [Card]; deck = lens _deck $ \p x -> p { _deck = x}
