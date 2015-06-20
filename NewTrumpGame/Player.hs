module NewTrumpGame.Player
  (Player(..), ComputerPlayer, HumanPlayer, selectedHand) where

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

focus :: Lens' ([a], [a]) a
focus = lens (\(_, (x:_)) -> x) $ \(a, (_:c)) x -> (a, x:c)

selectedHand :: Lens' HumanPlayer Card
selectedHand = lens (\x -> humanHand x) (\(HumanPlayer _ deck) x -> HumanPlayer x deck) . focus
