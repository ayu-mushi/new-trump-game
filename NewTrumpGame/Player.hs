module NewTrumpGame.Player
  (Player(..), ComputerPlayer, HumanPlayer, handZipper, selectNextHand, selectBeginHand) where

import Lens.Family2
import Lens.Family2.Unchecked
import qualified Haste.Perch as P
import Data.Monoid (mconcat)

import NewTrumpGame.Cards

class Player a where
  hand :: Lens' a [Card]
  deck :: Lens' a [Card]
  initialDraw :: [Card] -> a
  playerId :: a -> String

data ComputerPlayer = ComputerPlayer {
  cpHand :: [Card],
  cpDeck :: [Card]
  }

instance Player ComputerPlayer where
  hand = lens cpHand $ \p x -> p { cpHand = x }
  deck = lens cpDeck $ \p x -> p { cpDeck = x }
  initialDraw deck = ComputerPlayer (take 3 deck) (drop 3 deck)
  playerId _ = "computers"

instance P.ToElem ComputerPlayer where
  toElem p = do
    P.forElems ("#"++(playerId p)++" .deck") $ do
      P.clear
      P.toElem $ "コンピュータの残り山札: " ++ (show $ length $ p ^. deck)
    P.forElems ("#"++(playerId p)++" .hand") $ do
      P.clear
      mconcat $ map (P.li . show) $ p ^. hand

data HumanPlayer = HumanPlayer {
  humanHand :: ([Card], [Card]),
  humanDeck :: [Card],
  isSelected :: Bool
  }

handZipper :: Lens' HumanPlayer ([Card], [Card])
handZipper = lens humanHand $ \p x -> p { humanHand = x }

instance Player HumanPlayer where
  hand = lens (uncurry ((++) . reverse). humanHand) $ \p x -> p { humanHand = ([], x) }
  deck = lens humanDeck $ \p x -> p { humanDeck = x }
  initialDraw deck = HumanPlayer ([], (take 3 deck)) (drop 3 deck) False
  playerId _ = "yours"

instance P.ToElem HumanPlayer where
  toElem p = do
    P.forElems ("#"++(playerId p)++" .deck") $ do
      P.clear
      P.toElem $ "あなたの残り山札: " ++ (show $ length $ p ^. deck)
    P.forElems ("#"++(playerId p)++" .hand") $ do
      P.clear
      let (ls, a:rs) = humanHand p
      mconcat $ map (P.li . show) ls
      (if isSelected p then P.attr `flip` (P.atr "id" "selected") else id) $ P.li $ show a
      mconcat $ map (P.li . show) rs

focus :: Lens' ([a], [a]) a
focus = lens (\(_, (x:_)) -> x) $ \(a, (_:c)) x -> (a, x:c)

selectedHand :: Lens' HumanPlayer Card
selectedHand = handZipper . focus

selectBeginHand :: HumanPlayer -> HumanPlayer
selectBeginHand = handZipper %~ start
  where start (ls, rs) = ([], reverse ls ++ rs)

selectNextHand :: HumanPlayer -> HumanPlayer
selectNextHand = handZipper %~ next
  where
    next (ls, (a:rs)) = (a:ls, rs)
    next z = z
