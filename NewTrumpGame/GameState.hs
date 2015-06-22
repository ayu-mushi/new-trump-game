module NewTrumpGame.GameState
  (initGame) where
import Data.Monoid (mconcat)
import qualified Haste.Perch as P
import Lens.Family2
import Lens.Family2.Unchecked

import NewTrumpGame.Cards
import NewTrumpGame.Player

newtype Field = Field { fromField :: [[Maybe Card]] }

instance P.ToElem Field where
  toElem (Field xss) = P.forElems "table#field" $ do
    P.clear
    mconcat $ map (P.tr . mconcat . map (P.td . showMaybeCard)) xss
    where
      showMaybeCard mbcard = case mbcard of
        Nothing -> ""
        Just card -> show card

data Game = Game {
  _players :: (Bool, (HumanPlayer, ComputerPlayer)),
  _field :: Field
  }
players :: Lens' Game (Bool, (HumanPlayer, ComputerPlayer)); players = lens _players (\p x -> p { _players = x })
field :: Lens' Game Field; field = lens _field (\p x -> p { _field = x})

instance P.ToElem Game where
  toElem game = do
    P.toElem $ game ^. field
    let (turnPlayer, (a, b)) = game ^. players
    P.forElems "#turnplayer" $ do
      P.clear
      P.toElem $ "-- " ++ (if turnPlayer then "あなた" else "コンピュータ") ++ "の番です"
    P.toElem a
    P.toElem b

initGame :: IO Game
initGame = do
  deck0 <- initDeck
  deck1 <- initDeck

  return $
    Game { _players = (True, (initialDraw deck0, initialDraw deck1)),_field = Field $ replicate 5 (replicate 3 Nothing)}
