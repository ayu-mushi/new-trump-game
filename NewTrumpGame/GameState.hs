module NewTrumpGame.GameState
  (initGame, Game, players) where
import Data.Monoid (mconcat)
import qualified Haste.Perch as P
import Lens.Family2
import Lens.Family2.Unchecked
import Lens.Family2.Stock (_1, _2)

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

type ZipicAccessor a = [a] -> ([a], [a])
zipNext :: ZipicAccessor a -> ZipicAccessor a
zipNext za xs = ((a:ls), rs)
  where (ls, (a:rs)) = za xs
zipFirst :: ZipicAccessor a -> ZipicAccessor a
zipFirst za xs = ([], xs)

specialIfSelected :: ZipicAccessor a -> [a] -> (a -> b) -> (a -> b) -> [b]
specialIfSelected za xs general special = (reverse $ map general ls) ++ ((special a):(map general rs))
  where (ls, (a:rs)) = za xs

type ZipicAccessorsMul a = [a] -> [[a]]

data Phase =
  Draw
  | Hand
  | Sacrifice
    (ZipicAccessor Card) -- object of summon
  | Summon 
    (ZipicAccessor Card) -- object of summon
    (Maybe (ZipicAccessorsMul Card)) -- sucrifices
  | End

instance Show Phase where
  show p = case p of
    Draw        -> "ドロー"
    Hand        -> "手札を選択"
    Sacrifice _ -> "生贄を選択"
    Summon  _ _ -> "召喚する位置を選択"
    End         -> "手番を交代"

data Game = Game {
  _players :: (Player, Player), -- (players ^. _1 . playerName) == "あなた"
  _areYouTurnPlayer :: Bool,
  _phase :: Phase,
  _field :: Field
  }
players :: Lens' Game (Player, Player); players = lens _players (\p x -> p { _players = x })
areYouTurnPlayer :: Lens' Game Bool; areYouTurnPlayer = lens _areYouTurnPlayer $ \p x -> p { _areYouTurnPlayer = x }
phase :: Lens' Game Phase; phase = lens _phase $ \p x -> p { _phase = x }
field :: Lens' Game Field; field = lens _field (\p x -> p { _field = x})

getTurnPlayer :: Game -> Player
getTurnPlayer game = game ^. players . (if game ^. areYouTurnPlayer then _1 else _2)

instance P.ToElem Game where
  toElem game = do
    P.toElem $ game ^. field
    P.forElems "#status" $ do
      P.clear
      P.toElem $ "-- " ++ ((getTurnPlayer game) ^. playerName) ++ "の番です、" ++ (show $ game ^. phase)
    let (a, b) = game ^. players
    P.toElem a
    P.toElem b

initGame :: IO Game
initGame = do
  deck0 <- initDeck
  deck1 <- initDeck

  return $
    Game {
      _players =
        (initialDraw "あなた" "yours" show deck0,
          initialDraw "コンピュータ" "computers" (const "?") deck1)
      , _areYouTurnPlayer = True
      , _phase = Draw
      , _field = Field $ replicate 5 (replicate 3 Nothing)
    }
