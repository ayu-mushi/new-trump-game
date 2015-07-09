module NewTrumpGame.GameState
  (initGame, Game, players) where
import Data.Monoid (mconcat)
import qualified Haste.Perch as P
import Haste (alert)
import Haste.DOM (elemsByQS, setAttr)
import Lens.Family2
import Lens.Family2.Unchecked
import Lens.Family2.Stock (_1, _2)
import Data.Monoid (mempty)

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
    Int -- object of summon
  | Summon 
    Int -- object of summon
    [Int] -- sucrifices
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

turnPlayer :: Lens' Game Player
turnPlayer = lens getting setting
  where
    getting game = game ^. players . (if game ^. areYouTurnPlayer then _1 else _2)
    setting game p = players . (if game ^. areYouTurnPlayer then _1 else _2) .~ p $ game

highlightObjOfSummon :: Int -> P.Perch
highlightObjOfSummon objOfSummon = P.Perch $
  \e -> do 
    body <- P.getBody
    handsEls <- elemsByQS body "#yours ol.hand li"
    setAttr (handsEls !! objOfSummon) "id" "selected"
    return e

instance P.ToElem Game where
  toElem game = do
    P.toElem $ game ^. field
    P.forElems "#status" $ do
      P.clear
      P.toElem $ "-- " ++ (game ^. turnPlayer . playerName) ++ "の番です、" ++ (show $ game ^. phase)
    let (a, b) = game ^. players
    P.toElem a
    P.toElem b
    case game ^. phase of
      Sacrifice objOfSummon ->
        P.Perch $ \e -> do 
          body <- P.getBody
          handsEls <- elemsByQS body "#yours ol.hand li"
          setAttr (handsEls !! objOfSummon) "id" "selected"
          return e
      Summon objOfSummon objOfSacr ->
        mempty
      _ ->
        mempty

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
      , _phase = Sacrifice 1
      , _field = Field $ replicate 5 (replicate 3 Nothing)
    }
