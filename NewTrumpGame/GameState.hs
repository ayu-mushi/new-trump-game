module NewTrumpGame.GameState
  (initGame, Game, players) where
import Data.Monoid (mconcat, mempty, (<>), mappend)
import qualified Haste.Perch as P
import Haste (alert)
import Haste.DOM (elemsByQS, setAttr)
import Lens.Family2
import Lens.Family2.Unchecked
import Lens.Family2.Stock (_1, _2, both)
import System.Random (RandomGen)

import NewTrumpGame.Cards
import NewTrumpGame.Player

newtype Field = Field { fromField :: [[Maybe Card]] }

instance P.ToElem Field where
  toElem (Field xss) = P.forElems "table#field" $
    mappend P.clear $
      mconcat $ map (P.tr . mconcat . map (P.td . showMaybeCard)) xss
    where
      showMaybeCard mbcard = case mbcard of
        Nothing -> ""
        Just card -> show card

data Phase =
  Draw
  | Hand
  | Sacrifice
    Int -- object of summon
    [Int] -- sucrifices
  | Summon 
    Int 
    [Int]
  | End

instance Show Phase where
  show p = case p of
    Draw          -> "ドロー"
    Hand          -> "手札を選択"
    Sacrifice _ _ -> "生贄を選択"
    Summon    _ _ -> "召喚する位置を選択"
    End           -> "手番を交代"

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

instance P.ToElem Game where
  toElem game = mconcat [
    P.toElem $ game ^. field
   ,P.forElems "#status" $
      mappend P.clear $
        P.toElem $ "-- " ++ (game ^. turnPlayer . playerName) ++ "の番です、" ++ (show $ game ^. phase)
   ,uncurry mappend $ both %~ P.toElem $ game ^. players
   ,let highlightObjOfSummon objOfSummon = P.Perch $ \e -> do { handsEls <- elemsByQS e "#yours ol.hand li"; setAttr (handsEls !! objOfSummon) "id" "obj-of-summon"; return e } in
      case game ^. phase of
        Sacrifice objOfSummon objOfSacr ->
          mappend (highlightObjOfSummon objOfSummon) $
            P.Perch $ \e -> do
              handsEls <- elemsByQS e "#yours ol.hand li"
              mapM_ (setAttr `flip` "class" `flip` "sacrifice") (map (handsEls !!) objOfSacr)
              return e
        Summon objOfSummon objOfSacr ->
          mappend (highlightObjOfSummon objOfSummon) $
            P.Perch $ \e -> do
              handsEls <- elemsByQS e "#yours ol.hand li"
              mapM_ (setAttr `flip` "class" `flip` "sacrifice") (map (handsEls !!) objOfSacr)
              return e
        _ ->
          mempty
    ]

initGame :: RandomGen g => g -> g -> Game
initGame g h = 
    Game {
      _players =
        (initialDraw "あなた" "yours" show $ initDeck g,
          initialDraw "コンピュータ" "computers" (const "?") $ initDeck h)
      , _areYouTurnPlayer = True
      , _phase = Summon 0 [1, 2]
      , _field = Field $ replicate 5 (replicate 3 Nothing)
    }
