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
import Control.Monad (forM_, when)
import Data.Maybe (isNothing, fromJust)

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

selectObjOfSummon :: Int -> Game -> Game
selectObjOfSummon i game = let yourHand = game ^. players . _1 . hand in
  if isColored $ yourHand !! i
    then if 0 == (cost $ fromJust $ fromCard $ yourHand !! i)
      then phase .~ Summon i [] $ game
      else phase .~ Sacrifice i [] $ game
    else game

selectSacrifice :: Int -> Game -> Game
selectSacrifice i game = let yourHand = game ^. players . _1 . hand in
  case game ^. phase of
    Sacrifice objOfSummon sacrifices ->
      if (cost $ fromJust $ fromCard $ yourHand!!objOfSummon) < (foldr (+) 0 $ map (energy . (yourHand!!)) $ i:sacrifices)
        then phase .~ (Sacrifice objOfSummon $ i:sacrifices) $ game
        else phase .~ (Summon objOfSummon $ i:sacrifices) $ game
    _ ->
      error "You can select sacrifice if and only if it is sacrifice phase and is your turn"

gap :: ([a], [a]) -> a
gap (_, (a:_)) = a

summon :: ([Maybe Card] -> ([Maybe Card], [Maybe Card])) -> Game -> Game
summon cut game = let yourHand = game ^. players . _1 . hand in
  case game ^. phase of
    Summon objOfSummon sacrifices ->
      if isNothing $ gap $ cut $ last $ fromField $ game ^. field
        then
          game & players . _1 . hand .~ map (^._2) [ixedCard | ixedCard <- (zip [0..] yourHand), n <- sacrifices, n == (ixedCard ^. _1) ] & phase .~ End
        else 
          error "it is a havitant, previously"
    _ -> error "You can select summon zone if and only if it is summon phase and is your turn"

instance P.ToElem Game where
  toElem game = mconcat [
    P.toElem $ game ^. field
   ,P.forElems "#status" $
      mappend P.clear $
        P.toElem $ "-- " ++ (game ^. turnPlayer . playerName) ++ "の番です、" ++ (show $ game ^. phase)
   ,uncurry mappend $ both %~ P.toElem $ game ^. players
   ,let highlightObjOfSummon objOfSummon = P.Perch $ \e -> do { handsLis <- elemsByQS e "#yours ol.hand li"; setAttr (handsLis !! objOfSummon) "id" "obj-of-summon"; return e } in
      case game ^. phase of
        Sacrifice objOfSummon objOfSacr ->
          mappend (highlightObjOfSummon objOfSummon) $
            P.Perch $ \e -> do
              handsLis <- elemsByQS e "#yours ol.hand li"
              mapM_ (setAttr `flip` "class" `flip` "sacrifice") (map (handsLis !!) objOfSacr)
              return e
        Summon objOfSummon objOfSacr ->
          mappend (highlightObjOfSummon objOfSummon) $
            P.Perch $ \e -> do
              handsLis <- elemsByQS e "#yours ol.hand li"
              mapM_ (setAttr `flip` "class" `flip` "sacrifice") (map (handsLis !!) objOfSacr)
              fieldTrs <- elemsByQS e "#field tr"
              mostUnderTds <- elemsByQS (last fieldTrs) "td"
              forM_ (zip mostUnderTds (map isNothing $ last $ fromField $ game ^. field)) $
                \(eachTd, isNotLived) -> when isNotLived $ setAttr eachTd "class" "summonable-zone"
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
