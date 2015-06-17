module Main (main) where
import Haste (alert, Elem, toJSString, Event(OnClick), evtName)
import Haste.Foreign (ffi)
import System.IO.Unsafe (unsafePerformIO)
import qualified Haste.Perch as P
import Control.Monad (void)
import Data.Monoid (mconcat)

import Cards
import Lens

data Player = Player {
  _hand :: [Card],
  _deck :: [Card]
  }
hand :: Lens' Player [Card]; hand = lens _hand $ \p x -> p { _hand = x}
deck :: Lens' Player [Card]; deck = lens _deck $ \p x -> p { _deck = x}

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
  _players :: (Bool, Player, Player),
  _field :: Field
  }
players :: Lens' Game (Bool, Player, Player); players = lens _players (\p x -> p { _players = x })
field :: Lens' Game Field; field = lens _field (\p x -> p { _field = x})

instance P.ToElem Game where
  toElem game = do
    P.toElem $ game ^. field
    let (turnPlayer, a, b) = game ^. players
    refreshPlayerHtml a "yours"
    refreshPlayerHtml b "computers"
    P.forElems "#turnplayer" $ do
      P.clear
      P.toElem $ "-- " ++ (if turnPlayer then "あなた" else "コンピュータ") ++ "の番です"
    where
      refreshPlayerHtml x name = do
        P.forElems ("#"++name++" .deck") $ do
          P.clear
          P.toElem $ "残り山札: " ++ (show $ length $ x ^. deck)
        P.forElems ("#"++name++" .hand") $ do
          P.clear
          mconcat $ map (P.li . show) $ x ^. hand

turnPlayer :: (Bool, Player, Player) -> Player
turnPlayer (p, a, b) = if p then a else b

tagName :: Elem -> IO String
tagName = ffi $ toJSString "(function(e){ return e.tagName })"

prevElem :: Elem -> IO (Maybe Elem)
prevElem el = do
  prevNode <- jsPreviousSibling el
  is0 <- isNull prevNode
  if is0
    then return Nothing
    else do
      isEl <- isElementNode prevNode
      if isEl
        then return $ Just prevNode
        else prevElem prevNode
  where
    jsPreviousSibling = ffi $ toJSString "(function(e){ return e.previousSibling })"
    isNull = ffi $ toJSString "(function(x) {return x === null})"
    isElementNode = ffi $ toJSString "(function(node) {return node.nodeType === 1})"

indexEl :: Elem -> IO Int
indexEl = (indexEl' `flip` 0)
  where
    indexEl' :: Elem -> Int -> IO Int
    indexEl' tag i = do
      tag' <- prevElem tag
      case tag' of
        Nothing -> return i
        Just el -> indexEl' el $ succ i

indexOfParentEl :: Elem -> IO Int
indexOfParentEl = (>>=indexEl) . P.parent

forTargetWhenEvt :: Elem -> Event IO a -> (Elem -> IO ()) -> IO ()
forTargetWhenEvt el event action = void $ jsAddEventListener el (evtName event) action
  where
    jsAddEventListener :: Elem -> String -> (Elem -> IO ()) -> IO ()
    jsAddEventListener = ffi $ toJSString "(function(node, evtName, f){ node.addEventListener(evtName, (function(e){ f(e.target) }))})"

forIndexOfClickedLiElem :: (Int -> IO ()) -> Elem -> IO ()
forIndexOfClickedLiElem f el = forTargetWhenEvt el OnClick $ 
  \el -> do
    tn <- tagName el
    if tn == "LI"
      then indexEl el >>= f
      else return ()

forIndexOfClickedTdElem :: (Int -> Int -> IO ()) -> Elem -> IO ()
forIndexOfClickedTdElem f el = forTargetWhenEvt el OnClick $
  \el -> do
    tn <- tagName el
    if tn == "TD"
      then do
        x <- indexOfParentEl el
        y <- indexEl el
        f x y
      else
        return ()

initialDraw :: [Card] -> Player
initialDraw deck = Player (take 3 deck) (drop 3 deck)

initGame :: IO Game
initGame = do
  deck0 <- initDeck
  deck1 <- initDeck

  return $
    Game { _players = (True, initialDraw deck0, initialDraw deck1),_field = Field $ replicate 5 (replicate 3 Nothing)}

main :: IO ()
main = do
  game <- initGame
  body <- P.getBody
  P.build (P.toElem game) body
  return ()
