module Main (main) where
import Data.Array (Array(Array))
import System.Random.Shuffle (shuffleM)
import Haste (alert, Elem, toJSString, Event(OnClick), evtName)
import Haste.Foreign (ffi)
import System.IO.Unsafe (unsafePerformIO)
import qualified Haste.Perch as P (build, PerchM(Perch), Perch, forElems, getBody, parent)
import Control.Monad (void)

import Cards

data Player = Player {
  hand :: [Card],
  deck :: [Card]
  }

newtype Field = Field { fromField :: Array (Int, Int) Card }

data Game = Game {
  players :: (Bool, Player, Player),
  field :: Field
  }

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

main :: IO ()
main = do
  body <- P.getBody
  flip P.build body $ P.forElems "ul" $ P.Perch $ \e -> do
    forIndexOfClickedLiElem (alert . show) e
    return e
  return ()
