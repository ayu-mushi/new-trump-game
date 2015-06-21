module Main (main) where
import Haste (alert, Elem, toJSString, Event(OnClick), evtName)
import Haste.Foreign (ffi)
import qualified Haste.Perch as P
import Data.Monoid (mconcat)
import Control.Monad (void)

import Control.Lens

import NewTrumpGame.GameState
import NewTrumpGame.Cards

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

indexEl :: (a -> a) -> a -> Elem -> IO a
indexEl s z tag = do
  tag' <- prevElem tag
  case tag' of
    Nothing -> return z
    Just el -> indexEl s (s z) tag

indexOfParentEl :: (a -> a) -> a -> Elem -> IO a
indexOfParentEl s z = (>>=indexEl s z) . P.parent

forTargetWhenEvt :: Elem -> Event IO a -> (Elem -> IO ()) -> IO ()
forTargetWhenEvt el event action = void $ jsAddEventListener el (evtName event) action
  where
    jsAddEventListener :: Elem -> String -> (Elem -> IO ()) -> IO ()
    jsAddEventListener = ffi $ toJSString "(function(node, evtName, f){ node.addEventListener(evtName, (function(e){ f(e.target) }))})"

forIndexOfClickedLiElem :: (a -> a) -> a -> (a -> IO ()) -> Elem -> IO ()
forIndexOfClickedLiElem s z f el = forTargetWhenEvt el OnClick $ 
  \el -> do
    tn <- tagName el
    if tn == "LI"
      then indexEl s z el >>= f
      else return ()

forIndexOfClickedTdElem :: (a -> a) -> (b -> b) -> a -> b -> (a -> b -> IO ()) -> Elem -> IO ()
forIndexOfClickedTdElem as bs az bz f el = forTargetWhenEvt el OnClick $
  \el -> do
    tn <- tagName el
    if tn == "TD"
      then do
        x <- indexOfParentEl as az el
        y <- indexEl bs bz el
        f x y
      else
        return ()

main :: IO ()
main = do
  game <- initGame
  body <- P.getBody
  P.build (P.toElem game) body
  return ()
