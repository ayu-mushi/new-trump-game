{-# LANGUAGE Rank2Types #-}
module Game.BoardTrump.Util where

import qualified Haste.Perch as P
import Haste (alert, Elem, toJSString, Event(OnClick), evtName, setTimeout)
import Haste.Foreign (ffi)
import Lens.Family2 (Lens')
import Lens.Family2.Unchecked (lens)

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

indexEl :: Int -> Elem -> IO Int
indexEl z tag = do
  tag' <- prevElem tag
  case tag' of
    Nothing -> return z
    Just el -> indexEl (succ z) el

indexOfParentEl :: Elem -> IO Int
indexOfParentEl = (>>=indexEl 0) . P.parent

forTargetWhenEvt :: Event IO a -> (Elem -> IO ()) -> P.Perch
forTargetWhenEvt event action = P.Perch $ \e -> do {jsAddEventListener e (evtName event) action; return e}
  where
    jsAddEventListener :: Elem -> String -> (Elem -> IO ()) -> IO ()
    jsAddEventListener = ffi $ toJSString "(function(node, evtName, f){ node.addEventListener(evtName, (function(e){ f(e.target) }))})"

forIndexOfClickedLiElem :: (Int -> IO ()) -> P.Perch
forIndexOfClickedLiElem f = forTargetWhenEvt OnClick $
  \el -> do
    tn <- tagName el
    if tn == "LI"
      then indexEl 0 el >>= f
      else return ()

forIndexOfClickedTdElem :: (Int -> Int -> IO ()) -> P.Perch
forIndexOfClickedTdElem f = forTargetWhenEvt OnClick $
  \el -> do
    tn <- tagName el
    if tn == "TD"
      then do
        x <- indexOfParentEl el
        y <- indexEl 0 el
        f x y
      else
        return ()

ix :: Int -> Lens' [a] a
ix i = lens (!! i) $ \p x -> (take i p) ++ [x] ++ (drop (i+1) p)
