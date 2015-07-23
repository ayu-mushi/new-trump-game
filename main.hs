module Main (main) where
import Haste (alert, Elem, toJSString, Event(OnClick), evtName, setTimeout)
import Haste.DOM (elemsByQS)
import Haste.Foreign (ffi)
import Control.Concurrent
import qualified Haste.Perch as P
import Control.Monad (void, when)
import Data.Monoid ((<>))
import Data.List (foldr1)
import Lens.Family2
import Lens.Family2.Stock
import System.Random (mkStdGen)

import NewTrumpGame.GameState
import NewTrumpGame.Player
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

refresh :: MVar Game -> IO ()
refresh reftoGame = void $
  withMVar reftoGame $ \game -> do
    body <- P.getBody
    P.build (P.toElem game) body

withTime :: IO () -> IO ()
withTime = setTimeout 1000

appendActWithTime :: IO () -> IO () -> IO ()
appendActWithTime a b = a >> withTime b

concatActWithTime :: [IO ()] -> IO ()
concatActWithTime = foldr1 appendActWithTime

turnChange :: MVar Game -> IO ()
turnChange reftoGame = concatActWithTime [
  return (),
  (modifyMVar_ reftoGame $ return . (phase .~ Draw) . (isYourTurn %~ not)) >> refresh reftoGame,
  (modifyMVar_ reftoGame $ return . draw) >> refresh reftoGame
  ]

whenClickField :: MVar Game -> P.Perch
whenClickField reftoGame = P.forElems "#field" $ forIndexOfClickedTdElem $ \i j -> do
  modifyMVar_ reftoGame $ return . operateWithField i j
  refresh reftoGame
  withMVar reftoGame $ \game -> case game ^. phase of End -> turnChange reftoGame; _ -> return ()
  return ()

whenClickHand :: MVar Game -> P.Perch
whenClickHand reftoGame = P.forElems "#yours ol.hand" $ forIndexOfClickedLiElem $ \i -> do
  modifyMVar_ reftoGame $ return . operateWithHand i
  refresh reftoGame
  withMVar reftoGame $ \game -> case game ^. phase of End -> turnChange reftoGame; _ -> return ()
  return ()

main :: IO ()
main = do
  g <- newStdGen
  h <- newStdGen
  i <- newStdGen
  let game = initGame g h i
  reftoGame <- newMVar game
  body <- P.getBody
  P.build (whenClickHand reftoGame <> whenClickField reftoGame <> P.toElem game) body
  withTime $ modifyMVar_ reftoGame (return . draw) >> refresh reftoGame

  where
    newStdGen = fmap mkStdGen $ ffi $ toJSString "(function(){ return Math.floor(Math.random() * Math.pow(2, 53)); })"
