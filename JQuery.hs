module JQuery
  (JQuery,
   nextEl,
   prevEl
  ) where
import Haste
import Haste.Prim
import Haste.Foreign

newtype JQuery = JQuery { fromJQuery :: Elem }

nextEl :: JQuery -> IO JQuery
nextEl domEl = do 
  elem <- jsNextEl $ fromJQuery domEl
  return $ JQuery elem
  where
    jsNextEl :: Elem -> IO Elem
    jsNextEl = ffi $ toJSString "(function(domEl){ return domEl.next() })"

prevEl :: JQuery -> IO JQuery
prevEl domEl = do 
  elem <- jsPrevEl $ fromJQuery domEl
  return $ JQuery elem
  where
    jsPrevEl :: Elem -> IO Elem
    jsPrevEl = ffi $ toJSString "(function(domEl){ return domEl.prev() })"
