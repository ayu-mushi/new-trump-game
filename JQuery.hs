module JQuery
  (JQuery,
   childrenEl,
   nextEl,
   prevEl
  ) where
import Haste
import Haste.Prim
import Haste.Foreign

newtype JQuery = JQuery { fromJQuery :: Elem }

ffi' :: FFI a => String -> a
ffi' = ffi . toJSString

childrenEl :: JQuery -> IO JQuery
childrenEl domEl = do 
  elem <- ffi' "(function(domEl){ return domEl.children() })" $ fromJQuery domEl
  return $ JQuery elem

nextEl :: JQuery -> IO JQuery
nextEl domEl = do 
  elem <- ffi' "(function(domEl){ return domEl.next() })" $ fromJQuery domEl
  return $ JQuery elem

prevEl :: JQuery -> IO JQuery
prevEl domEl = do 
  elem <- ffi' "(function(domEl){ return domEl.prev() })" $ fromJQuery domEl
  return $ JQuery elem
