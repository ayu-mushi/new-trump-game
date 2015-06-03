module JQuery
  (JQuery,
   childrenEl,
   nextEl,
   prevEl,
   whenClick
  ) where
import Haste (toJSString, Elem)
import Haste.Prim ()
import Haste.Foreign (ffi, FFI)

newtype JQuery = JQuery { fromJQuery :: Elem }

ffi' :: FFI a => String -> a
ffi' = ffi . toJSString

childrenEl :: JQuery -> IO JQuery
childrenEl domEl = do 
  e <- ffi' "(function(domEl){ return domEl.children() })" $ fromJQuery domEl
  return $ JQuery e

nextEl :: JQuery -> IO JQuery
nextEl domEl = do 
  e <- ffi' "(function(domEl){ return domEl.next() })" $ fromJQuery domEl
  return $ JQuery e

prevEl :: JQuery -> IO JQuery
prevEl domEl = do 
  e <- ffi' "(function(domEl){ return domEl.prev() })" $ fromJQuery domEl
  return $ JQuery e

whenClick :: JQuery -> IO () -> IO ()
whenClick = ffi' "(function(domEl, f){ domEl.click(f) })" . fromJQuery
