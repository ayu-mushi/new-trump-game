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

-- "jQueryによる要素の存在チェックまとめ: 小粋空間" http://www.koikikukan.com/archives/2012/04/18-022222.php
isExistEl :: Elem -> IO Bool
isExistEl = ffi' "(function(domEl){return domEl[0]?true:false})"

childrenEl :: JQuery -> IO JQuery
childrenEl domEl = do 
  e <- ffi' "(function(domEl){ return domEl.children() })" $ fromJQuery domEl
  return $ JQuery e

nextEl :: JQuery -> IO (Maybe JQuery)
nextEl domEl = do 
  e <- ffi' "(function(domEl){ return domEl.next() })" $ fromJQuery domEl
  p <- isExistEl e
  return $ if p then Just $ JQuery e else Nothing

prevEl :: JQuery -> IO (Maybe JQuery)
prevEl domEl = do 
  e <- ffi' "(function(domEl){ return domEl.prev() })" $ fromJQuery domEl
  p <- isExistEl e
  return $ if p then Just $ JQuery e else Nothing

whenClick :: JQuery -> IO () -> IO ()
whenClick = ffi' "(function(domEl, f){ domEl.click(f) })" . fromJQuery
