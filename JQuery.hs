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

import Lens

newtype JQuery = JQuery { fromJQuery :: Elem }

ffi' :: FFI a => String -> a
ffi' = ffi . toJSString

-- "jQueryによる要素の存在チェックまとめ: 小粋空間" http://www.koikikukan.com/archives/2012/04/18-022222.php
isExistEl :: Elem -> IO Bool
isExistEl = ffi' "(function(domEl){return domEl[0]?true:false})"

childrenEl :: JQuery -> IO JQuery
childrenEl domEl = fmap JQuery $
  ffi' "(function(domEl){ return domEl.children() })" $ fromJQuery domEl

nextEl :: JQuery -> IO JQuery
nextEl domEl = fmap JQuery $
  ffi' "(function(domEl){ return domEl.next() })" $ fromJQuery domEl

prevEl :: JQuery -> IO JQuery
prevEl domEl = fmap JQuery $
  ffi' "(function(domEl){ return domEl.prev() })" $ fromJQuery domEl

whenClick :: JQuery -> (Elem -> IO ()) -> IO ()
whenClick domEl method = ffi' "(function(domEl, f){ domEl.click(function(){ f(this) }) })" (fromJQuery domEl) method
