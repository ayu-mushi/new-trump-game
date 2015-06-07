module JQuery
  (JQuery(fromJQuery),
   DOMObj(fromDOMObj),
   childrenEl,
   nextEl,
   prevEl,
   whenClick
  ) where
import Haste (toJSString, Elem)
import Haste.Prim ()
import Haste.Foreign (ffi, FFI)

import Lens
import Html5

newtype JQuery = JQuery { fromJQuery :: Elem }
newtype DOMObj = DOMObj { fromDOMObj :: Elem }

sizeEl :: JQuery -> IO Int
sizeEl = ffi' "(function(domEl){ return domEl.size() })" . fromJQuery

ffi' :: FFI a => String -> a
ffi' = ffi . toJSString

ready :: IO () -> IO ()
ready = ffi' "(function (f) { jQuery(document).ready(f) })"

-- "jQueryによる要素の存在チェックまとめ: 小粋空間" http://www.koikikukan.com/archives/2012/04/18-022222.php
isExistEl :: JQuery -> IO Bool
isExistEl = ffi' "(function(domEl){return domEl[0]?true:false})" . fromJQuery

childrenEl :: JQuery -> IO JQuery
childrenEl = fmap JQuery . ffi' "(function(domEl){ return domEl.children() })" . fromJQuery

nextEl :: JQuery -> IO JQuery
nextEl = fmap JQuery . ffi' "(function(domEl){ return domEl.next() })" . fromJQuery

prevEl :: JQuery -> IO JQuery
prevEl = fmap JQuery . ffi' "(function(domEl){ return domEl.prev() })" . fromJQuery

whenClick :: JQuery -> (DOMObj -> IO ()) -> IO ()
whenClick (JQuery domEl) method = ffi' "(function(domEl, f){ domEl.click(function(){ f(this) }) })" domEl $ method . DOMObj

appendEl :: Html5Elem -> JQuery -> IO ()
appendEl e = ffi' "(function(str, domEl){ domEl.append(str) })" (show e) . fromJQuery

indexEl :: JQuery -> DOMObj -> IO (Maybe Int)
indexEl (JQuery domEl) (DOMObj subject) = fmap (\n -> if n == -1 then Nothing else Just n)
  $ ffi' "(function(domEl, subj){ return domEl.index(subj) })" domEl subject
