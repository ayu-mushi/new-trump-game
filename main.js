// This object will hold all exports.
var Haste = {};

/* Thunk
   Creates a thunk representing the given closure.
   If the non-updatable flag is undefined, the thunk is updatable.
*/
function T(f, nu) {
    this.f = f;
    if(nu === undefined) {
        this.x = __updatable;
    }
}

function F(f) {
    this.f = f;
}

// Special object used for blackholing.
var __blackhole = {};

// Used to indicate that an object is updatable.
var __updatable = {};

/* Apply
   Applies the function f to the arguments args. If the application is under-
   saturated, a closure is returned, awaiting further arguments. If it is over-
   saturated, the function is fully applied, and the result (assumed to be a
   function) is then applied to the remaining arguments.
*/
function A(f, args) {
    if(f instanceof T) {
        f = E(f);
    }
    // Closure does some funny stuff with functions that occasionally
    // results in non-functions getting applied, so we have to deal with
    // it.
    if(!(f instanceof Function)) {
        f = B(f);
        if(!(f instanceof Function)) {
            return f;
        }
    }

    if(f.arity === undefined) {
        f.arity = f.length;
    }
    if(args.length === f.arity) {
        switch(f.arity) {
            case 0:  return f();
            case 1:  return f(args[0]);
            default: return f.apply(null, args);
        }
    } else if(args.length > f.arity) {
        switch(f.arity) {
            case 0:  return f();
            case 1:  return A(f(args.shift()), args);
            default: return A(f.apply(null, args.splice(0, f.arity)), args);
        }
    } else {
        var g = function() {
            return A(f, args.concat(Array.prototype.slice.call(arguments)));
        };
        g.arity = f.arity - args.length;
        return g;
    }
}

/* Eval
   Evaluate the given thunk t into head normal form.
   If the "thunk" we get isn't actually a thunk, just return it.
*/
function E(t) {
    if(t instanceof T) {
        if(t.f !== __blackhole) {
            var f = t.f;
            t.f = __blackhole;
            if(t.x === __updatable) {
                t.x = f();
            } else {
                return f();
            }
        }
        return t.x;
    } else {
        return t;
    }
}

/* Bounce
   Bounce on a trampoline for as long as we get a function back.
*/
function B(f) {
    while(f instanceof F) {
        var fun = f.f;
        f.f = __blackhole;
        f = fun();
    }
    return f;
}

// Export Haste, A, B and E. Haste because we need to preserve exports, A, B
// and E because they're handy for Haste.Foreign.
if(!window) {
    var window = {};
}
window['Haste'] = Haste;
window['A'] = A;
window['E'] = E;
window['B'] = B;


/* Throw an error.
   We need to be able to use throw as an exception so we wrap it in a function.
*/
function die(err) {
    throw err;
}

function quot(a, b) {
    return (a-a%b)/b;
}

function quotRemI(a, b) {
    return [0, (a-a%b)/b, a%b];
}

// 32 bit integer multiplication, with correct overflow behavior
// note that |0 or >>>0 needs to be applied to the result, for int and word
// respectively.
if(Math.imul) {
    var imul = Math.imul;
} else {
    var imul = function(a, b) {
        // ignore high a * high a as the result will always be truncated
        var lows = (a & 0xffff) * (b & 0xffff); // low a * low b
        var aB = (a & 0xffff) * (b & 0xffff0000); // low a * high b
        var bA = (a & 0xffff0000) * (b & 0xffff); // low b * high a
        return lows + aB + bA; // sum will not exceed 52 bits, so it's safe
    }
}

function addC(a, b) {
    var x = a+b;
    return [0, x & 0xffffffff, x > 0x7fffffff];
}

function subC(a, b) {
    var x = a-b;
    return [0, x & 0xffffffff, x < -2147483648];
}

function sinh (arg) {
    return (Math.exp(arg) - Math.exp(-arg)) / 2;
}

function tanh (arg) {
    return (Math.exp(arg) - Math.exp(-arg)) / (Math.exp(arg) + Math.exp(-arg));
}

function cosh (arg) {
    return (Math.exp(arg) + Math.exp(-arg)) / 2;
}

// Scratch space for byte arrays.
var rts_scratchBuf = new ArrayBuffer(8);
var rts_scratchW32 = new Uint32Array(rts_scratchBuf);
var rts_scratchFloat = new Float32Array(rts_scratchBuf);
var rts_scratchDouble = new Float64Array(rts_scratchBuf);

function decodeFloat(x) {
    rts_scratchFloat[0] = x;
    var sign = x < 0 ? -1 : 1;
    var exp = ((rts_scratchW32[0] >> 23) & 0xff) - 150;
    var man = rts_scratchW32[0] & 0x7fffff;
    if(exp === 0) {
        ++exp;
    } else {
        man |= (1 << 23);
    }
    return [0, sign*man, exp];
}

function decodeDouble(x) {
    rts_scratchDouble[0] = x;
    var sign = x < 0 ? -1 : 1;
    var manHigh = rts_scratchW32[1] & 0xfffff;
    var manLow = rts_scratchW32[0];
    var exp = ((rts_scratchW32[1] >> 20) & 0x7ff) - 1075;
    if(exp === 0) {
        ++exp;
    } else {
        manHigh |= (1 << 20);
    }
    return [0, sign, manHigh, manLow, exp];
}

function isFloatFinite(x) {
    return isFinite(x);
}

function isDoubleFinite(x) {
    return isFinite(x);
}

function err(str) {
    die(toJSStr(str));
}

/* unpackCString#
   NOTE: update constructor tags if the code generator starts munging them.
*/
function unCStr(str) {return unAppCStr(str, [0]);}

function unFoldrCStr(str, f, z) {
    var acc = z;
    for(var i = str.length-1; i >= 0; --i) {
        acc = B(A(f, [[0, str.charCodeAt(i)], acc]));
    }
    return acc;
}

function unAppCStr(str, chrs) {
    var i = arguments[2] ? arguments[2] : 0;
    if(i >= str.length) {
        return E(chrs);
    } else {
        return [1,[0,str.charCodeAt(i)],new T(function() {
            return unAppCStr(str,chrs,i+1);
        })];
    }
}

function charCodeAt(str, i) {return str.charCodeAt(i);}

function fromJSStr(str) {
    return unCStr(E(str));
}

function toJSStr(hsstr) {
    var s = '';
    for(var str = E(hsstr); str[0] == 1; str = E(str[2])) {
        s += String.fromCharCode(E(str[1])[1]);
    }
    return s;
}

// newMutVar
function nMV(val) {
    return ({x: val});
}

// readMutVar
function rMV(mv) {
    return mv.x;
}

// writeMutVar
function wMV(mv, val) {
    mv.x = val;
}

// atomicModifyMutVar
function mMV(mv, f) {
    var x = B(A(f, [mv.x]));
    mv.x = x[1];
    return x[2];
}

function localeEncoding() {
    var le = newByteArr(5);
    le['v']['i8'][0] = 'U'.charCodeAt(0);
    le['v']['i8'][1] = 'T'.charCodeAt(0);
    le['v']['i8'][2] = 'F'.charCodeAt(0);
    le['v']['i8'][3] = '-'.charCodeAt(0);
    le['v']['i8'][4] = '8'.charCodeAt(0);
    return le;
}

var isDoubleNaN = isNaN;
var isFloatNaN = isNaN;

function isDoubleInfinite(d) {
    return (d === Infinity);
}
var isFloatInfinite = isDoubleInfinite;

function isDoubleNegativeZero(x) {
    return (x===0 && (1/x)===-Infinity);
}
var isFloatNegativeZero = isDoubleNegativeZero;

function strEq(a, b) {
    return a == b;
}

function strOrd(a, b) {
    if(a < b) {
        return [0];
    } else if(a == b) {
        return [1];
    }
    return [2];
}

function jsCatch(act, handler) {
    try {
        return B(A(act,[0]));
    } catch(e) {
        return B(A(handler,[e, 0]));
    }
}

/* Haste represents constructors internally using 1 for the first constructor,
   2 for the second, etc.
   However, dataToTag should use 0, 1, 2, etc. Also, booleans might be unboxed.
 */
function dataToTag(x) {
    if(x instanceof Array) {
        return x[0];
    } else {
        return x;
    }
}

function __word_encodeDouble(d, e) {
    return d * Math.pow(2,e);
}

var __word_encodeFloat = __word_encodeDouble;
var jsRound = Math.round;
var jsTrunc = Math.trunc ? Math.trunc : function(x) {
    return x < 0 ? Math.ceil(x) : Math.floor(x);
};
function jsRoundW(n) {
    return Math.abs(jsTrunc(n));
}
var realWorld = undefined;
if(typeof _ == 'undefined') {
    var _ = undefined;
}

function popCnt(i) {
    i = i - ((i >> 1) & 0x55555555);
    i = (i & 0x33333333) + ((i >> 2) & 0x33333333);
    return (((i + (i >> 4)) & 0x0F0F0F0F) * 0x01010101) >> 24;
}

function jsAlert(val) {
    if(typeof alert != 'undefined') {
        alert(val);
    } else {
        print(val);
    }
}

function jsLog(val) {
    console.log(val);
}

function jsPrompt(str) {
    var val;
    if(typeof prompt != 'undefined') {
        val = prompt(str);
    } else {
        print(str);
        val = readline();
    }
    return val == undefined ? '' : val.toString();
}

function jsEval(str) {
    var x = eval(str);
    return x == undefined ? '' : x.toString();
}

function isNull(obj) {
    return obj === null;
}

function jsRead(str) {
    return Number(str);
}

function jsShowI(val) {return val.toString();}
function jsShow(val) {
    var ret = val.toString();
    return val == Math.round(val) ? ret + '.0' : ret;
}

function jsGetMouseCoords(e) {
    var posx = 0;
    var posy = 0;
    if (!e) var e = window.event;
    if (e.pageX || e.pageY) 	{
	posx = e.pageX;
	posy = e.pageY;
    }
    else if (e.clientX || e.clientY) 	{
	posx = e.clientX + document.body.scrollLeft
	    + document.documentElement.scrollLeft;
	posy = e.clientY + document.body.scrollTop
	    + document.documentElement.scrollTop;
    }
    return [posx - (e.currentTarget.offsetLeft || 0),
	    posy - (e.currentTarget.offsetTop || 0)];
}

function jsSetCB(elem, evt, cb) {
    // Count return press in single line text box as a change event.
    if(evt == 'change' && elem.type.toLowerCase() == 'text') {
        setCB(elem, 'keyup', function(k) {
            if(k == '\n'.charCodeAt(0)) {
                B(A(cb,[[0,k.keyCode],0]));
            }
        });
    }

    var fun;
    switch(evt) {
    case 'click':
    case 'dblclick':
    case 'mouseup':
    case 'mousedown':
        fun = function(x) {
            var mpos = jsGetMouseCoords(x);
            var mx = [0,mpos[0]];
            var my = [0,mpos[1]];
            B(A(cb,[[0,x.button],[0,mx,my],0]));
        };
        break;
    case 'mousemove':
    case 'mouseover':
        fun = function(x) {
            var mpos = jsGetMouseCoords(x);
            var mx = [0,mpos[0]];
            var my = [0,mpos[1]];
            B(A(cb,[[0,mx,my],0]));
        };
        break;
    case 'keypress':
    case 'keyup':
    case 'keydown':
        fun = function(x) {B(A(cb,[[0,x.keyCode],0]));};
        break;
    case 'wheel':
        fun = function(x) {
            var mpos = jsGetMouseCoords(x);
            var mx = [0,mpos[0]];
            var my = [0,mpos[1]];
            var mdx = [0,x.deltaX];
            var mdy = [0,x.deltaY];
            var mdz = [0,x.deltaZ];
            B(A(cb,[[0,mx,my],[0,mdx,mdy,mdz],0]));
        };
        break;
    default:
        fun = function() {B(A(cb,[0]));};
        break;
    }
    return setCB(elem, evt, fun);
}

function setCB(elem, evt, cb) {
    if(elem.addEventListener) {
        elem.addEventListener(evt, cb, false);
        return true;
    } else if(elem.attachEvent) {
        elem.attachEvent('on'+evt, cb);
        return true;
    }
    return false;
}

function jsSetTimeout(msecs, cb) {
    window.setTimeout(function() {B(A(cb,[0]));}, msecs);
}

function jsGet(elem, prop) {
    return elem[prop].toString();
}

function jsSet(elem, prop, val) {
    elem[prop] = val;
}

function jsGetAttr(elem, prop) {
    if(elem.hasAttribute(prop)) {
        return elem.getAttribute(prop).toString();
    } else {
        return "";
    }
}

function jsSetAttr(elem, prop, val) {
    elem.setAttribute(prop, val);
}

function jsGetStyle(elem, prop) {
    return elem.style[prop].toString();
}

function jsSetStyle(elem, prop, val) {
    elem.style[prop] = val;
}

function jsKillChild(child, parent) {
    parent.removeChild(child);
}

function jsClearChildren(elem) {
    while(elem.hasChildNodes()){
        elem.removeChild(elem.lastChild);
    }
}

function jsFind(elem) {
    var e = document.getElementById(elem)
    if(e) {
        return [1,[0,e]];
    }
    return [0];
}

function jsElemsByClassName(cls) {
    var es = document.getElementsByClassName(cls);
    var els = [0];

    for (var i = es.length-1; i >= 0; --i) {
        els = [1, [0, es[i]], els];
    }
    return els;
}

function jsQuerySelectorAll(elem, query) {
    var els = [0], nl;

    if (!elem || typeof elem.querySelectorAll !== 'function') {
        return els;
    }

    nl = elem.querySelectorAll(query);

    for (var i = nl.length-1; i >= 0; --i) {
        els = [1, [0, nl[i]], els];
    }

    return els;
}

function jsCreateElem(tag) {
    return document.createElement(tag);
}

function jsCreateTextNode(str) {
    return document.createTextNode(str);
}

function jsGetChildBefore(elem) {
    elem = elem.previousSibling;
    while(elem) {
        if(typeof elem.tagName != 'undefined') {
            return [1,[0,elem]];
        }
        elem = elem.previousSibling;
    }
    return [0];
}

function jsGetLastChild(elem) {
    var len = elem.childNodes.length;
    for(var i = len-1; i >= 0; --i) {
        if(typeof elem.childNodes[i].tagName != 'undefined') {
            return [1,[0,elem.childNodes[i]]];
        }
    }
    return [0];
}


function jsGetFirstChild(elem) {
    var len = elem.childNodes.length;
    for(var i = 0; i < len; i++) {
        if(typeof elem.childNodes[i].tagName != 'undefined') {
            return [1,[0,elem.childNodes[i]]];
        }
    }
    return [0];
}


function jsGetChildren(elem) {
    var children = [0];
    var len = elem.childNodes.length;
    for(var i = len-1; i >= 0; --i) {
        if(typeof elem.childNodes[i].tagName != 'undefined') {
            children = [1, [0,elem.childNodes[i]], children];
        }
    }
    return children;
}

function jsSetChildren(elem, children) {
    children = E(children);
    jsClearChildren(elem, 0);
    while(children[0] === 1) {
        elem.appendChild(E(E(children[1])[1]));
        children = E(children[2]);
    }
}

function jsAppendChild(child, container) {
    container.appendChild(child);
}

function jsAddChildBefore(child, container, after) {
    container.insertBefore(child, after);
}

var jsRand = Math.random;

// Concatenate a Haskell list of JS strings
function jsCat(strs, sep) {
    var arr = [];
    strs = E(strs);
    while(strs[0]) {
        strs = E(strs);
        arr.push(E(strs[1])[1]);
        strs = E(strs[2]);
    }
    return arr.join(sep);
}

var jsJSONParse = JSON.parse;

// JSON stringify a string
function jsStringify(str) {
    return JSON.stringify(str);
}

// Parse a JSON message into a Haste.JSON.JSON value.
// As this pokes around inside Haskell values, it'll need to be updated if:
// * Haste.JSON.JSON changes;
// * E() starts to choke on non-thunks;
// * data constructor code generation changes; or
// * Just and Nothing change tags.
function jsParseJSON(str) {
    try {
        var js = JSON.parse(str);
        var hs = toHS(js);
    } catch(_) {
        return [0];
    }
    return [1,hs];
}

function toHS(obj) {
    switch(typeof obj) {
    case 'number':
        return [0, jsRead(obj)];
    case 'string':
        return [1, obj];
    case 'boolean':
        return [2, obj]; // Booleans are special wrt constructor tags!
    case 'object':
        if(obj instanceof Array) {
            return [3, arr2lst_json(obj, 0)];
        } else if (obj == null) {
            return [5];
        } else {
            // Object type but not array - it's a dictionary.
            // The RFC doesn't say anything about the ordering of keys, but
            // considering that lots of people rely on keys being "in order" as
            // defined by "the same way someone put them in at the other end,"
            // it's probably a good idea to put some cycles into meeting their
            // misguided expectations.
            var ks = [];
            for(var k in obj) {
                ks.unshift(k);
            }
            var xs = [0];
            for(var i = 0; i < ks.length; i++) {
                xs = [1, [0, [0,ks[i]], toHS(obj[ks[i]])], xs];
            }
            return [4, xs];
        }
    }
}

function arr2lst_json(arr, elem) {
    if(elem >= arr.length) {
        return [0];
    }
    return [1, toHS(arr[elem]), new T(function() {return arr2lst_json(arr,elem+1);}),true]
}

function arr2lst(arr, elem) {
    if(elem >= arr.length) {
        return [0];
    }
    return [1, arr[elem], new T(function() {return arr2lst(arr,elem+1);})]
}
window['arr2lst'] = arr2lst;

function lst2arr(xs) {
    var arr = [];
    for(; xs[0]; xs = E(xs[2])) {
        arr.push(E(xs[1]));
    }
    return arr;
}
window['lst2arr'] = lst2arr;

function ajaxReq(method, url, async, postdata, cb) {
    var xhr = new XMLHttpRequest();
    xhr.open(method, url, async);

    if(method == "POST") {
        xhr.setRequestHeader("Content-type",
                             "application/x-www-form-urlencoded");
    }
    xhr.onreadystatechange = function() {
        if(xhr.readyState == 4) {
            if(xhr.status == 200) {
                B(A(cb,[[1,[0,xhr.responseText]],0]));
            } else {
                B(A(cb,[[0],0])); // Nothing
            }
        }
    }
    xhr.send(postdata);
}

// Create a little endian ArrayBuffer representation of something.
function toABHost(v, n, x) {
    var a = new ArrayBuffer(n);
    new window[v](a)[0] = x;
    return a;
}

function toABSwap(v, n, x) {
    var a = new ArrayBuffer(n);
    new window[v](a)[0] = x;
    var bs = new Uint8Array(a);
    for(var i = 0, j = n-1; i < j; ++i, --j) {
        var tmp = bs[i];
        bs[i] = bs[j];
        bs[j] = tmp;
    }
    return a;
}

window['toABle'] = toABHost;
window['toABbe'] = toABSwap;

// Swap byte order if host is not little endian.
var buffer = new ArrayBuffer(2);
new DataView(buffer).setInt16(0, 256, true);
if(new Int16Array(buffer)[0] !== 256) {
    window['toABle'] = toABSwap;
    window['toABbe'] = toABHost;
}

// MVar implementation.
// Since Haste isn't concurrent, takeMVar and putMVar don't block on empty
// and full MVars respectively, but terminate the program since they would
// otherwise be blocking forever.

function newMVar() {
    return ({empty: true});
}

function tryTakeMVar(mv) {
    if(mv.empty) {
        return [0, 0, undefined];
    } else {
        var val = mv.x;
        mv.empty = true;
        mv.x = null;
        return [0, 1, val];
    }
}

function takeMVar(mv) {
    if(mv.empty) {
        // TODO: real BlockedOnDeadMVar exception, perhaps?
        err("Attempted to take empty MVar!");
    }
    var val = mv.x;
    mv.empty = true;
    mv.x = null;
    return val;
}

function putMVar(mv, val) {
    if(!mv.empty) {
        // TODO: real BlockedOnDeadMVar exception, perhaps?
        err("Attempted to put full MVar!");
    }
    mv.empty = false;
    mv.x = val;
}

function tryPutMVar(mv, val) {
    if(!mv.empty) {
        return 0;
    } else {
        mv.empty = false;
        mv.x = val;
        return 1;
    }
}

function sameMVar(a, b) {
    return (a == b);
}

function isEmptyMVar(mv) {
    return mv.empty ? 1 : 0;
}

// Implementation of stable names.
// Unlike native GHC, the garbage collector isn't going to move data around
// in a way that we can detect, so each object could serve as its own stable
// name if it weren't for the fact we can't turn a JS reference into an
// integer.
// So instead, each object has a unique integer attached to it, which serves
// as its stable name.

var __next_stable_name = 1;

function makeStableName(x) {
    if(!x.stableName) {
        x.stableName = __next_stable_name;
        __next_stable_name += 1;
    }
    return x.stableName;
}

function eqStableName(x, y) {
    return (x == y) ? 1 : 0;
}

var Integer = function(bits, sign) {
  this.bits_ = [];
  this.sign_ = sign;

  var top = true;
  for (var i = bits.length - 1; i >= 0; i--) {
    var val = bits[i] | 0;
    if (!top || val != sign) {
      this.bits_[i] = val;
      top = false;
    }
  }
};

Integer.IntCache_ = {};

var I_fromInt = function(value) {
  if (-128 <= value && value < 128) {
    var cachedObj = Integer.IntCache_[value];
    if (cachedObj) {
      return cachedObj;
    }
  }

  var obj = new Integer([value | 0], value < 0 ? -1 : 0);
  if (-128 <= value && value < 128) {
    Integer.IntCache_[value] = obj;
  }
  return obj;
};

var I_fromNumber = function(value) {
  if (isNaN(value) || !isFinite(value)) {
    return Integer.ZERO;
  } else if (value < 0) {
    return I_negate(I_fromNumber(-value));
  } else {
    var bits = [];
    var pow = 1;
    for (var i = 0; value >= pow; i++) {
      bits[i] = (value / pow) | 0;
      pow *= Integer.TWO_PWR_32_DBL_;
    }
    return new Integer(bits, 0);
  }
};

var I_fromBits = function(bits) {
  var high = bits[bits.length - 1];
  return new Integer(bits, high & (1 << 31) ? -1 : 0);
};

var I_fromString = function(str, opt_radix) {
  if (str.length == 0) {
    throw Error('number format error: empty string');
  }

  var radix = opt_radix || 10;
  if (radix < 2 || 36 < radix) {
    throw Error('radix out of range: ' + radix);
  }

  if (str.charAt(0) == '-') {
    return I_negate(I_fromString(str.substring(1), radix));
  } else if (str.indexOf('-') >= 0) {
    throw Error('number format error: interior "-" character');
  }

  var radixToPower = I_fromNumber(Math.pow(radix, 8));

  var result = Integer.ZERO;
  for (var i = 0; i < str.length; i += 8) {
    var size = Math.min(8, str.length - i);
    var value = parseInt(str.substring(i, i + size), radix);
    if (size < 8) {
      var power = I_fromNumber(Math.pow(radix, size));
      result = I_add(I_mul(result, power), I_fromNumber(value));
    } else {
      result = I_mul(result, radixToPower);
      result = I_add(result, I_fromNumber(value));
    }
  }
  return result;
};


Integer.TWO_PWR_32_DBL_ = (1 << 16) * (1 << 16);
Integer.ZERO = I_fromInt(0);
Integer.ONE = I_fromInt(1);
Integer.TWO_PWR_24_ = I_fromInt(1 << 24);

var I_toInt = function(self) {
  return self.bits_.length > 0 ? self.bits_[0] : self.sign_;
};

var I_toWord = function(self) {
  return I_toInt(self) >>> 0;
};

var I_toNumber = function(self) {
  if (isNegative(self)) {
    return -I_toNumber(I_negate(self));
  } else {
    var val = 0;
    var pow = 1;
    for (var i = 0; i < self.bits_.length; i++) {
      val += I_getBitsUnsigned(self, i) * pow;
      pow *= Integer.TWO_PWR_32_DBL_;
    }
    return val;
  }
};

var I_getBits = function(self, index) {
  if (index < 0) {
    return 0;
  } else if (index < self.bits_.length) {
    return self.bits_[index];
  } else {
    return self.sign_;
  }
};

var I_getBitsUnsigned = function(self, index) {
  var val = I_getBits(self, index);
  return val >= 0 ? val : Integer.TWO_PWR_32_DBL_ + val;
};

var getSign = function(self) {
  return self.sign_;
};

var isZero = function(self) {
  if (self.sign_ != 0) {
    return false;
  }
  for (var i = 0; i < self.bits_.length; i++) {
    if (self.bits_[i] != 0) {
      return false;
    }
  }
  return true;
};

var isNegative = function(self) {
  return self.sign_ == -1;
};

var isOdd = function(self) {
  return (self.bits_.length == 0) && (self.sign_ == -1) ||
         (self.bits_.length > 0) && ((self.bits_[0] & 1) != 0);
};

var I_equals = function(self, other) {
  if (self.sign_ != other.sign_) {
    return false;
  }
  var len = Math.max(self.bits_.length, other.bits_.length);
  for (var i = 0; i < len; i++) {
    if (I_getBits(self, i) != I_getBits(other, i)) {
      return false;
    }
  }
  return true;
};

var I_notEquals = function(self, other) {
  return !I_equals(self, other);
};

var I_greaterThan = function(self, other) {
  return I_compare(self, other) > 0;
};

var I_greaterThanOrEqual = function(self, other) {
  return I_compare(self, other) >= 0;
};

var I_lessThan = function(self, other) {
  return I_compare(self, other) < 0;
};

var I_lessThanOrEqual = function(self, other) {
  return I_compare(self, other) <= 0;
};

var I_compare = function(self, other) {
  var diff = I_sub(self, other);
  if (isNegative(diff)) {
    return -1;
  } else if (isZero(diff)) {
    return 0;
  } else {
    return +1;
  }
};

var I_compareInt = function(self, other) {
  return I_compare(self, I_fromInt(other));
}

var shorten = function(self, numBits) {
  var arr_index = (numBits - 1) >> 5;
  var bit_index = (numBits - 1) % 32;
  var bits = [];
  for (var i = 0; i < arr_index; i++) {
    bits[i] = I_getBits(self, i);
  }
  var sigBits = bit_index == 31 ? 0xFFFFFFFF : (1 << (bit_index + 1)) - 1;
  var val = I_getBits(self, arr_index) & sigBits;
  if (val & (1 << bit_index)) {
    val |= 0xFFFFFFFF - sigBits;
    bits[arr_index] = val;
    return new Integer(bits, -1);
  } else {
    bits[arr_index] = val;
    return new Integer(bits, 0);
  }
};

var I_negate = function(self) {
  return I_add(not(self), Integer.ONE);
};

var I_add = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  var carry = 0;

  for (var i = 0; i <= len; i++) {
    var a1 = I_getBits(self, i) >>> 16;
    var a0 = I_getBits(self, i) & 0xFFFF;

    var b1 = I_getBits(other, i) >>> 16;
    var b0 = I_getBits(other, i) & 0xFFFF;

    var c0 = carry + a0 + b0;
    var c1 = (c0 >>> 16) + a1 + b1;
    carry = c1 >>> 16;
    c0 &= 0xFFFF;
    c1 &= 0xFFFF;
    arr[i] = (c1 << 16) | c0;
  }
  return I_fromBits(arr);
};

var I_sub = function(self, other) {
  return I_add(self, I_negate(other));
};

var I_mul = function(self, other) {
  if (isZero(self)) {
    return Integer.ZERO;
  } else if (isZero(other)) {
    return Integer.ZERO;
  }

  if (isNegative(self)) {
    if (isNegative(other)) {
      return I_mul(I_negate(self), I_negate(other));
    } else {
      return I_negate(I_mul(I_negate(self), other));
    }
  } else if (isNegative(other)) {
    return I_negate(I_mul(self, I_negate(other)));
  }

  if (I_lessThan(self, Integer.TWO_PWR_24_) &&
      I_lessThan(other, Integer.TWO_PWR_24_)) {
    return I_fromNumber(I_toNumber(self) * I_toNumber(other));
  }

  var len = self.bits_.length + other.bits_.length;
  var arr = [];
  for (var i = 0; i < 2 * len; i++) {
    arr[i] = 0;
  }
  for (var i = 0; i < self.bits_.length; i++) {
    for (var j = 0; j < other.bits_.length; j++) {
      var a1 = I_getBits(self, i) >>> 16;
      var a0 = I_getBits(self, i) & 0xFFFF;

      var b1 = I_getBits(other, j) >>> 16;
      var b0 = I_getBits(other, j) & 0xFFFF;

      arr[2 * i + 2 * j] += a0 * b0;
      Integer.carry16_(arr, 2 * i + 2 * j);
      arr[2 * i + 2 * j + 1] += a1 * b0;
      Integer.carry16_(arr, 2 * i + 2 * j + 1);
      arr[2 * i + 2 * j + 1] += a0 * b1;
      Integer.carry16_(arr, 2 * i + 2 * j + 1);
      arr[2 * i + 2 * j + 2] += a1 * b1;
      Integer.carry16_(arr, 2 * i + 2 * j + 2);
    }
  }

  for (var i = 0; i < len; i++) {
    arr[i] = (arr[2 * i + 1] << 16) | arr[2 * i];
  }
  for (var i = len; i < 2 * len; i++) {
    arr[i] = 0;
  }
  return new Integer(arr, 0);
};

Integer.carry16_ = function(bits, index) {
  while ((bits[index] & 0xFFFF) != bits[index]) {
    bits[index + 1] += bits[index] >>> 16;
    bits[index] &= 0xFFFF;
  }
};

var I_mod = function(self, other) {
  return I_rem(I_add(other, I_rem(self, other)), other);
}

var I_div = function(self, other) {
  if(I_greaterThan(self, Integer.ZERO) != I_greaterThan(other, Integer.ZERO)) {
    if(I_rem(self, other) != Integer.ZERO) {
      return I_sub(I_quot(self, other), Integer.ONE);
    }
  }
  return I_quot(self, other);
}

var I_quotRem = function(self, other) {
  return [0, I_quot(self, other), I_rem(self, other)];
}

var I_divMod = function(self, other) {
  return [0, I_div(self, other), I_mod(self, other)];
}

var I_quot = function(self, other) {
  if (isZero(other)) {
    throw Error('division by zero');
  } else if (isZero(self)) {
    return Integer.ZERO;
  }

  if (isNegative(self)) {
    if (isNegative(other)) {
      return I_quot(I_negate(self), I_negate(other));
    } else {
      return I_negate(I_quot(I_negate(self), other));
    }
  } else if (isNegative(other)) {
    return I_negate(I_quot(self, I_negate(other)));
  }

  var res = Integer.ZERO;
  var rem = self;
  while (I_greaterThanOrEqual(rem, other)) {
    var approx = Math.max(1, Math.floor(I_toNumber(rem) / I_toNumber(other)));
    var log2 = Math.ceil(Math.log(approx) / Math.LN2);
    var delta = (log2 <= 48) ? 1 : Math.pow(2, log2 - 48);
    var approxRes = I_fromNumber(approx);
    var approxRem = I_mul(approxRes, other);
    while (isNegative(approxRem) || I_greaterThan(approxRem, rem)) {
      approx -= delta;
      approxRes = I_fromNumber(approx);
      approxRem = I_mul(approxRes, other);
    }

    if (isZero(approxRes)) {
      approxRes = Integer.ONE;
    }

    res = I_add(res, approxRes);
    rem = I_sub(rem, approxRem);
  }
  return res;
};

var I_rem = function(self, other) {
  return I_sub(self, I_mul(I_quot(self, other), other));
};

var not = function(self) {
  var len = self.bits_.length;
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = ~self.bits_[i];
  }
  return new Integer(arr, ~self.sign_);
};

var I_and = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = I_getBits(self, i) & I_getBits(other, i);
  }
  return new Integer(arr, self.sign_ & other.sign_);
};

var I_or = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = I_getBits(self, i) | I_getBits(other, i);
  }
  return new Integer(arr, self.sign_ | other.sign_);
};

var I_xor = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = I_getBits(self, i) ^ I_getBits(other, i);
  }
  return new Integer(arr, self.sign_ ^ other.sign_);
};

var I_shiftLeft = function(self, numBits) {
  var arr_delta = numBits >> 5;
  var bit_delta = numBits % 32;
  var len = self.bits_.length + arr_delta + (bit_delta > 0 ? 1 : 0);
  var arr = [];
  for (var i = 0; i < len; i++) {
    if (bit_delta > 0) {
      arr[i] = (I_getBits(self, i - arr_delta) << bit_delta) |
               (I_getBits(self, i - arr_delta - 1) >>> (32 - bit_delta));
    } else {
      arr[i] = I_getBits(self, i - arr_delta);
    }
  }
  return new Integer(arr, self.sign_);
};

var I_shiftRight = function(self, numBits) {
  var arr_delta = numBits >> 5;
  var bit_delta = numBits % 32;
  var len = self.bits_.length - arr_delta;
  var arr = [];
  for (var i = 0; i < len; i++) {
    if (bit_delta > 0) {
      arr[i] = (I_getBits(self, i + arr_delta) >>> bit_delta) |
               (I_getBits(self, i + arr_delta + 1) << (32 - bit_delta));
    } else {
      arr[i] = I_getBits(self, i + arr_delta);
    }
  }
  return new Integer(arr, self.sign_);
};

var I_signum = function(self) {
  var cmp = I_compare(self, Integer.ZERO);
  if(cmp > 0) {
    return Integer.ONE
  }
  if(cmp < 0) {
    return I_sub(Integer.ZERO, Integer.ONE);
  }
  return Integer.ZERO;
};

var I_abs = function(self) {
  if(I_compare(self, Integer.ZERO) < 0) {
    return I_sub(Integer.ZERO, self);
  }
  return self;
};

var I_decodeDouble = function(x) {
  var dec = decodeDouble(x);
  var mantissa = I_fromBits([dec[3], dec[2]]);
  if(dec[1] < 0) {
    mantissa = I_negate(mantissa);
  }
  return [0, dec[4], mantissa];
}

var I_toString = function(self) {
  var radix = 10;

  if (isZero(self)) {
    return '0';
  } else if (isNegative(self)) {
    return '-' + I_toString(I_negate(self));
  }

  var radixToPower = I_fromNumber(Math.pow(radix, 6));

  var rem = self;
  var result = '';
  while (true) {
    var remDiv = I_div(rem, radixToPower);
    var intval = I_toInt(I_sub(rem, I_mul(remDiv, radixToPower)));
    var digits = intval.toString();

    rem = remDiv;
    if (isZero(rem)) {
      return digits + result;
    } else {
      while (digits.length < 6) {
        digits = '0' + digits;
      }
      result = '' + digits + result;
    }
  }
};

var I_fromRat = function(a, b) {
    return I_toNumber(a) / I_toNumber(b);
}

function I_fromInt64(x) {
    return I_fromBits([x.getLowBits(), x.getHighBits()]);
}

function I_toInt64(x) {
    return Long.fromBits(I_getBits(x, 0), I_getBits(x, 1));
}

function I_fromWord64(x) {
    return x;
}

function I_toWord64(x) {
    return I_rem(I_add(__w64_max, x), __w64_max);
}

// Copyright 2009 The Closure Library Authors. All Rights Reserved.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//      http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS-IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

var Long = function(low, high) {
  this.low_ = low | 0;
  this.high_ = high | 0;
};

Long.IntCache_ = {};

Long.fromInt = function(value) {
  if (-128 <= value && value < 128) {
    var cachedObj = Long.IntCache_[value];
    if (cachedObj) {
      return cachedObj;
    }
  }

  var obj = new Long(value | 0, value < 0 ? -1 : 0);
  if (-128 <= value && value < 128) {
    Long.IntCache_[value] = obj;
  }
  return obj;
};

Long.fromNumber = function(value) {
  if (isNaN(value) || !isFinite(value)) {
    return Long.ZERO;
  } else if (value <= -Long.TWO_PWR_63_DBL_) {
    return Long.MIN_VALUE;
  } else if (value + 1 >= Long.TWO_PWR_63_DBL_) {
    return Long.MAX_VALUE;
  } else if (value < 0) {
    return Long.fromNumber(-value).negate();
  } else {
    return new Long(
        (value % Long.TWO_PWR_32_DBL_) | 0,
        (value / Long.TWO_PWR_32_DBL_) | 0);
  }
};

Long.fromBits = function(lowBits, highBits) {
  return new Long(lowBits, highBits);
};

Long.TWO_PWR_16_DBL_ = 1 << 16;
Long.TWO_PWR_24_DBL_ = 1 << 24;
Long.TWO_PWR_32_DBL_ =
    Long.TWO_PWR_16_DBL_ * Long.TWO_PWR_16_DBL_;
Long.TWO_PWR_31_DBL_ =
    Long.TWO_PWR_32_DBL_ / 2;
Long.TWO_PWR_48_DBL_ =
    Long.TWO_PWR_32_DBL_ * Long.TWO_PWR_16_DBL_;
Long.TWO_PWR_64_DBL_ =
    Long.TWO_PWR_32_DBL_ * Long.TWO_PWR_32_DBL_;
Long.TWO_PWR_63_DBL_ =
    Long.TWO_PWR_64_DBL_ / 2;
Long.ZERO = Long.fromInt(0);
Long.ONE = Long.fromInt(1);
Long.NEG_ONE = Long.fromInt(-1);
Long.MAX_VALUE =
    Long.fromBits(0xFFFFFFFF | 0, 0x7FFFFFFF | 0);
Long.MIN_VALUE = Long.fromBits(0, 0x80000000 | 0);
Long.TWO_PWR_24_ = Long.fromInt(1 << 24);

Long.prototype.toInt = function() {
  return this.low_;
};

Long.prototype.toNumber = function() {
  return this.high_ * Long.TWO_PWR_32_DBL_ +
         this.getLowBitsUnsigned();
};

Long.prototype.getHighBits = function() {
  return this.high_;
};

Long.prototype.getLowBits = function() {
  return this.low_;
};

Long.prototype.getLowBitsUnsigned = function() {
  return (this.low_ >= 0) ?
      this.low_ : Long.TWO_PWR_32_DBL_ + this.low_;
};

Long.prototype.isZero = function() {
  return this.high_ == 0 && this.low_ == 0;
};

Long.prototype.isNegative = function() {
  return this.high_ < 0;
};

Long.prototype.isOdd = function() {
  return (this.low_ & 1) == 1;
};

Long.prototype.equals = function(other) {
  return (this.high_ == other.high_) && (this.low_ == other.low_);
};

Long.prototype.notEquals = function(other) {
  return (this.high_ != other.high_) || (this.low_ != other.low_);
};

Long.prototype.lessThan = function(other) {
  return this.compare(other) < 0;
};

Long.prototype.lessThanOrEqual = function(other) {
  return this.compare(other) <= 0;
};

Long.prototype.greaterThan = function(other) {
  return this.compare(other) > 0;
};

Long.prototype.greaterThanOrEqual = function(other) {
  return this.compare(other) >= 0;
};

Long.prototype.compare = function(other) {
  if (this.equals(other)) {
    return 0;
  }

  var thisNeg = this.isNegative();
  var otherNeg = other.isNegative();
  if (thisNeg && !otherNeg) {
    return -1;
  }
  if (!thisNeg && otherNeg) {
    return 1;
  }

  if (this.subtract(other).isNegative()) {
    return -1;
  } else {
    return 1;
  }
};

Long.prototype.negate = function() {
  if (this.equals(Long.MIN_VALUE)) {
    return Long.MIN_VALUE;
  } else {
    return this.not().add(Long.ONE);
  }
};

Long.prototype.add = function(other) {
  var a48 = this.high_ >>> 16;
  var a32 = this.high_ & 0xFFFF;
  var a16 = this.low_ >>> 16;
  var a00 = this.low_ & 0xFFFF;

  var b48 = other.high_ >>> 16;
  var b32 = other.high_ & 0xFFFF;
  var b16 = other.low_ >>> 16;
  var b00 = other.low_ & 0xFFFF;

  var c48 = 0, c32 = 0, c16 = 0, c00 = 0;
  c00 += a00 + b00;
  c16 += c00 >>> 16;
  c00 &= 0xFFFF;
  c16 += a16 + b16;
  c32 += c16 >>> 16;
  c16 &= 0xFFFF;
  c32 += a32 + b32;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c48 += a48 + b48;
  c48 &= 0xFFFF;
  return Long.fromBits((c16 << 16) | c00, (c48 << 16) | c32);
};

Long.prototype.subtract = function(other) {
  return this.add(other.negate());
};

Long.prototype.multiply = function(other) {
  if (this.isZero()) {
    return Long.ZERO;
  } else if (other.isZero()) {
    return Long.ZERO;
  }

  if (this.equals(Long.MIN_VALUE)) {
    return other.isOdd() ? Long.MIN_VALUE : Long.ZERO;
  } else if (other.equals(Long.MIN_VALUE)) {
    return this.isOdd() ? Long.MIN_VALUE : Long.ZERO;
  }

  if (this.isNegative()) {
    if (other.isNegative()) {
      return this.negate().multiply(other.negate());
    } else {
      return this.negate().multiply(other).negate();
    }
  } else if (other.isNegative()) {
    return this.multiply(other.negate()).negate();
  }

  if (this.lessThan(Long.TWO_PWR_24_) &&
      other.lessThan(Long.TWO_PWR_24_)) {
    return Long.fromNumber(this.toNumber() * other.toNumber());
  }

  var a48 = this.high_ >>> 16;
  var a32 = this.high_ & 0xFFFF;
  var a16 = this.low_ >>> 16;
  var a00 = this.low_ & 0xFFFF;

  var b48 = other.high_ >>> 16;
  var b32 = other.high_ & 0xFFFF;
  var b16 = other.low_ >>> 16;
  var b00 = other.low_ & 0xFFFF;

  var c48 = 0, c32 = 0, c16 = 0, c00 = 0;
  c00 += a00 * b00;
  c16 += c00 >>> 16;
  c00 &= 0xFFFF;
  c16 += a16 * b00;
  c32 += c16 >>> 16;
  c16 &= 0xFFFF;
  c16 += a00 * b16;
  c32 += c16 >>> 16;
  c16 &= 0xFFFF;
  c32 += a32 * b00;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c32 += a16 * b16;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c32 += a00 * b32;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c48 += a48 * b00 + a32 * b16 + a16 * b32 + a00 * b48;
  c48 &= 0xFFFF;
  return Long.fromBits((c16 << 16) | c00, (c48 << 16) | c32);
};

Long.prototype.div = function(other) {
  if (other.isZero()) {
    throw Error('division by zero');
  } else if (this.isZero()) {
    return Long.ZERO;
  }

  if (this.equals(Long.MIN_VALUE)) {
    if (other.equals(Long.ONE) ||
        other.equals(Long.NEG_ONE)) {
      return Long.MIN_VALUE;
    } else if (other.equals(Long.MIN_VALUE)) {
      return Long.ONE;
    } else {
      var halfThis = this.shiftRight(1);
      var approx = halfThis.div(other).shiftLeft(1);
      if (approx.equals(Long.ZERO)) {
        return other.isNegative() ? Long.ONE : Long.NEG_ONE;
      } else {
        var rem = this.subtract(other.multiply(approx));
        var result = approx.add(rem.div(other));
        return result;
      }
    }
  } else if (other.equals(Long.MIN_VALUE)) {
    return Long.ZERO;
  }

  if (this.isNegative()) {
    if (other.isNegative()) {
      return this.negate().div(other.negate());
    } else {
      return this.negate().div(other).negate();
    }
  } else if (other.isNegative()) {
    return this.div(other.negate()).negate();
  }

  var res = Long.ZERO;
  var rem = this;
  while (rem.greaterThanOrEqual(other)) {
    var approx = Math.max(1, Math.floor(rem.toNumber() / other.toNumber()));

    var log2 = Math.ceil(Math.log(approx) / Math.LN2);
    var delta = (log2 <= 48) ? 1 : Math.pow(2, log2 - 48);

    var approxRes = Long.fromNumber(approx);
    var approxRem = approxRes.multiply(other);
    while (approxRem.isNegative() || approxRem.greaterThan(rem)) {
      approx -= delta;
      approxRes = Long.fromNumber(approx);
      approxRem = approxRes.multiply(other);
    }

    if (approxRes.isZero()) {
      approxRes = Long.ONE;
    }

    res = res.add(approxRes);
    rem = rem.subtract(approxRem);
  }
  return res;
};

Long.prototype.modulo = function(other) {
  return this.subtract(this.div(other).multiply(other));
};

Long.prototype.not = function() {
  return Long.fromBits(~this.low_, ~this.high_);
};

Long.prototype.and = function(other) {
  return Long.fromBits(this.low_ & other.low_,
                                 this.high_ & other.high_);
};

Long.prototype.or = function(other) {
  return Long.fromBits(this.low_ | other.low_,
                                 this.high_ | other.high_);
};

Long.prototype.xor = function(other) {
  return Long.fromBits(this.low_ ^ other.low_,
                                 this.high_ ^ other.high_);
};

Long.prototype.shiftLeft = function(numBits) {
  numBits &= 63;
  if (numBits == 0) {
    return this;
  } else {
    var low = this.low_;
    if (numBits < 32) {
      var high = this.high_;
      return Long.fromBits(
          low << numBits,
          (high << numBits) | (low >>> (32 - numBits)));
    } else {
      return Long.fromBits(0, low << (numBits - 32));
    }
  }
};

Long.prototype.shiftRight = function(numBits) {
  numBits &= 63;
  if (numBits == 0) {
    return this;
  } else {
    var high = this.high_;
    if (numBits < 32) {
      var low = this.low_;
      return Long.fromBits(
          (low >>> numBits) | (high << (32 - numBits)),
          high >> numBits);
    } else {
      return Long.fromBits(
          high >> (numBits - 32),
          high >= 0 ? 0 : -1);
    }
  }
};

Long.prototype.shiftRightUnsigned = function(numBits) {
  numBits &= 63;
  if (numBits == 0) {
    return this;
  } else {
    var high = this.high_;
    if (numBits < 32) {
      var low = this.low_;
      return Long.fromBits(
          (low >>> numBits) | (high << (32 - numBits)),
          high >>> numBits);
    } else if (numBits == 32) {
      return Long.fromBits(high, 0);
    } else {
      return Long.fromBits(high >>> (numBits - 32), 0);
    }
  }
};



// Int64
function hs_eqInt64(x, y) {return x.equals(y);}
function hs_neInt64(x, y) {return !x.equals(y);}
function hs_ltInt64(x, y) {return x.compare(y) < 0;}
function hs_leInt64(x, y) {return x.compare(y) <= 0;}
function hs_gtInt64(x, y) {return x.compare(y) > 0;}
function hs_geInt64(x, y) {return x.compare(y) >= 0;}
function hs_quotInt64(x, y) {return x.div(y);}
function hs_remInt64(x, y) {return x.modulo(y);}
function hs_plusInt64(x, y) {return x.add(y);}
function hs_minusInt64(x, y) {return x.subtract(y);}
function hs_timesInt64(x, y) {return x.multiply(y);}
function hs_negateInt64(x) {return x.negate();}
function hs_uncheckedIShiftL64(x, bits) {return x.shiftLeft(bits);}
function hs_uncheckedIShiftRA64(x, bits) {return x.shiftRight(bits);}
function hs_uncheckedIShiftRL64(x, bits) {return x.shiftRightUnsigned(bits);}
function hs_intToInt64(x) {return new Long(x, 0);}
function hs_int64ToInt(x) {return x.toInt();}



// Word64
function hs_wordToWord64(x) {
    return I_fromInt(x);
}
function hs_word64ToWord(x) {
    return I_toInt(x);
}
function hs_mkWord64(low, high) {
    return I_fromBits([low, high]);
}

var hs_and64 = I_and;
var hs_or64 = I_or;
var hs_xor64 = I_xor;
var __i64_all_ones = I_fromBits([0xffffffff, 0xffffffff]);
function hs_not64(x) {
    return I_xor(x, __i64_all_ones);
}
var hs_eqWord64 = I_equals;
var hs_neWord64 = I_notEquals;
var hs_ltWord64 = I_lessThan;
var hs_leWord64 = I_lessThanOrEqual;
var hs_gtWord64 = I_greaterThan;
var hs_geWord64 = I_greaterThanOrEqual;
var hs_quotWord64 = I_quot;
var hs_remWord64 = I_rem;
var __w64_max = I_fromBits([0,0,1]);
function hs_uncheckedShiftL64(x, bits) {
    return I_rem(I_shiftLeft(x, bits), __w64_max);
}
var hs_uncheckedShiftRL64 = I_shiftRight;
function hs_int64ToWord64(x) {
    var tmp = I_add(__w64_max, I_fromBits([x.getLowBits(), x.getHighBits()]));
    return I_rem(tmp, __w64_max);
}
function hs_word64ToInt64(x) {
    return Long.fromBits(I_getBits(x, 0), I_getBits(x, 1));
}

// Joseph Myers' MD5 implementation; used under the BSD license.

function md5cycle(x, k) {
    var a = x[0], b = x[1], c = x[2], d = x[3];

    a = ff(a, b, c, d, k[0], 7, -680876936);
    d = ff(d, a, b, c, k[1], 12, -389564586);
    c = ff(c, d, a, b, k[2], 17,  606105819);
    b = ff(b, c, d, a, k[3], 22, -1044525330);
    a = ff(a, b, c, d, k[4], 7, -176418897);
    d = ff(d, a, b, c, k[5], 12,  1200080426);
    c = ff(c, d, a, b, k[6], 17, -1473231341);
    b = ff(b, c, d, a, k[7], 22, -45705983);
    a = ff(a, b, c, d, k[8], 7,  1770035416);
    d = ff(d, a, b, c, k[9], 12, -1958414417);
    c = ff(c, d, a, b, k[10], 17, -42063);
    b = ff(b, c, d, a, k[11], 22, -1990404162);
    a = ff(a, b, c, d, k[12], 7,  1804603682);
    d = ff(d, a, b, c, k[13], 12, -40341101);
    c = ff(c, d, a, b, k[14], 17, -1502002290);
    b = ff(b, c, d, a, k[15], 22,  1236535329);

    a = gg(a, b, c, d, k[1], 5, -165796510);
    d = gg(d, a, b, c, k[6], 9, -1069501632);
    c = gg(c, d, a, b, k[11], 14,  643717713);
    b = gg(b, c, d, a, k[0], 20, -373897302);
    a = gg(a, b, c, d, k[5], 5, -701558691);
    d = gg(d, a, b, c, k[10], 9,  38016083);
    c = gg(c, d, a, b, k[15], 14, -660478335);
    b = gg(b, c, d, a, k[4], 20, -405537848);
    a = gg(a, b, c, d, k[9], 5,  568446438);
    d = gg(d, a, b, c, k[14], 9, -1019803690);
    c = gg(c, d, a, b, k[3], 14, -187363961);
    b = gg(b, c, d, a, k[8], 20,  1163531501);
    a = gg(a, b, c, d, k[13], 5, -1444681467);
    d = gg(d, a, b, c, k[2], 9, -51403784);
    c = gg(c, d, a, b, k[7], 14,  1735328473);
    b = gg(b, c, d, a, k[12], 20, -1926607734);

    a = hh(a, b, c, d, k[5], 4, -378558);
    d = hh(d, a, b, c, k[8], 11, -2022574463);
    c = hh(c, d, a, b, k[11], 16,  1839030562);
    b = hh(b, c, d, a, k[14], 23, -35309556);
    a = hh(a, b, c, d, k[1], 4, -1530992060);
    d = hh(d, a, b, c, k[4], 11,  1272893353);
    c = hh(c, d, a, b, k[7], 16, -155497632);
    b = hh(b, c, d, a, k[10], 23, -1094730640);
    a = hh(a, b, c, d, k[13], 4,  681279174);
    d = hh(d, a, b, c, k[0], 11, -358537222);
    c = hh(c, d, a, b, k[3], 16, -722521979);
    b = hh(b, c, d, a, k[6], 23,  76029189);
    a = hh(a, b, c, d, k[9], 4, -640364487);
    d = hh(d, a, b, c, k[12], 11, -421815835);
    c = hh(c, d, a, b, k[15], 16,  530742520);
    b = hh(b, c, d, a, k[2], 23, -995338651);

    a = ii(a, b, c, d, k[0], 6, -198630844);
    d = ii(d, a, b, c, k[7], 10,  1126891415);
    c = ii(c, d, a, b, k[14], 15, -1416354905);
    b = ii(b, c, d, a, k[5], 21, -57434055);
    a = ii(a, b, c, d, k[12], 6,  1700485571);
    d = ii(d, a, b, c, k[3], 10, -1894986606);
    c = ii(c, d, a, b, k[10], 15, -1051523);
    b = ii(b, c, d, a, k[1], 21, -2054922799);
    a = ii(a, b, c, d, k[8], 6,  1873313359);
    d = ii(d, a, b, c, k[15], 10, -30611744);
    c = ii(c, d, a, b, k[6], 15, -1560198380);
    b = ii(b, c, d, a, k[13], 21,  1309151649);
    a = ii(a, b, c, d, k[4], 6, -145523070);
    d = ii(d, a, b, c, k[11], 10, -1120210379);
    c = ii(c, d, a, b, k[2], 15,  718787259);
    b = ii(b, c, d, a, k[9], 21, -343485551);

    x[0] = add32(a, x[0]);
    x[1] = add32(b, x[1]);
    x[2] = add32(c, x[2]);
    x[3] = add32(d, x[3]);

}

function cmn(q, a, b, x, s, t) {
    a = add32(add32(a, q), add32(x, t));
    return add32((a << s) | (a >>> (32 - s)), b);
}

function ff(a, b, c, d, x, s, t) {
    return cmn((b & c) | ((~b) & d), a, b, x, s, t);
}

function gg(a, b, c, d, x, s, t) {
    return cmn((b & d) | (c & (~d)), a, b, x, s, t);
}

function hh(a, b, c, d, x, s, t) {
    return cmn(b ^ c ^ d, a, b, x, s, t);
}

function ii(a, b, c, d, x, s, t) {
    return cmn(c ^ (b | (~d)), a, b, x, s, t);
}

function md51(s) {
    var n = s.length,
        state = [1732584193, -271733879, -1732584194, 271733878], i;
    for (i=64; i<=s.length; i+=64) {
        md5cycle(state, md5blk(s.substring(i-64, i)));
    }
    s = s.substring(i-64);
    var tail = [0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0];
    for (i=0; i<s.length; i++)
        tail[i>>2] |= s.charCodeAt(i) << ((i%4) << 3);
    tail[i>>2] |= 0x80 << ((i%4) << 3);
    if (i > 55) {
        md5cycle(state, tail);
        for (i=0; i<16; i++) tail[i] = 0;
    }
    tail[14] = n*8;
    md5cycle(state, tail);
    return state;
}
window['md51'] = md51;

function md5blk(s) {
    var md5blks = [], i;
    for (i=0; i<64; i+=4) {
        md5blks[i>>2] = s.charCodeAt(i)
            + (s.charCodeAt(i+1) << 8)
            + (s.charCodeAt(i+2) << 16)
            + (s.charCodeAt(i+3) << 24);
    }
    return md5blks;
}

var hex_chr = '0123456789abcdef'.split('');

function rhex(n)
{
    var s='', j=0;
    for(; j<4; j++)
        s += hex_chr[(n >> (j * 8 + 4)) & 0x0F]
        + hex_chr[(n >> (j * 8)) & 0x0F];
    return s;
}

function hex(x) {
    for (var i=0; i<x.length; i++)
        x[i] = rhex(x[i]);
    return x.join('');
}

function md5(s) {
    return hex(md51(s));
}

function add32(a, b) {
    return (a + b) & 0xFFFFFFFF;
}

// Functions for dealing with arrays.

function newArr(n, x) {
    var arr = [];
    for(; n >= 0; --n) {
        arr.push(x);
    }
    return arr;
}

// Create all views at once; perhaps it's wasteful, but it's better than having
// to check for the right view at each read or write.
function newByteArr(n) {
    // Pad the thing to multiples of 8.
    var padding = 8 - n % 8;
    if(padding < 8) {
        n += padding;
    }
    var arr = {};
    var buffer = new ArrayBuffer(n);
    var views = {};
    views['i8']  = new Int8Array(buffer);
    views['i16'] = new Int16Array(buffer);
    views['i32'] = new Int32Array(buffer);
    views['w8']  = new Uint8Array(buffer);
    views['w16'] = new Uint16Array(buffer);
    views['w32'] = new Uint32Array(buffer);
    views['f32'] = new Float32Array(buffer);
    views['f64'] = new Float64Array(buffer);
    arr['b'] = buffer;
    arr['v'] = views;
    // ByteArray and Addr are the same thing, so keep an offset if we get
    // casted.
    arr['off'] = 0;
    return arr;
}

// An attempt at emulating pointers enough for ByteString and Text to be
// usable without patching the hell out of them.
// The general idea is that Addr# is a byte array with an associated offset.

function plusAddr(addr, off) {
    var newaddr = {};
    newaddr['off'] = addr['off'] + off;
    newaddr['b']   = addr['b'];
    newaddr['v']   = addr['v'];
    return newaddr;
}

function writeOffAddr(type, elemsize, addr, off, x) {
    addr['v'][type][addr.off/elemsize + off] = x;
}

function readOffAddr(type, elemsize, addr, off) {
    return addr['v'][type][addr.off/elemsize + off];
}

// Two addresses are equal if they point to the same buffer and have the same
// offset. For other comparisons, just use the offsets - nobody in their right
// mind would check if one pointer is less than another, completely unrelated,
// pointer and then act on that information anyway.
function addrEq(a, b) {
    if(a == b) {
        return true;
    }
    return a && b && a['b'] == b['b'] && a['off'] == b['off'];
}

function addrLT(a, b) {
    if(a) {
        return b && a['off'] < b['off'];
    } else {
        return (b != 0); 
    }
}

function addrGT(a, b) {
    if(b) {
        return a && a['off'] > b['off'];
    } else {
        return (a != 0);
    }
}

function withChar(f, charCode) {
    return f(String.fromCharCode(charCode)).charCodeAt(0);
}

function u_towlower(charCode) {
    return withChar(function(c) {return c.toLowerCase()}, charCode);
}

function u_towupper(charCode) {
    return withChar(function(c) {return c.toUpperCase()}, charCode);
}

var u_towtitle = u_towupper;

function u_iswupper(charCode) {
    var c = String.fromCharCode(charCode);
    return c == c.toUpperCase() && c != c.toLowerCase();
}

function u_iswlower(charCode) {
    var c = String.fromCharCode(charCode);
    return  c == c.toLowerCase() && c != c.toUpperCase();
}

function u_iswdigit(charCode) {
    return charCode >= 48 && charCode <= 57;
}

function u_iswcntrl(charCode) {
    return charCode <= 0x1f || charCode == 0x7f;
}

function u_iswspace(charCode) {
    var c = String.fromCharCode(charCode);
    return c.replace(/\s/g,'') != c;
}

function u_iswalpha(charCode) {
    var c = String.fromCharCode(charCode);
    return c.replace(__hs_alphare, '') != c;
}

function u_iswalnum(charCode) {
    return u_iswdigit(charCode) || u_iswalpha(charCode);
}

function u_iswprint(charCode) {
    return !u_iswcntrl(charCode);
}

function u_gencat(c) {
    throw 'u_gencat is only supported with --full-unicode.';
}

// Regex that matches any alphabetic character in any language. Horrible thing.
var __hs_alphare = /[\u0041-\u005A\u0061-\u007A\u00AA\u00B5\u00BA\u00C0-\u00D6\u00D8-\u00F6\u00F8-\u02C1\u02C6-\u02D1\u02E0-\u02E4\u02EC\u02EE\u0370-\u0374\u0376\u0377\u037A-\u037D\u0386\u0388-\u038A\u038C\u038E-\u03A1\u03A3-\u03F5\u03F7-\u0481\u048A-\u0527\u0531-\u0556\u0559\u0561-\u0587\u05D0-\u05EA\u05F0-\u05F2\u0620-\u064A\u066E\u066F\u0671-\u06D3\u06D5\u06E5\u06E6\u06EE\u06EF\u06FA-\u06FC\u06FF\u0710\u0712-\u072F\u074D-\u07A5\u07B1\u07CA-\u07EA\u07F4\u07F5\u07FA\u0800-\u0815\u081A\u0824\u0828\u0840-\u0858\u08A0\u08A2-\u08AC\u0904-\u0939\u093D\u0950\u0958-\u0961\u0971-\u0977\u0979-\u097F\u0985-\u098C\u098F\u0990\u0993-\u09A8\u09AA-\u09B0\u09B2\u09B6-\u09B9\u09BD\u09CE\u09DC\u09DD\u09DF-\u09E1\u09F0\u09F1\u0A05-\u0A0A\u0A0F\u0A10\u0A13-\u0A28\u0A2A-\u0A30\u0A32\u0A33\u0A35\u0A36\u0A38\u0A39\u0A59-\u0A5C\u0A5E\u0A72-\u0A74\u0A85-\u0A8D\u0A8F-\u0A91\u0A93-\u0AA8\u0AAA-\u0AB0\u0AB2\u0AB3\u0AB5-\u0AB9\u0ABD\u0AD0\u0AE0\u0AE1\u0B05-\u0B0C\u0B0F\u0B10\u0B13-\u0B28\u0B2A-\u0B30\u0B32\u0B33\u0B35-\u0B39\u0B3D\u0B5C\u0B5D\u0B5F-\u0B61\u0B71\u0B83\u0B85-\u0B8A\u0B8E-\u0B90\u0B92-\u0B95\u0B99\u0B9A\u0B9C\u0B9E\u0B9F\u0BA3\u0BA4\u0BA8-\u0BAA\u0BAE-\u0BB9\u0BD0\u0C05-\u0C0C\u0C0E-\u0C10\u0C12-\u0C28\u0C2A-\u0C33\u0C35-\u0C39\u0C3D\u0C58\u0C59\u0C60\u0C61\u0C85-\u0C8C\u0C8E-\u0C90\u0C92-\u0CA8\u0CAA-\u0CB3\u0CB5-\u0CB9\u0CBD\u0CDE\u0CE0\u0CE1\u0CF1\u0CF2\u0D05-\u0D0C\u0D0E-\u0D10\u0D12-\u0D3A\u0D3D\u0D4E\u0D60\u0D61\u0D7A-\u0D7F\u0D85-\u0D96\u0D9A-\u0DB1\u0DB3-\u0DBB\u0DBD\u0DC0-\u0DC6\u0E01-\u0E30\u0E32\u0E33\u0E40-\u0E46\u0E81\u0E82\u0E84\u0E87\u0E88\u0E8A\u0E8D\u0E94-\u0E97\u0E99-\u0E9F\u0EA1-\u0EA3\u0EA5\u0EA7\u0EAA\u0EAB\u0EAD-\u0EB0\u0EB2\u0EB3\u0EBD\u0EC0-\u0EC4\u0EC6\u0EDC-\u0EDF\u0F00\u0F40-\u0F47\u0F49-\u0F6C\u0F88-\u0F8C\u1000-\u102A\u103F\u1050-\u1055\u105A-\u105D\u1061\u1065\u1066\u106E-\u1070\u1075-\u1081\u108E\u10A0-\u10C5\u10C7\u10CD\u10D0-\u10FA\u10FC-\u1248\u124A-\u124D\u1250-\u1256\u1258\u125A-\u125D\u1260-\u1288\u128A-\u128D\u1290-\u12B0\u12B2-\u12B5\u12B8-\u12BE\u12C0\u12C2-\u12C5\u12C8-\u12D6\u12D8-\u1310\u1312-\u1315\u1318-\u135A\u1380-\u138F\u13A0-\u13F4\u1401-\u166C\u166F-\u167F\u1681-\u169A\u16A0-\u16EA\u1700-\u170C\u170E-\u1711\u1720-\u1731\u1740-\u1751\u1760-\u176C\u176E-\u1770\u1780-\u17B3\u17D7\u17DC\u1820-\u1877\u1880-\u18A8\u18AA\u18B0-\u18F5\u1900-\u191C\u1950-\u196D\u1970-\u1974\u1980-\u19AB\u19C1-\u19C7\u1A00-\u1A16\u1A20-\u1A54\u1AA7\u1B05-\u1B33\u1B45-\u1B4B\u1B83-\u1BA0\u1BAE\u1BAF\u1BBA-\u1BE5\u1C00-\u1C23\u1C4D-\u1C4F\u1C5A-\u1C7D\u1CE9-\u1CEC\u1CEE-\u1CF1\u1CF5\u1CF6\u1D00-\u1DBF\u1E00-\u1F15\u1F18-\u1F1D\u1F20-\u1F45\u1F48-\u1F4D\u1F50-\u1F57\u1F59\u1F5B\u1F5D\u1F5F-\u1F7D\u1F80-\u1FB4\u1FB6-\u1FBC\u1FBE\u1FC2-\u1FC4\u1FC6-\u1FCC\u1FD0-\u1FD3\u1FD6-\u1FDB\u1FE0-\u1FEC\u1FF2-\u1FF4\u1FF6-\u1FFC\u2071\u207F\u2090-\u209C\u2102\u2107\u210A-\u2113\u2115\u2119-\u211D\u2124\u2126\u2128\u212A-\u212D\u212F-\u2139\u213C-\u213F\u2145-\u2149\u214E\u2183\u2184\u2C00-\u2C2E\u2C30-\u2C5E\u2C60-\u2CE4\u2CEB-\u2CEE\u2CF2\u2CF3\u2D00-\u2D25\u2D27\u2D2D\u2D30-\u2D67\u2D6F\u2D80-\u2D96\u2DA0-\u2DA6\u2DA8-\u2DAE\u2DB0-\u2DB6\u2DB8-\u2DBE\u2DC0-\u2DC6\u2DC8-\u2DCE\u2DD0-\u2DD6\u2DD8-\u2DDE\u2E2F\u3005\u3006\u3031-\u3035\u303B\u303C\u3041-\u3096\u309D-\u309F\u30A1-\u30FA\u30FC-\u30FF\u3105-\u312D\u3131-\u318E\u31A0-\u31BA\u31F0-\u31FF\u3400-\u4DB5\u4E00-\u9FCC\uA000-\uA48C\uA4D0-\uA4FD\uA500-\uA60C\uA610-\uA61F\uA62A\uA62B\uA640-\uA66E\uA67F-\uA697\uA6A0-\uA6E5\uA717-\uA71F\uA722-\uA788\uA78B-\uA78E\uA790-\uA793\uA7A0-\uA7AA\uA7F8-\uA801\uA803-\uA805\uA807-\uA80A\uA80C-\uA822\uA840-\uA873\uA882-\uA8B3\uA8F2-\uA8F7\uA8FB\uA90A-\uA925\uA930-\uA946\uA960-\uA97C\uA984-\uA9B2\uA9CF\uAA00-\uAA28\uAA40-\uAA42\uAA44-\uAA4B\uAA60-\uAA76\uAA7A\uAA80-\uAAAF\uAAB1\uAAB5\uAAB6\uAAB9-\uAABD\uAAC0\uAAC2\uAADB-\uAADD\uAAE0-\uAAEA\uAAF2-\uAAF4\uAB01-\uAB06\uAB09-\uAB0E\uAB11-\uAB16\uAB20-\uAB26\uAB28-\uAB2E\uABC0-\uABE2\uAC00-\uD7A3\uD7B0-\uD7C6\uD7CB-\uD7FB\uF900-\uFA6D\uFA70-\uFAD9\uFB00-\uFB06\uFB13-\uFB17\uFB1D\uFB1F-\uFB28\uFB2A-\uFB36\uFB38-\uFB3C\uFB3E\uFB40\uFB41\uFB43\uFB44\uFB46-\uFBB1\uFBD3-\uFD3D\uFD50-\uFD8F\uFD92-\uFDC7\uFDF0-\uFDFB\uFE70-\uFE74\uFE76-\uFEFC\uFF21-\uFF3A\uFF41-\uFF5A\uFF66-\uFFBE\uFFC2-\uFFC7\uFFCA-\uFFCF\uFFD2-\uFFD7\uFFDA-\uFFDC]/g;

// 2D Canvas drawing primitives.
function jsHasCtx2D(elem) {return !!elem.getContext;}
function jsGetCtx2D(elem) {return elem.getContext('2d');}
function jsBeginPath(ctx) {ctx.beginPath();}
function jsMoveTo(ctx, x, y) {ctx.moveTo(x, y);}
function jsLineTo(ctx, x, y) {ctx.lineTo(x, y);}
function jsStroke(ctx) {ctx.stroke();}
function jsFill(ctx) {ctx.fill();}
function jsRotate(ctx, radians) {ctx.rotate(radians);}
function jsTranslate(ctx, x, y) {ctx.translate(x, y);}
function jsScale(ctx, x, y) {ctx.scale(x, y);}
function jsPushState(ctx) {ctx.save();}
function jsPopState(ctx) {ctx.restore();}
function jsResetCanvas(el) {el.width = el.width;}
function jsDrawImage(ctx, img, x, y) {ctx.drawImage(img, x, y);}
function jsDrawImageClipped(ctx, img, x, y, cx, cy, cw, ch) {
    ctx.drawImage(img, cx, cy, cw, ch, x, y, cw, ch);
}
function jsDrawText(ctx, str, x, y) {ctx.fillText(str, x, y);}
function jsClip(ctx) {ctx.clip();}
function jsArc(ctx, x, y, radius, fromAngle, toAngle) {
    ctx.arc(x, y, radius, fromAngle, toAngle);
}
function jsCanvasToDataURL(el) {return el.toDataURL('image/png');}

// Simulate handles.
// When implementing new handles, remember that passed strings may be thunks,
// and so need to be evaluated before use.

function jsNewHandle(init, read, write, flush, close, seek, tell) {
    var h = {
        read: read || function() {},
        write: write || function() {},
        seek: seek || function() {},
        tell: tell || function() {},
        close: close || function() {},
        flush: flush || function() {}
    };
    init.call(h);
    return h;
}

function jsReadHandle(h, len) {return h.read(len);}
function jsWriteHandle(h, str) {return h.write(str);}
function jsFlushHandle(h) {return h.flush();}
function jsCloseHandle(h) {return h.close();}

function jsMkConWriter(op) {
    return function(str) {
        str = E(str);
        var lines = (this.buf + str).split('\n');
        for(var i = 0; i < lines.length-1; ++i) {
            op.call(console, lines[i]);
        }
        this.buf = lines[lines.length-1];
    }
}

function jsMkStdout() {
    return jsNewHandle(
        function() {this.buf = '';},
        function(_) {return '';},
        jsMkConWriter(console.log),
        function() {console.log(this.buf); this.buf = '';}
    );
}

function jsMkStderr() {
    return jsNewHandle(
        function() {this.buf = '';},
        function(_) {return '';},
        jsMkConWriter(console.warn),
        function() {console.warn(this.buf); this.buf = '';}
    );
}

function jsMkStdin() {
    return jsNewHandle(
        function() {this.buf = '';},
        function(len) {
            while(this.buf.length < len) {
                this.buf += prompt('[stdin]') + '\n';
            }
            var ret = this.buf.substr(0, len);
            this.buf = this.buf.substr(len);
            return ret;
        }
    );
}

// "Weak Pointers". Mostly useless implementation since
// JS does its own GC.

function mkWeak(key, val, fin) {
    fin = !fin? function() {}: fin;
    return {key: key, val: val, fin: fin};
}

function derefWeak(w) {
    return [0, 1, E(w).val];
}

function finalizeWeak(w) {
    return [0, B(A(E(w).fin, [0]))];
}

var _0=new T(function(){return B(unCStr("Prelude.(!!): negative index\n"));}),_1=new T(function(){return B(err(_0));}),_2=new T(function(){return B(unCStr("Prelude.(!!): index too large\n"));}),_3=new T(function(){return B(err(_2));}),_4=function(_5,_6){while(1){var _7=E(_5);if(!_7[0]){return E(_3);}else{var _8=E(_6);if(!_8){return E(_7[1]);}else{_5=_7[2];_6=_8-1|0;continue;}}}},_9=[8,_],_a=function(_b){var _c=String(_b),_d=_c;return new F(function(){return fromJSStr(_d);});},_e=[0,2147483562],_f=[0,1],_g=[0,_f,_e],_h=function(_i){return E(_g);},_j=function(_k,_l){return [0,E(_k)[1],E(_l)[1]];},_m=function(_n,_o){var _p=quot(_o,52774),_q=(imul(40692,_o-(imul(_p,52774)|0)|0)|0)-(imul(_p,3791)|0)|0,_r=new T(function(){if(_q>=0){var _s=[0,_q];}else{var _s=[0,_q+2147483399|0];}var _t=_s;return _t;}),_u=quot(_n,53668),_v=(imul(40014,_n-(imul(_u,53668)|0)|0)|0)-(imul(_u,12211)|0)|0,_w=new T(function(){if(_v>=0){var _x=[0,_v];}else{var _x=[0,_v+2147483563|0];}var _y=_x;return _y;});return [0,new T(function(){var _z=E(_w)[1]-E(_r)[1]|0;if(_z>=1){var _A=[0,_z];}else{var _A=[0,_z+2147483562|0];}var _B=_A,_C=_B,_D=_C,_E=_D;return _E;}),new T(function(){return B(_j(_w,_r));})];},_F=function(_G){var _H=E(_G),_I=B(_m(_H[1],_H[2]));return [0,_I[1],_I[2]];},_J=function(_K,_L){var _M=new T(function(){return E(B(_m(_K,_L))[2]);});return [0,new T(function(){var _N=E(_K);if(_N==2147483562){var _O=[0,1,E(_M)[2]];}else{var _O=[0,_N+1|0,E(_M)[2]];}return _O;}),new T(function(){var _P=E(_M)[1],_Q=E(_L);if(_Q==1){var _R=[0,_P,2147483398];}else{var _R=[0,_P,_Q-1|0];}var _S=_R;return _S;})];},_T=function(_U){var _V=E(_U),_W=B(_J(_V[1],_V[2]));return [0,_W[1],_W[2]];},_X=[0,_F,_h,_T],_Y=function(_Z,_10,_){var _11=jsCreateTextNode(toJSStr(E(_Z))),_12=_11,_13=jsAppendChild(_12,E(_10)[1]);return [0,_12];},_14=function(_15,_16,_){var _17=E(_15);if(!_17[0]){return _16;}else{var _18=B(A(_17[1],[_16,_])),_19=_18,_1a=B(_14(_17[2],_16,_)),_1b=_1a;return _16;}},_1c=new T(function(){return B(unCStr("\u30c9\u30ed\u30fc"));}),_1d=new T(function(){return B(unCStr("\u3042\u306a\u305f\u306e\u52dd\u3061\u3067\u3059!"));}),_1e=new T(function(){return B(unCStr("\u30b3\u30f3\u30d4\u30e5\u30fc\u30bf\u304c\u52dd\u3061!"));}),_1f=new T(function(){return B(unCStr("\u5f15\u304d\u5206\u3051!!!!!!!!!!!!!"));}),_1g=new T(function(){return B(unCStr("\u624b\u756a\u3092\u4ea4\u4ee3"));}),_1h=new T(function(){return B(unCStr("\u53ec\u559a\u3059\u308b\u4f4d\u7f6e\u3092\u9078\u629e"));}),_1i=new T(function(){return B(unCStr("\u751f\u8d04\u3092\u9078\u629e"));}),_1j=new T(function(){return B(unCStr("\u79fb\u52d5\u3059\u308b\u4f4d\u7f6e\u3092\u9078\u629e"));}),_1k=new T(function(){return B(unCStr("\u884c\u52d5\u3092\u9078\u629e"));}),_1l=new T(function(){return B(unCStr("your-card"));}),_1m=new T(function(){return B(unCStr("computers-card"));}),_1n=new T(function(){return B(unCStr("class"));}),_1o=[0],_1p=new T(function(){return B(unCStr("td"));}),_1q=function(_1r,_1s,_1t,_){var _1u=jsCreateElem(toJSStr(E(_1p))),_1v=_1u,_1w=jsAppendChild(_1v,E(_1t)[1]),_1x=[0,_1v],_1y=B(A(_1r,[_1s,_1x,_])),_1z=_1y;return _1x;},_1A=function(_1B,_){return new F(function(){return _1q(_Y,_1o,_1B,_);});},_1C=[0,75],_1D=[1,_1C,_1o],_1E=[0,81],_1F=[1,_1E,_1o],_1G=[0,74],_1H=[1,_1G,_1o],_1I=[0,65],_1J=[1,_1I,_1o],_1K=function(_1L,_1M){var _1N=E(_1L);return _1N[0]==0?E(_1M):[1,_1N[1],new T(function(){return B(_1K(_1N[2],_1M));})];},_1O=function(_1P,_1Q){var _1R=jsShowI(_1P),_1S=_1R;return new F(function(){return _1K(fromJSStr(_1S),_1Q);});},_1T=[0,41],_1U=[0,40],_1V=function(_1W,_1X,_1Y){if(_1X>=0){return new F(function(){return _1O(_1X,_1Y);});}else{return _1W<=6?B(_1O(_1X,_1Y)):[1,_1U,new T(function(){var _1Z=jsShowI(_1X),_20=_1Z;return B(_1K(fromJSStr(_20),[1,_1T,_1Y]));})];}},_21=function(_22){var _23=E(_22);switch(_23){case 1:return E(_1J);case 11:return E(_1H);case 12:return E(_1F);case 13:return E(_1D);default:return new F(function(){return _1V(0,_23,_1o);});}},_24=0,_25=function(_26,_27,_28,_29){return new F(function(){return A(_26,[function(_){var _2a=jsSetAttr(E(_27)[1],toJSStr(E(_28)),toJSStr(E(_29)));return _24;}]);});},_2b=function(_2c){return E(_2c);},_2d=function(_2e){var _2f=E(_2e);if(!_2f[0]){return E(_1A);}else{var _2g=E(_2f[1]),_2h=_2g[2];return !E(_2g[1])?function(_2i,_){var _2j=B(_1q(_Y,new T(function(){return B(_21(E(_2h)[1]));}),_2i,_)),_2k=_2j,_2l=B(A(_25,[_2b,_2k,_1n,_1m,_])),_2m=_2l;return _2k;}:function(_2n,_){var _2o=B(_1q(_Y,new T(function(){return B(_21(E(_2h)[1]));}),_2n,_)),_2p=_2o,_2q=B(A(_25,[_2b,_2p,_1n,_1l,_])),_2r=_2q;return _2p;};}},_2s=function(_2t,_2u){var _2v=E(_2u);return _2v[0]==0?[0]:[1,new T(function(){return B(A(_2t,[_2v[1]]));}),new T(function(){return B(_2s(_2t,_2v[2]));})];},_2w=new T(function(){return B(unCStr("tr"));}),_2x=function(_2y,_2z,_2A,_){var _2B=jsCreateElem(toJSStr(E(_2w))),_2C=_2B,_2D=jsAppendChild(_2C,E(_2A)[1]),_2E=[0,_2C],_2F=B(A(_2y,[_2z,_2E,_])),_2G=_2F;return _2E;},_2H=function(_2I){return E(_2I);},_2J=function(_2K){return function(_2L,_2M){return new F(function(){return _2x(_2H,function(_2N,_){return new F(function(){return _14(new T(function(){return B(_2s(_2d,_2K));}),_2N,_);});},_2L,_2M);});};},_2O=new T(function(){return B(unCStr("table#field"));}),_2P=new T(function(){return [0,"arr2lst"];}),_2Q=function(_2R){var _2S=B(A(_2R,[_])),_2T=_2S;return E(_2T);},_2U=function(_2V){return new F(function(){return _2Q(function(_){var _=0;return new F(function(){return eval(_2V);});});});},_2W=function(_2X,_2Y){return new F(function(){return _2Q(function(_){var _=0;return new F(function(){return A(_2U,[E(_2P)[1],E(_2X),E(_2Y),_]);});});});},_2Z=new T(function(){return B(_2U("(function(sel){return document.querySelectorAll(sel);})"));}),_30=function(_31,_32,_33,_){var _34=B(A(_2Z,[E(toJSStr(E(_31))),_])),_35=_34,_36=function(_37,_){var _38=E(_37);if(!_38[0]){return _1o;}else{var _39=B(A(_32,[[0,_38[1]],_])),_3a=_39,_3b=B(_36(_38[2],_)),_3c=_3b;return [1,_3a,_3c];}},_3d=B(_36(B(_2W(_35,0)),_)),_3e=_3d;return _33;},_3f=function(_3g){return function(_2L,_2M){return new F(function(){return _30(_2O,function(_3h,_){var _3i=E(_3h),_3j=jsClearChildren(_3i[1]),_3k=B(_14(new T(function(){return B(_2s(_2J,_3g));}),_3i,_)),_3l=_3k;return _3i;},_2L,_2M);});};},_3m=new T(function(){return B(unCStr(" .deck"));}),_3n=new T(function(){return B(unCStr("\u306e\u6b8b\u308a\u5c71\u672d: "));}),_3o=[0,35],_3p=new T(function(){return B(unCStr(" .hand"));}),_3q=function(_3r,_3s){while(1){var _3t=E(_3r);if(!_3t[0]){return E(_3s);}else{_3r=_3t[2];var _3u=_3s+1|0;_3s=_3u;continue;}}},_3v=new T(function(){return B(unCStr("li"));}),_3w=function(_3x,_3y,_3z,_){var _3A=jsCreateElem(toJSStr(E(_3v))),_3B=_3A,_3C=jsAppendChild(_3B,E(_3z)[1]),_3D=[0,_3B],_3E=B(A(_3x,[_3y,_3D,_])),_3F=_3E;return _3D;},_3G=function(_3H){return function(_3I,_){var _3J=B(_30([1,_3o,new T(function(){return B(_1K(E(_3H)[4],_3m));})],function(_3K,_){var _3L=E(_3K),_3M=jsClearChildren(_3L[1]),_3N=B(_Y(new T(function(){var _3O=E(_3H);return B(_1K(_3O[3],new T(function(){return B(_1K(_3n,new T(function(){return B(_1V(0,B(_3q(_3O[2],0)),_1o));},1)));},1)));}),_3L,_)),_3P=_3N;return _3L;},_3I,_)),_3Q=_3J,_3R=B(_30([1,_3o,new T(function(){return B(_1K(E(_3H)[4],_3p));})],function(_3S,_){var _3T=E(_3S),_3U=jsClearChildren(_3T[1]),_3V=B(_14(new T(function(){var _3W=E(_3H);return B(_2s(function(_3X){return function(_2L,_2M){return new F(function(){return _3w(_Y,new T(function(){return B(A(_3W[5],[_3X]));}),_2L,_2M);});};},_3W[1]));}),_3T,_)),_3Y=_3V;return _3T;},_3I,_)),_3Z=_3R;return _3I;};},_40=function(_41,_42){while(1){var _43=E(_42);if(!_43[0]){return E(_41);}else{var _44=_41+E(_43[1])[1]|0;_42=_43[2];_41=_44;continue;}}},_45=new T(function(){return B(unCStr("selectable-hand"));}),_46=function(_47,_){while(1){var _48=E(_47);if(!_48[0]){return _24;}else{var _49=B(A(_25,[_2b,_48[1],_1n,_45,_])),_4a=_49;_47=_48[2];continue;}}},_4b=[0,2],_4c=[0,1],_4d=function(_4e){return E(_4e)[0]==0?E(_4b):E(_4c);},_4f=function(_4g,_4h,_4i,_4j){var _4k=E(_4i);if(!_4k[0]){return E(_4h);}else{var _4l=E(_4j);if(!_4l[0]){return E(_4h);}else{return new F(function(){return A(_4g,[_4k[1],_4l[1],new T(function(){return B(_4f(_4g,_4h,_4k[2],_4l[2]));})]);});}}},_4m=function(_4n){return E(_4n)[0]==0?true:false;},_4o=function(_4p,_4q){while(1){var _4r=E(_4q);if(!_4r[0]){return E(_4p);}else{_4p=_4r[1];_4q=_4r[2];continue;}}},_4s=new T(function(){return B(unCStr(": empty list"));}),_4t=new T(function(){return B(unCStr("Prelude."));}),_4u=function(_4v){return new F(function(){return err(B(_1K(_4t,new T(function(){return B(_1K(_4v,_4s));},1))));});},_4w=new T(function(){return B(unCStr("last"));}),_4x=new T(function(){return B(_4u(_4w));}),_4y=new T(function(){return B(unCStr("#status"));}),_4z=new T(function(){return B(unCStr("#yours ol.hand li"));}),_4A=function(_4B,_4C,_4D,_){if(!E(_4C)){return new F(function(){return A(_4D,[_]);});}else{var _4E=B(A(_25,[_2b,_4B,_1n,_45,_])),_4F=_4E;return new F(function(){return A(_4D,[_]);});}},_4G=function(_){return _24;},_4H=new T(function(){return B(unCStr("summonable-zone"));}),_4I=function(_4J,_4K,_4L,_){if(!E(_4K)){return new F(function(){return A(_4L,[_]);});}else{var _4M=B(A(_25,[_2b,_4J,_1n,_4H,_])),_4N=_4M;return new F(function(){return A(_4L,[_]);});}},_4O=new T(function(){return B(unCStr("sacrifice"));}),_4P=new T(function(){return B(unCStr("id"));}),_4Q=new T(function(){return B(unCStr("obj-of-summon"));}),_4R=new T(function(){return B(unCStr("#field tr"));}),_4S=new T(function(){return B(unCStr("td"));}),_4T=new T(function(){return B(unCStr("\u306e\u756a\u3067\u3059\u3001"));}),_4U=function(_4V,_){return _4V;},_4W=function(_4X){return function(_2L,_2M){return new F(function(){return _14([1,new T(function(){return B(_3f(new T(function(){return E(E(_4X)[4]);},1)));}),[1,function(_1B,_){return new F(function(){return _30(_4y,function(_4Y,_){var _4Z=E(_4Y),_50=jsClearChildren(_4Z[1]),_51=B(A(new T(function(){var _52=E(_4X),_53=_52[1],_54=E(_52[3]);if(_54[0]==6){var _55=function(_2L,_2M){return new F(function(){return _Y(new T(function(){var _56=E(_54[1]);return _56[0]==0?E(_1f):!E(_56[1])?E(_1e):E(_1d);}),_2L,_2M);});};}else{var _55=function(_2L,_2M){return new F(function(){return _Y(new T(function(){return B(unAppCStr("-- ",new T(function(){var _57=new T(function(){return B(_1K(_4T,new T(function(){switch(E(_54)[0]){case 0:var _58=E(_1c);break;case 1:var _58=E(_1k);break;case 2:var _58=E(_1j);break;case 3:var _58=E(_1i);break;case 4:var _58=E(_1h);break;default:var _58=E(_1g);}return _58;},1)));},1);if(!E(_52[2])){var _59=B(_1K(E(E(_53)[2])[3],_57));}else{var _59=B(_1K(E(E(_53)[1])[3],_57));}return _59;})));}),_2L,_2M);});};}var _5a=_55;return _5a;}),[_4Z,_])),_5b=_51;return _4Z;},_1B,_);});},[1,function(_5c,_){var _5d=E(new T(function(){var _5e=E(E(_4X)[1]);return [0,new T(function(){return B(_3G(_5e[1]));}),new T(function(){return B(_3G(_5e[2]));})];})),_5f=B(A(_5d[1],[_5c,_])),_5g=_5f,_5h=B(A(_5d[2],[_5c,_])),_5i=_5h;return _5c;},[1,new T(function(){var _5j=E(_4X),_5k=E(_5j[3]);switch(_5k[0]){case 1:var _5l=function(_5m,_){var _5n=E(_5m),_5o=jsQuerySelectorAll(_5n[1],toJSStr(E(_4z))),_5p=_5o,_5q=B(A(_4f,[_4A,_4G,_5p,new T(function(){var _5r=E(E(_5j[1])[1])[1],_5s=new T(function(){return B(_2s(_4d,_5r));});return B(_2s(function(_5t){var _5u=E(_5t);return _5u[0]==0?false:E(_5u[1])[1]<=10?0<=B(_40(-1,_5s)):2<=B(_40(-1,_5s));},_5r));}),_])),_5v=_5q;return _5n;};break;case 3:var _5l=function(_5w,_){var _5x=E(_5w),_5y=jsQuerySelectorAll(_5x[1],toJSStr(E(_4z))),_5z=_5y,_5A=_5z,_5B=E(_5k[2]);if(!_5B[0]){var _5C=B(_46(_5A,_)),_5D=_5C;return _5x;}else{var _5E=E(_5B[1])[1];if(_5E>=0){var _5F=E(_1n),_5G=E(_4O),_5H=jsSetAttr(B(_4(_5A,_5E))[1],toJSStr(_5F),toJSStr(_5G)),_5I=B((function(_5J,_){while(1){var _5K=E(_5J);if(!_5K[0]){return _24;}else{var _5L=E(_5K[1])[1];if(_5L>=0){var _5M=jsSetAttr(B(_4(_5A,_5L))[1],toJSStr(_5F),toJSStr(_5G));_5J=_5K[2];continue;}else{return E(_1);}}}})(_5B[2],_)),_5N=_5I,_5O=B(_46(_5A,_)),_5P=_5O;return _5x;}else{return E(_1);}}};break;case 4:var _5l=function(_5Q,_){var _5R=E(_5Q),_5S=_5R[1],_5T=jsQuerySelectorAll(_5S,toJSStr(E(_4z))),_5U=_5T,_5V=E(_5k[1])[1];if(_5V>=0){var _5W=jsSetAttr(B(_4(_5U,_5V))[1],toJSStr(E(_4P)),toJSStr(E(_4Q))),_5X=jsQuerySelectorAll(_5S,toJSStr(E(_4R))),_5Y=_5X,_5Z=E(_5Y);if(!_5Z[0]){return E(_4x);}else{var _60=jsQuerySelectorAll(B(_4o(_5Z[1],_5Z[2]))[1],toJSStr(E(_4S))),_61=_60,_62=B(A(_4f,[_4I,_4G,_61,new T(function(){var _63=E(_5j[4]);if(!_63[0]){var _64=E(_4x);}else{var _64=B(_2s(_4m,B(_4o(_63[1],_63[2]))));}return _64;}),_])),_65=_62;return _5R;}}else{return E(_1);}};break;default:var _5l=E(_4U);}var _66=_5l;return _66;}),_1o]]]],_2L,_2M);});};},_67=new T(function(){return B(unCStr("Prelude.Enum.succ{Int}: tried to take `succ\' of maxBound"));}),_68=new T(function(){return B(err(_67));}),_69=function(_6a){var _6b=E(E(_6a)[1]);return _6b==2147483647?E(_68):[0,_6b+1|0];},_6c=[0],_6d=new T(function(){return B(_2U("(function(x) {return x === null})"));}),_6e=new T(function(){return B(_2U("(function(node) {return node.nodeType === 1})"));}),_6f=new T(function(){return [0,"(function(e){ return e.previousSibling })"];}),_6g=function(_6h,_6i,_){while(1){var _6j=(function(_6k,_6l,_){var _6m=E(_6f)[1],_6n=B(A(_2U,[_6m,E(_6l),_])),_6o=_6n,_6p=E(_6o),_6q=B(A(_6d,[_6p,_])),_6r=_6q;if(_6r<=0){var _6s=B(A(_6e,[_6p,_])),_6t=_6s;if(_6t<=0){var _6u=B((function(_6v,_){while(1){var _6w=B(A(_2U,[_6m,E(_6v),_])),_6x=_6w,_6y=E(_6x),_6z=B(A(_6d,[_6y,_])),_6A=_6z;if(_6A<=0){var _6B=B(A(_6e,[_6y,_])),_6C=_6B;if(_6C<=0){_6v=_6x;continue;}else{return [1,[0,_6y]];}}else{return _6c;}}})(_6o,_)),_6D=_6u,_6E=E(_6D);if(!_6E[0]){return _6k;}else{_6h=new T(function(){return B(_69(_6k));});_6i=E(_6E[1])[1];return null;}}else{_6h=new T(function(){return B(_69(_6k));});_6i=_6p;return null;}}else{return _6k;}})(_6h,_6i,_);if(_6j!=null){return _6j;}}},_6F=false,_6G=[1],_6H=true,_6I=[6,_6c],_6J=new T(function(){return B(unCStr("tail"));}),_6K=new T(function(){return B(_4u(_6J));}),_6L=function(_6M,_6N,_6O,_6P){var _6Q=function(_6R){var _6S=new T(function(){return !E(_6O)?[0,_6M,new T(function(){var _6T=E(_6N);return [0,[1,_6R,_6T[1]],_6T[2],_6T[3],_6T[4],_6T[5]];})]:[0,new T(function(){var _6U=E(_6M);return [0,[1,_6R,_6U[1]],_6U[2],_6U[3],_6U[4],_6U[5]];}),_6N];},1);return [0,new T(function(){if(!E(_6O)){var _6V=E(_6S),_6W=[0,_6V[1],new T(function(){var _6X=E(_6V[2]);return [0,_6X[1],new T(function(){var _6Y=E(_6X[2]);return _6Y[0]==0?E(_6K):E(_6Y[2]);}),_6X[3],_6X[4],_6X[5]];})];}else{var _6Z=E(_6S),_6W=[0,new T(function(){var _70=E(_6Z[1]);return [0,_70[1],new T(function(){var _71=E(_70[2]);return _71[0]==0?E(_6K):E(_71[2]);}),_70[3],_70[4],_70[5]];}),_6Z[2]];}return _6W;}),_6O,_6G,_6P];};if(!E(_6O)){var _72=E(_6N),_73=E(_72[2]);return _73[0]==0?[0,[0,_6M,_72],_6F,_6I,_6P]:B(_6Q(_73[1]));}else{var _74=E(_6M),_75=E(_74[2]);return _75[0]==0?[0,[0,_74,_6N],_6H,_6I,_6P]:B(_6Q(_75[1]));}},_76=[0,119],_77=[1,_76,_1o],_78=function(_79){return new F(function(){return _21(E(_79)[1]);});},_7a=function(_7b){var _7c=E(_7b);return _7c[0]==0?E(_77):B(_78(_7c[1]));},_7d=[0],_7e=function(_7f,_7g){while(1){var _7h=E(_7f);if(!_7h){return E(_7g);}else{var _7i=E(_7g);if(!_7i[0]){return [0];}else{_7f=_7h-1|0;_7g=_7i[2];continue;}}}},_7j=function(_7k){return E(_7k);},_7l=function(_7m){return [1,[1,[0,_7m]],new T(function(){var _7n=E(_7m);if(_7n==13){var _7o=[0];}else{var _7o=B(_7l(_7n+1|0));}return _7o;})];},_7p=new T(function(){return B(_7l(1));}),_7q=[1,_6c,_7p],_7r=new T(function(){return B(_2s(_7j,_7q));}),_7s=function(_7t){return _7t>1?[0,_7r,new T(function(){var _7u=B(_7s(_7t-1|0));return [1,_7u[1],_7u[2]];})]:[0,_7r,_1o];},_7v=function(_7w){var _7x=E(_7w);if(!_7x[0]){return [0];}else{return new F(function(){return _1K(_7x[1],new T(function(){return B(_7v(_7x[2]));},1));});}},_7y=new T(function(){var _7z=B(_7s(4));return B(_7v([1,_7z[1],_7z[2]]));}),_7A=new T(function(){return [0,B(_3q(_7y,0))];}),_7B=function(_7C){return _7C>1?[0,_6c,new T(function(){var _7D=B(_7B(_7C-1|0));return [1,_7D[1],_7D[2]];})]:[0,_6c,_1o];},_7E=new T(function(){var _7F=B(_7B(3));return [1,_7F[1],_7F[2]];}),_7G=function(_7H){return _7H>1?[0,_7E,new T(function(){var _7I=B(_7G(_7H-1|0));return [1,_7I[1],_7I[2]];})]:[0,_7E,_1o];},_7J=new T(function(){var _7K=B(_7G(5));return [1,_7K[1],_7K[2]];}),_7L=[0,63],_7M=[1,_7L,_1o],_7N=function(_7O){return E(_7M);},_7P=new T(function(){return B(unCStr("computers"));}),_7Q=new T(function(){return B(unCStr("\u30b3\u30f3\u30d4\u30e5\u30fc\u30bf"));}),_7R=new T(function(){return B(unCStr("yours"));}),_7S=new T(function(){return B(unCStr("\u3042\u306a\u305f"));}),_7T=function(_7U){return [0,E(E(_7U))];},_7V=function(_7W){var _7X=E(_7W);if(!_7X[0]){return [0,_1o,_1o];}else{var _7Y=E(_7X[1]),_7Z=new T(function(){var _80=B(_7V(_7X[2]));return [0,_80[1],_80[2]];});return [0,[1,_7Y[1],new T(function(){return E(E(_7Z)[1]);})],[1,_7Y[2],new T(function(){return E(E(_7Z)[2]);})]];}},_81=function(_82,_83){return [0,imul(E(_82)[1],E(_83)[1])|0];},_84=function(_85,_86){return [0,E(_85)[1]+E(_86)[1]|0];},_87=function(_88,_89){return [0,E(_88)[1]-E(_89)[1]|0];},_8a=function(_8b){var _8c=E(_8b),_8d=_8c[1];return _8d<0?[0, -_8d]:E(_8c);},_8e=function(_8f){var _8g=E(_8f);return _8g[0]==0?E(_8g[1]):I_toInt(_8g[1]);},_8h=function(_8i){return [0,B(_8e(_8i))];},_8j=function(_8k){return [0, -E(_8k)[1]];},_8l=[0,-1],_8m=[0,0],_8n=[0,1],_8o=function(_8p){var _8q=E(_8p)[1];return _8q>=0?E(_8q)==0?E(_8m):E(_8n):E(_8l);},_8r=[0,_84,_81,_87,_8j,_8a,_8o,_8h],_8s=[0,1],_8t=new T(function(){return B(unCStr("base"));}),_8u=new T(function(){return B(unCStr("GHC.Exception"));}),_8v=new T(function(){return B(unCStr("ArithException"));}),_8w=[0,I_fromBits([4194982440,719304104]),I_fromBits([3110813675,1843557400]),_8t,_8u,_8v],_8x=[0,I_fromBits([4194982440,719304104]),I_fromBits([3110813675,1843557400]),_8w,_1o],_8y=function(_8z){return E(_8x);},_8A=function(_8B){return E(E(_8B)[1]);},_8C=function(_8D,_8E,_8F){var _8G=B(A(_8D,[_])),_8H=B(A(_8E,[_])),_8I=hs_eqWord64(_8G[1],_8H[1]),_8J=_8I;if(!E(_8J)){return [0];}else{var _8K=hs_eqWord64(_8G[2],_8H[2]),_8L=_8K;return E(_8L)==0?[0]:[1,_8F];}},_8M=function(_8N){var _8O=E(_8N);return new F(function(){return _8C(B(_8A(_8O[1])),_8y,_8O[2]);});},_8P=new T(function(){return B(unCStr("arithmetic underflow"));}),_8Q=new T(function(){return B(unCStr("arithmetic overflow"));}),_8R=new T(function(){return B(unCStr("Ratio has zero denominator"));}),_8S=new T(function(){return B(unCStr("denormal"));}),_8T=new T(function(){return B(unCStr("divide by zero"));}),_8U=new T(function(){return B(unCStr("loss of precision"));}),_8V=function(_8W){switch(E(_8W)){case 0:return E(_8Q);case 1:return E(_8P);case 2:return E(_8U);case 3:return E(_8T);case 4:return E(_8S);default:return E(_8R);}},_8X=function(_8Y){return new F(function(){return _1K(_8P,_8Y);});},_8Z=function(_8Y){return new F(function(){return _1K(_8Q,_8Y);});},_90=function(_8Y){return new F(function(){return _1K(_8R,_8Y);});},_91=function(_8Y){return new F(function(){return _1K(_8S,_8Y);});},_92=function(_8Y){return new F(function(){return _1K(_8T,_8Y);});},_93=function(_8Y){return new F(function(){return _1K(_8U,_8Y);});},_94=function(_95){switch(E(_95)){case 0:return E(_8Z);case 1:return E(_8X);case 2:return E(_93);case 3:return E(_92);case 4:return E(_91);default:return E(_90);}},_96=[0,44],_97=[0,93],_98=[0,91],_99=function(_9a,_9b,_9c){var _9d=E(_9b);return _9d[0]==0?B(unAppCStr("[]",_9c)):[1,_98,new T(function(){return B(A(_9a,[_9d[1],new T(function(){var _9e=function(_9f){var _9g=E(_9f);return _9g[0]==0?E([1,_97,_9c]):[1,_96,new T(function(){return B(A(_9a,[_9g[1],new T(function(){return B(_9e(_9g[2]));})]));})];};return B(_9e(_9d[2]));})]));})];},_9h=function(_9i,_9j){return new F(function(){return _99(_94,_9i,_9j);});},_9k=function(_9l,_9m){switch(E(_9m)){case 0:return E(_8Z);case 1:return E(_8X);case 2:return E(_93);case 3:return E(_92);case 4:return E(_91);default:return E(_90);}},_9n=[0,_9k,_8V,_9h],_9o=new T(function(){return [0,_8y,_9n,_9p,_8M];}),_9p=function(_8Y){return [0,_9o,_8Y];},_9q=3,_9r=new T(function(){return B(_9p(_9q));}),_9s=new T(function(){return die(_9r);}),_9t=function(_9u,_9v){var _9w=E(_9u);if(!_9w[0]){var _9x=_9w[1],_9y=E(_9v);return _9y[0]==0?_9x==_9y[1]:I_compareInt(_9y[1],_9x)==0?true:false;}else{var _9z=_9w[1],_9A=E(_9v);return _9A[0]==0?I_compareInt(_9z,_9A[1])==0?true:false:I_compare(_9z,_9A[1])==0?true:false;}},_9B=function(_9C){return E(E(_9C)[7]);},_9D=function(_9E,_9F){var _9G=E(_9E);if(!_9G[0]){var _9H=_9G[1],_9I=E(_9F);return _9I[0]==0?_9H>=_9I[1]:I_compareInt(_9I[1],_9H)<=0;}else{var _9J=_9G[1],_9K=E(_9F);return _9K[0]==0?I_compareInt(_9J,_9K[1])>=0:I_compare(_9J,_9K[1])>=0;}},_9L=function(_9M){return E(E(_9M)[2]);},_9N=[0,0],_9O=function(_9P,_9Q){var _9R=E(_9P);if(!_9R[0]){var _9S=_9R[1],_9T=E(_9Q);return _9T[0]==0?_9S>_9T[1]:I_compareInt(_9T[1],_9S)<0;}else{var _9U=_9R[1],_9V=E(_9Q);return _9V[0]==0?I_compareInt(_9U,_9V[1])>0:I_compare(_9U,_9V[1])>0;}},_9W=[0,1000],_9X=function(_9Y,_9Z){while(1){var _a0=E(_9Y);if(!_a0[0]){var _a1=_a0[1],_a2=E(_9Z);if(!_a2[0]){var _a3=_a2[1],_a4=subC(_a1,_a3);if(!E(_a4[2])){return [0,_a4[1]];}else{_9Y=[1,I_fromInt(_a1)];_9Z=[1,I_fromInt(_a3)];continue;}}else{_9Y=[1,I_fromInt(_a1)];_9Z=_a2;continue;}}else{var _a5=E(_9Z);if(!_a5[0]){_9Y=_a0;_9Z=[1,I_fromInt(_a5[1])];continue;}else{return [1,I_sub(_a0[1],_a5[1])];}}}},_a6=function(_a7,_a8){var _a9=_a7%_a8;if(_a7<=0){if(_a7>=0){return E(_a9);}else{if(_a8<=0){return E(_a9);}else{var _aa=E(_a9);return _aa==0?0:_aa+_a8|0;}}}else{if(_a8>=0){if(_a7>=0){return E(_a9);}else{if(_a8<=0){return E(_a9);}else{var _ab=E(_a9);return _ab==0?0:_ab+_a8|0;}}}else{var _ac=E(_a9);return _ac==0?0:_ac+_a8|0;}}},_ad=function(_ae,_af){while(1){var _ag=E(_ae);if(!_ag[0]){var _ah=E(_ag[1]);if(_ah==(-2147483648)){_ae=[1,I_fromInt(-2147483648)];continue;}else{var _ai=E(_af);if(!_ai[0]){return [0,B(_a6(_ah,_ai[1]))];}else{_ae=[1,I_fromInt(_ah)];_af=_ai;continue;}}}else{var _aj=_ag[1],_ak=E(_af);return _ak[0]==0?[0,I_toInt(I_mod(_aj,I_fromInt(_ak[1])))]:[1,I_mod(_aj,_ak[1])];}}},_al=function(_am){return E(E(_am)[1]);},_an=function(_ao,_ap){while(1){var _aq=E(_ao);if(!_aq[0]){var _ar=_aq[1],_as=E(_ap);if(!_as[0]){var _at=_as[1],_au=addC(_ar,_at);if(!E(_au[2])){return [0,_au[1]];}else{_ao=[1,I_fromInt(_ar)];_ap=[1,I_fromInt(_at)];continue;}}else{_ao=[1,I_fromInt(_ar)];_ap=_as;continue;}}else{var _av=E(_ap);if(!_av[0]){_ao=_aq;_ap=[1,I_fromInt(_av[1])];continue;}else{return [1,I_add(_aq[1],_av[1])];}}}},_aw=function(_ax){return [0,_ax];},_ay=function(_az,_aA){while(1){var _aB=E(_az);if(!_aB[0]){var _aC=_aB[1],_aD=E(_aA);if(!_aD[0]){var _aE=_aD[1];if(!(imul(_aC,_aE)|0)){return [0,imul(_aC,_aE)|0];}else{_az=[1,I_fromInt(_aC)];_aA=[1,I_fromInt(_aE)];continue;}}else{_az=[1,I_fromInt(_aC)];_aA=_aD;continue;}}else{var _aF=E(_aA);if(!_aF[0]){_az=_aB;_aA=[1,I_fromInt(_aF[1])];continue;}else{return [1,I_mul(_aB[1],_aF[1])];}}}},_aG=function(_aH,_aI,_aJ,_aK,_aL){while(1){var _aM=(function(_aN,_aO,_aP,_aQ,_aR){if(!B(_9O(_aP,_aQ))){var _aS=B(_an(B(_9X(_aQ,_aP)),_8s)),_aT=new T(function(){return B(A(_9L,[_aN,_aR]));}),_aU=new T(function(){return E(E(_aT)[1]);}),_aV=new T(function(){return B(_an(B(_9X(B(_aw(E(E(_aT)[2])[1])),B(_aw(E(_aU)[1])))),_8s));}),_aW=B((function(_aX,_aY,_aZ){while(1){if(!B(_9D(_aX,B(_ay(_aS,_9W))))){var _b0=B(A(new T(function(){return B(_al(_aN));}),[_aZ])),_b1=B(_ay(_aX,_aV)),_b2=B(_an(B(_ay(_aY,_aV)),B(_9X(B(_aw(E(_b0[1])[1])),new T(function(){return B(_aw(E(_aU)[1]));})))));_aZ=_b0[2];_aX=_b1;_aY=_b2;continue;}else{return [0,_aY,_aZ];}}})(_8s,_9N,_aR));return [0,new T(function(){return B(A(_9B,[_aO,new T(function(){if(!B(_9t(_aS,_9N))){var _b3=B(_an(_aP,B(_ad(_aW[1],_aS))));}else{var _b3=E(_9s);}return _b3;})]));}),_aW[2]];}else{var _b4=_aN,_b5=_aO,_b6=_aQ,_b7=_aP,_b8=_aR;_aH=_b4;_aI=_b5;_aJ=_b6;_aK=_b7;_aL=_b8;return null;}})(_aH,_aI,_aJ,_aK,_aL);if(_aM!=null){return _aM;}}},_b9=[0,0],_ba=function(_bb,_bc,_bd){var _be=E(_bc);if(!_be){return [0];}else{var _bf=new T(function(){var _bg=B(_aG(_bb,_8r,_b9,B(_aw(_be)),_bd));return [0,_bg[1],_bg[2]];});return [1,[0,new T(function(){return E(E(_bf)[1]);}),_bd],new T(function(){return B(_ba(_bb,_be-1|0,new T(function(){return E(E(_bf)[2]);})));})];}},_bh=function(_bi,_bj,_bk){return new F(function(){return _ba(_bi,E(_bj)[1],_bk);});},_bl=new T(function(){return B(unCStr("[extractTree] impossible"));}),_bm=new T(function(){return B(err(_bl));}),_bn=function(_bo,_bp){var _bq=function(_br){var _bs=E(_bp);if(!_bs[0]){return E(_bm);}else{var _bt=_bs[1],_bu=_bs[3],_bv=E(_bs[2]);if(!_bv[0]){var _bw=new T(function(){var _bx=B(_bn(_bo-1|0,_bu));return [0,_bx[1],_bx[2]];});return [0,new T(function(){return E(E(_bw)[1]);}),new T(function(){return [1,_bt-1|0,E(_bv),E(E(E(_bw)[2]))];})];}else{var _by=_bv[1],_bz=function(_bA){if(_bo>=_by){var _bB=new T(function(){var _bC=B(_bn(_bo-_by|0,_bu));return [0,_bC[1],_bC[2]];});return [0,new T(function(){return E(E(_bB)[1]);}),new T(function(){return [1,_bt-1|0,E(_bv),E(E(E(_bB)[2]))];})];}else{var _bD=new T(function(){var _bE=B(_bn(_bo,_bv));return [0,_bE[1],_bE[2]];});return [0,new T(function(){return E(E(_bD)[1]);}),new T(function(){return [1,_bt-1|0,E(E(E(_bD)[2])),E(_bu)];})];}},_bF=E(_bu);if(!_bF[0]){return (_bo+1|0)!=_bt?B(_bz(_)):[0,_bF[1],_bv];}else{return new F(function(){return _bz(_);});}}}};switch(E(_bo)){case 0:var _bG=E(_bp);if(!_bG[0]){return new F(function(){return _bq(_);});}else{var _bH=E(_bG[2]);return _bH[0]==0?[0,_bH[1],_bG[3]]:B(_bq(_));}break;case 1:var _bI=E(_bp);if(!_bI[0]){return new F(function(){return _bq(_);});}else{if(E(_bI[1])==2){var _bJ=E(_bI[2]);if(!_bJ[0]){var _bK=E(_bI[3]);return _bK[0]==0?[0,_bK[1],_bJ]:B(_bq(_));}else{return new F(function(){return _bq(_);});}}else{return new F(function(){return _bq(_);});}}break;default:return new F(function(){return _bq(_);});}},_bL=new T(function(){return B(unCStr("[shuffle] called with lists of different lengths"));}),_bM=new T(function(){return B(err(_bL));}),_bN=function(_bO,_bP){var _bQ=function(_bR){var _bS=E(_bP);if(!_bS[0]){return E(_bM);}else{var _bT=new T(function(){var _bU=B(_bn(E(_bS[1])[1],_bO));return [0,_bU[1],_bU[2]];});return [1,new T(function(){return E(E(_bT)[1]);}),new T(function(){return B(_bN(E(_bT)[2],_bS[2]));})];}},_bV=E(_bO);return _bV[0]==0?E(_bP)[0]==0?[1,_bV[1],_1o]:B(_bQ(_)):B(_bQ(_));},_bW=function(_bX){var _bY=E(_bX);if(!_bY[0]){return [0];}else{var _bZ=_bY[1],_c0=E(_bY[2]);if(!_c0[0]){return [1,_bZ,_1o];}else{var _c1=E(_c0[1]);return [1,new T(function(){var _c2=E(E(_bZ));if(!_c2[0]){var _c3=E(_c1);if(!_c3[0]){var _c4=[1,2,E(_c2),E(_c3)];}else{var _c4=[1,_c3[1]+1|0,E(_c2),E(_c3)];}var _c5=_c4;}else{var _c6=_c2[1],_c7=E(_c1);if(!_c7[0]){var _c8=[1,_c6+1|0,E(_c2),E(_c7)];}else{var _c8=[1,_c6+_c7[1]|0,E(_c2),E(_c7)];}var _c5=_c8;}return _c5;}),new T(function(){return B(_bW(_c0[2]));})];}}},_c9=new T(function(){return B(_bW(_1o));}),_ca=new T(function(){return B(_cb(_c9));}),_cb=function(_cc){while(1){var _cd=E(_cc);if(!_cd[0]){return E(_ca);}else{if(!E(_cd[2])[0]){return E(_cd[1]);}else{_cc=B(_bW(_cd));continue;}}}},_ce=function(_cf,_cg,_ch){return function(_ci){return new F(function(){return _bN(new T(function(){return B(_cb(B(_2s(_7T,_cg))));}),B(_7V(B(_bh(_cf,new T(function(){return [0,E(_ch)[1]-1|0];}),_ci))))[1]);});};},_cj=function(_ck,_cl){var _cm=E(_ck);if(!_cm){return [0];}else{var _cn=E(_cl);return _cn[0]==0?[0]:[1,_cn[1],new T(function(){return B(_cj(_cm-1|0,_cn[2]));})];}},_co=function(_cp,_cq,_cr){var _cs=new T(function(){return B(A(_ce,[_cp,_7y,_7A,_cr]));}),_ct=new T(function(){return B(A(_ce,[_cp,_7y,_7A,_cq]));});return [0,[0,[0,new T(function(){return B(_cj(3,_ct));}),new T(function(){return B(_7e(3,_ct));}),_7S,_7R,_7a],[0,new T(function(){return B(_cj(3,_cs));}),new T(function(){return B(_7e(3,_cs));}),_7Q,_7P,_7N]],_6H,_7d,_7J];},_cu=function(_cv,_cw){return new F(function(){return A(_cv,[_cw]);});},_cx=function(_cy,_cz,_cA,_cB){return new F(function(){return A(_cz,[function(_cC){var _cD=E(_cy)[1],_cE=[1,_cC,new T(function(){var _cF=E(_cy)[1]+1|0;return _cF>=0?B(_7e(_cF,_cB)):E(_cB);})];if(_cD>0){var _cG=function(_cH,_cI){var _cJ=E(_cH);if(!_cJ[0]){return E(_cE);}else{var _cK=_cJ[1];return _cI>1?[1,_cK,new T(function(){return B(_cG(_cJ[2],_cI-1|0));})]:[1,_cK,_cE];}};return new F(function(){return _cG(_cB,_cD);});}else{return E(_cE);}},new T(function(){return B(A(_cA,[new T(function(){var _cL=E(_cy)[1];return _cL>=0?B(_4(_cB,_cL)):E(_1);})]));})]);});},_cM=function(_cN){return [0];},_cO=function(_cP,_cQ,_cR,_cS,_cT,_cU,_cV,_cW){return [0,_cT,_cU,_cV,new T(function(){return B(_cx(_cP,_cu,function(_cX){return new F(function(){return _cx(_cS,_cu,_cM,_cX);});},new T(function(){return B(_cx(_cR,_cu,function(_cY){return new F(function(){return _cx(_cS,_cu,function(_cZ){var _d0=E(_cQ)[1];if(_d0>=0){var _d1=E(_cP)[1];if(_d1>=0){return new F(function(){return _4(B(_4(_cW,_d1)),_d0);});}else{return E(_1);}}else{return E(_1);}},_cY);});},_cW));})));})];},_d2=function(_d3,_d4,_d5){var _d6=B(_d7(_d3,_d4,_d5));return [1,_d6[1],_d6[2]];},_d7=function(_d8,_d9,_da){var _db=E(_da);if(!_db[0]){return [0,_d9,_1o];}else{var _dc=_db[1];return B(A(_d8,[_d9,_dc]))==2?[0,_dc,new T(function(){return B(_d2(_d8,_d9,_db[2]));})]:[0,_d9,_db];}},_dd=[5],_de=function(_df,_dg){return _df>=_dg?_df!=_dg?2:1:0;},_dh=function(_di,_dj){return new F(function(){return _de(E(_di)[1],E(_dj)[1]);});},_dk=function(_dl,_dm){var _dn=new T(function(){var _do=_dl+1|0;return _do>=0?B(_7e(_do,_dm)):E(_dm);});if(_dl>0){var _dp=function(_dq,_dr){var _ds=E(_dq);if(!_ds[0]){return E(_dn);}else{var _dt=_ds[1];return _dr>1?[1,_dt,new T(function(){return B(_dp(_ds[2],_dr-1|0));})]:[1,_dt,_dn];}};return new F(function(){return _dp(_dm,_dl);});}else{return E(_dn);}},_du=function(_dv,_dw){return new F(function(){return _dk(E(_dv)[1],_dw);});},_dx=function(_dy,_dz,_dA){return new F(function(){return A(_dy,[new T(function(){return B(A(_dz,[_dA]));})]);});},_dB=function(_dC,_dD){while(1){var _dE=(function(_dF,_dG){var _dH=E(_dG);if(!_dH[0]){return E(_dF);}else{_dC=function(_dI){return new F(function(){return _dx(_dF,_dH[1],_dI);});};_dD=_dH[2];return null;}})(_dC,_dD);if(_dE!=null){return _dE;}}},_dJ=function(_dK,_dL,_dM,_dN,_dO,_dP){var _dQ=new T(function(){if(!E(_dO)){var _dR=E(E(E(_dN)[2])[1]);}else{var _dR=E(E(E(_dN)[1])[1]);}return _dR;}),_dS=function(_dT){var _dU=E(_dT);if(!_dU[0]){return 0;}else{var _dV=_dU[2],_dW=E(_dU[1])[1];return _dW>=0?B(_4(_dQ,_dW))[0]==0?2+B(_dS(_dV))|0:1+B(_dS(_dV))|0:E(_1);}};if(_dK<=B((function(_dX,_dY){var _dZ=E(_dX)[1];return _dZ>=0?B(_4(_dQ,_dZ))[0]==0?2+B(_dS(_dY))|0:1+B(_dS(_dY))|0:E(_1);})(_dM,_dL))){var _e0=new T(function(){if(!E(_dO)){var _e1=E(E(_dN)[2]);}else{var _e1=E(E(_dN)[1]);}return _e1;}),_e2=new T(function(){var _e3=B(_d7(_dh,_dM,_dL));return B(A(_dB,[_2b,B(_2s(_du,[1,_e3[1],_e3[2]])),new T(function(){return E(E(_e0)[1]);})]));});return [0,new T(function(){if(!E(_dO)){var _e4=[0,E(_dN)[1],new T(function(){var _e5=E(_e0);return [0,_e2,_e5[2],_e5[3],_e5[4],_e5[5]];})];}else{var _e4=[0,new T(function(){var _e6=E(_e0);return [0,_e2,_e6[2],_e6[3],_e6[4],_e6[5]];}),E(_dN)[2]];}return _e4;}),_dO,_dd,_dP];}else{return [0,_dN,_dO,[3,[0,_dK],new T(function(){var _e7=B(_d7(_dh,_dM,_dL));return [1,_e7[1],_e7[2]];})],_dP];}},_e8=[0,0],_e9=function(_ea,_eb){var _ec=E(_eb);return _ec[0]==0?[0]:[1,_ea,new T(function(){return B(_e9(_ec[1],_ec[2]));})];},_ed=new T(function(){return B(unCStr("init"));}),_ee=new T(function(){return B(_4u(_ed));}),_ef=function(_eg,_eh,_ei,_ej,_ek,_el,_em){var _en=function(_eo){var _ep=new T(function(){var _eq=E(_em);if(!_eq[0]){var _er=E(_ee);}else{var _es=_eq[1],_et=_eq[2],_er=B(_1K(B(_e9(_es,_et)),[1,new T(function(){return B(_cx(_eh,_cu,function(_eu){return [1,[0,_ek,[0,_ei]]];},new T(function(){return B(_4o(_es,_et));})));}),_1o]));}return _er;}),_ev=new T(function(){if(!E(_ek)){var _ew=E(_ej),_ex=[0,_ew[1],new T(function(){var _ey=E(_ew[2]);return [0,new T(function(){return B(_du(_eg,_ey[1]));}),_ey[2],_ey[3],_ey[4],_ey[5]];})];}else{var _ez=E(_ej),_ex=[0,new T(function(){var _eA=E(_ez[1]);return [0,new T(function(){return B(_du(_eg,_eA[1]));}),_eA[2],_eA[3],_eA[4],_eA[5]];}),_ez[2]];}return _ex;});return E(_eo)==0?[0,_ev,_ek,_dd,_ep]:[0,_ev,_ek,[3,new T(function(){return _ei<=10?E(_e8):E(_4b);}),_1o],_ep];};return _ei<=10?B(_en(0)):B(_en(2));},_eB=[0,0],_eC=[0,0],_eD=0,_eE=new T(function(){return B(_9p(_eD));}),_eF=new T(function(){return die(_eE);}),_eG=function(_eH,_eI){var _eJ=E(_eI);if(!_eJ){return E(_9s);}else{var _eK=function(_eL){if(_eH<=0){if(_eH>=0){var _eM=quotRemI(_eH,_eJ);return [0,[0,_eM[1]],[0,_eM[2]]];}else{if(_eJ<=0){var _eN=quotRemI(_eH,_eJ);return [0,[0,_eN[1]],[0,_eN[2]]];}else{var _eO=quotRemI(_eH+1|0,_eJ);return [0,[0,_eO[1]-1|0],[0,(_eO[2]+_eJ|0)-1|0]];}}}else{if(_eJ>=0){if(_eH>=0){var _eP=quotRemI(_eH,_eJ);return [0,[0,_eP[1]],[0,_eP[2]]];}else{if(_eJ<=0){var _eQ=quotRemI(_eH,_eJ);return [0,[0,_eQ[1]],[0,_eQ[2]]];}else{var _eR=quotRemI(_eH+1|0,_eJ);return [0,[0,_eR[1]-1|0],[0,(_eR[2]+_eJ|0)-1|0]];}}}else{var _eS=quotRemI(_eH-1|0,_eJ);return [0,[0,_eS[1]-1|0],[0,(_eS[2]+_eJ|0)+1|0]];}}};return E(_eJ)==(-1)?E(_eH)==(-2147483648)?[0,_eF,_eC]:B(_eK(_)):B(_eK(_));}},_eT=function(_eU){var _eV=B(_eG((_eU>>>0&2147483647>>>0)>>>0&4.294967295e9,2147483562));return [0,E(_eV[2])[1]+1|0,B(_a6(E(_eV[1])[1],2147483398))+1|0];},_eW=function(_){var _eX=B(A(_2U,["(function(){ return Math.floor(Math.random() * Math.pow(2, 53)); })",_])),_eY=_eX;return new T(function(){var _eZ=jsTrunc(_eY),_f0=_eZ,_f1=B(_eT(_f0));return [0,_f1[1],_f1[2]];});},_f2=new T(function(){return B(_2U("(function(e){return e.parentNode;})"));}),_f3=function(_f4,_f5){while(1){var _f6=E(_f4);if(!_f6[0]){return E(_f5)[0]==0?true:false;}else{var _f7=E(_f5);if(!_f7[0]){return false;}else{if(E(_f6[1])[1]!=E(_f7[1])[1]){return false;}else{_f4=_f6[2];_f5=_f7[2];continue;}}}}},_f8=new T(function(){return B(_2U("(function(e){ return e.tagName })"));}),_f9=new T(function(){return B(unCStr("wheel"));}),_fa=new T(function(){return B(unCStr("mouseout"));}),_fb=new T(function(){return B(unCStr("mouseover"));}),_fc=new T(function(){return B(unCStr("mousemove"));}),_fd=new T(function(){return B(unCStr("blur"));}),_fe=new T(function(){return B(unCStr("focus"));}),_ff=new T(function(){return B(unCStr("change"));}),_fg=new T(function(){return B(unCStr("unload"));}),_fh=new T(function(){return B(unCStr("load"));}),_fi=new T(function(){return B(unCStr("submit"));}),_fj=new T(function(){return B(unCStr("keydown"));}),_fk=new T(function(){return B(unCStr("keyup"));}),_fl=new T(function(){return B(unCStr("keypress"));}),_fm=new T(function(){return B(unCStr("mouseup"));}),_fn=new T(function(){return B(unCStr("mousedown"));}),_fo=new T(function(){return B(unCStr("dblclick"));}),_fp=new T(function(){return B(unCStr("click"));}),_fq=function(_fr){switch(E(_fr)[0]){case 0:return E(_fh);case 1:return E(_fg);case 2:return E(_ff);case 3:return E(_fe);case 4:return E(_fd);case 5:return E(_fc);case 6:return E(_fb);case 7:return E(_fa);case 8:return E(_fp);case 9:return E(_fo);case 10:return E(_fn);case 11:return E(_fm);case 12:return E(_fl);case 13:return E(_fk);case 14:return E(_fj);case 15:return E(_fi);default:return E(_f9);}},_fs=new T(function(){return E(0);}),_ft=new T(function(){return B(_2U("(function(f) {  return (function() {      return (function(){        var args=Array.prototype.slice.call(arguments,0);        args.push(0);        return E(B(A(f, args)));    });  });})"));}),_fu=function(_fv,_){return new F(function(){return A(_ft,[E(_fv),_]);});},_fw=function(_fx,_){return new F(function(){return _fu(_fx,_);});},_fy=new T(function(){return B(_2U("(function(node, evtName, f){ node.addEventListener(evtName, (function(e){ f(e.target) }))})"));}),_fz=function(_fA,_fB){return function(_fC,_){var _fD=E(_fC),_fE=B(A(_fy,[E(_fD[1]),E(toJSStr(E(new T(function(){return B(_fq(_fA));})))),E(new T(function(){return B(_2Q(function(_){var _=0;return new F(function(){return _fw(function(_fF){return new F(function(){return _2Q(function(_){var _=0,_fG=B(A(_fB,[[0,_fF],_])),_fH=_fG;return E(_fs);});});},_);});}));})),_])),_fI=_fE;return _fD;};},_fJ=function(_){var _fK=B(A(_2U,["(function(){return document.body;})",_])),_fL=_fK;return [0,_fL];},_fM=new T(function(){return B(unCStr("LI"));}),_fN=new T(function(){return B(unCStr("TD"));}),_fO=function(_fP,_){var _fQ=B(_fJ(_)),_fR=_fQ;return new F(function(){return A(_4W,[_fP,_fR,_]);});},_fS=new T(function(){return B(unCStr("#field"));}),_fT=new T(function(){return B(unCStr("#yours ol.hand"));}),_fU=function(_){var _fV=B(_eW(_)),_fW=_fV,_fX=B(_eW(_)),_fY=_fX,_fZ=newMVar(),_g0=_fZ,_g1=new T(function(){var _g2=B(_co(_X,_fW,_fY)),_g3=E(_g2[1]),_g4=B(_6L(_g3[1],_g3[2],_g2[2],_g2[4]));return [0,_g4[1],_g4[2],_g4[3],_g4[4]];}),_=putMVar(_g0,_g1),_g5=function(_){var _g6=0,_g7=_g6;if(!E(_g7)){var _g8=B((function(_){var _g9=takeMVar(_g0),_ga=_g9,_gb=jsCatch(function(_){return new F(function(){return (function(_){return new F(function(){return _fO(_ga,_);});})();});},function(_gc,_){var _=putMVar(_g0,_ga);return new F(function(){return die(_gc);});}),_gd=_gb,_=putMVar(_g0,_ga);return _gd;})()),_ge=_g8;return _24;}else{var _gf=takeMVar(_g0),_gg=_gf,_gh=jsCatch(function(_){return new F(function(){return _fO(_gg,_);});},function(_gi,_){var _=putMVar(_g0,_gg);return new F(function(){return die(_gi);});}),_gj=_gh,_=putMVar(_g0,_gg);return _24;}},_gk=B(_fJ(_)),_gl=_gk,_gm=B(_30(_fT,new T(function(){return B(_fz(_9,function(_gn,_){var _go=E(_gn)[1],_gp=B(A(_f8,[E(_go),_])),_gq=_gp;if(!B(_f3(B(_a(_gq)),_fM))){return _24;}else{var _gr=B(_6g(_eB,_go,_)),_gs=_gr,_gt=0,_gu=_gt,_gv=function(_gw,_){return new T(function(){var _gx=E(_gw),_gy=_gx[1],_gz=_gx[2],_gA=_gx[4],_gB=E(_gx[3]);switch(_gB[0]){case 1:var _gC=E(_gs),_gD=_gC[1],_gE=E(_gy);if(_gD>=0){if(!E(_gz)){var _gF=B(_4(E(_gE[2])[1],_gD))[0]==0?[0,_gE,_6F,_6G,_gA]:[0,_gE,_6F,[4,_gC],_gA];}else{var _gF=B(_4(E(_gE[1])[1],_gD))[0]==0?[0,_gE,_6H,_6G,_gA]:[0,_gE,_6H,[4,_gC],_gA];}var _gG=_gF;}else{var _gG=E(_1);}var _gH=_gG,_gI=_gH,_gJ=_gI,_gK=_gJ;break;case 3:var _gL=B(_dJ(E(_gB[1])[1],_gB[2],_gs,_gy,_gz,_gA)),_gK=[0,_gL[1],_gL[2],_gL[3],_gL[4]];break;default:var _gK=E(_gx);}var _gM=_gK;return _gM;});};if(!E(_gu)){var _gN=B((function(_){var _gO=takeMVar(_g0),_gP=_gO,_gQ=jsCatch(function(_){return new F(function(){return (function(_){return new F(function(){return _gv(_gP,_);});})();});},function(_gR,_){var _=putMVar(_g0,_gP);return new F(function(){return die(_gR);});}),_gS=_gQ,_=putMVar(_g0,_gS);return _24;})()),_gT=_gN;return new F(function(){return _g5(_);});}else{var _gU=takeMVar(_g0),_gV=_gU,_gW=jsCatch(function(_){return new F(function(){return _gv(_gV,_);});},function(_gX,_){var _=putMVar(_g0,_gV);return new F(function(){return die(_gX);});}),_gY=_gW,_=putMVar(_g0,_gY);return new F(function(){return _g5(_);});}}}));}),_gl,_)),_gZ=_gm,_h0=B(_30(_fS,new T(function(){return B(_fz(_9,function(_h1,_){var _h2=E(_h1)[1],_h3=E(_h2),_h4=B(A(_f8,[_h3,_])),_h5=_h4;if(!B(_f3(B(_a(_h5)),_fN))){return _24;}else{var _h6=B(A(_f2,[_h3,_])),_h7=_h6,_h8=B(_6g(_eB,_h7,_)),_h9=_h8,_ha=B(_6g(_eB,_h2,_)),_hb=_ha,_hc=0,_hd=_hc,_he=function(_hf,_){return new T(function(){var _hg=E(_hf),_hh=_hg[1],_hi=_hg[2],_hj=_hg[4],_hk=E(_hg[3]);switch(_hk[0]){case 1:var _hl=[0,_hh,_hi,[2,[0,_h9,_hb]],_hj];break;case 2:var _hm=E(_hk[1]),_hn=B(_cO(_hm[1],_hm[2],_h9,_hb,_hh,_hi,_hk,_hj)),_hl=[0,_hn[1],_hn[2],_hn[3],_hn[4]];break;case 4:var _ho=E(_hk[1]),_hp=_ho[1];if(_hp>=0){var _hq=E(_hh),_hr=B(_4(E(_hq[1])[1],_hp));if(!_hr[0]){var _hs=E(_hg);}else{var _ht=B(_ef(_ho,_hb,E(_hr[1])[1],_hq,_hi,_hk,_hj)),_hs=[0,_ht[1],_ht[2],_ht[3],_ht[4]];}var _hu=_hs,_hv=_hu,_hw=_hv;}else{var _hw=E(_1);}var _hx=_hw,_hy=_hx,_hl=_hy;break;default:var _hl=E(_hg);}var _hz=_hl;return _hz;});};if(!E(_hd)){var _hA=B((function(_){var _hB=takeMVar(_g0),_hC=_hB,_hD=jsCatch(function(_){return new F(function(){return (function(_){return new F(function(){return _he(_hC,_);});})();});},function(_hE,_){var _=putMVar(_g0,_hC);return new F(function(){return die(_hE);});}),_hF=_hD,_=putMVar(_g0,_hF);return _24;})()),_hG=_hA,_hH=B(_g5(_)),_hI=_hH;return _24;}else{var _hJ=takeMVar(_g0),_hK=_hJ,_hL=jsCatch(function(_){return new F(function(){return _he(_hK,_);});},function(_hM,_){var _=putMVar(_g0,_hK);return new F(function(){return die(_hM);});}),_hN=_hL,_=putMVar(_g0,_hN),_hO=B(_g5(_)),_hP=_hO;return _24;}}}));}),_gl,_)),_hQ=_h0,_hR=B(A(_4W,[_g1,_gl,_])),_hS=_hR;return _24;},_hT=function(_){return new F(function(){return _fU(_);});};
var hasteMain = function() {B(A(_hT, [0]));};window.onload = hasteMain;