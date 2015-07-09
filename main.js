function gettimeofday(){}

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

var _0=function(_1,_2,_){var _3=jsCreateTextNode(toJSStr(E(_1))),_4=_3,_5=jsAppendChild(_4,E(_2)[1]);return [0,_4];},_6=new T(function(){return B(unCStr("\u624b\u756a\u3092\u4ea4\u4ee3"));}),_7=new T(function(){return B(unCStr("\u53ec\u559a\u3059\u308b\u4f4d\u7f6e\u3092\u9078\u629e"));}),_8=new T(function(){return B(unCStr("\u751f\u8d04\u3092\u9078\u629e"));}),_9=new T(function(){return B(unCStr("\u624b\u672d\u3092\u9078\u629e"));}),_a=new T(function(){return B(unCStr("\u30c9\u30ed\u30fc"));}),_b=function(_c,_d,_){var _e=E(_c);if(!_e[0]){return _d;}else{var _f=B(A(_e[1],[_d,_])),_g=_f,_h=B(_b(_e[2],_d,_)),_i=_h;return _d;}},_j=[0,119],_k=[0],_l=[1,_j,_k],_m=[0,75],_n=[1,_m,_k],_o=[0,81],_p=[1,_o,_k],_q=[0,74],_r=[1,_q,_k],_s=[0,65],_t=[1,_s,_k],_u=function(_v,_w){var _x=E(_v);return _x[0]==0?E(_w):[1,_x[1],new T(function(){return B(_u(_x[2],_w));})];},_y=function(_z,_A){var _B=jsShowI(_z),_C=_B;return new F(function(){return _u(fromJSStr(_C),_A);});},_D=[0,41],_E=[0,40],_F=function(_G,_H,_I){if(_H>=0){return new F(function(){return _y(_H,_I);});}else{return _G<=6?B(_y(_H,_I)):[1,_E,new T(function(){var _J=jsShowI(_H),_K=_J;return B(_u(fromJSStr(_K),[1,_D,_I]));})];}},_L=function(_M){var _N=E(_M);if(!_N[0]){return E(_l);}else{var _O=E(E(_N[1])[1]);switch(_O){case 1:return E(_t);case 11:return E(_r);case 12:return E(_p);case 13:return E(_n);default:return new F(function(){return _F(0,_O,_k);});}}},_P=new T(function(){return B(unCStr("td"));}),_Q=function(_R,_S,_T,_){var _U=jsCreateElem(toJSStr(E(_P))),_V=_U,_W=jsAppendChild(_V,E(_T)[1]),_X=[0,_V],_Y=B(A(_R,[_S,_X,_])),_Z=_Y;return _X;},_10=function(_11){return function(_12,_13){return new F(function(){return _Q(_0,new T(function(){var _14=E(_11);return _14[0]==0?[0]:B(_L(_14[1]));}),_12,_13);});};},_15=function(_16,_17){var _18=E(_17);return _18[0]==0?[0]:[1,new T(function(){return B(A(_16,[_18[1]]));}),new T(function(){return B(_15(_16,_18[2]));})];},_19=new T(function(){return B(unCStr("tr"));}),_1a=function(_1b,_1c,_1d,_){var _1e=jsCreateElem(toJSStr(E(_19))),_1f=_1e,_1g=jsAppendChild(_1f,E(_1d)[1]),_1h=[0,_1f],_1i=B(A(_1b,[_1c,_1h,_])),_1j=_1i;return _1h;},_1k=function(_1l){return E(_1l);},_1m=function(_1n){return function(_12,_13){return new F(function(){return _1a(_1k,function(_1o,_){return new F(function(){return _b(new T(function(){return B(_15(_10,_1n));}),_1o,_);});},_12,_13);});};},_1p=new T(function(){return B(unCStr("table#field"));}),_1q=new T(function(){return [0,"arr2lst"];}),_1r=function(_1s){var _1t=B(A(_1s,[_])),_1u=_1t;return E(_1u);},_1v=function(_1w){return new F(function(){return _1r(function(_){var _=0;return new F(function(){return eval(_1w);});});});},_1x=function(_1y,_1z){return new F(function(){return _1r(function(_){var _=0;return new F(function(){return A(_1v,[E(_1q)[1],E(_1y),E(_1z),_]);});});});},_1A=new T(function(){return B(_1v("(function(sel){return document.querySelectorAll(sel);})"));}),_1B=function(_1C,_1D,_1E,_){var _1F=B(A(_1A,[E(toJSStr(E(_1C))),_])),_1G=_1F,_1H=function(_1I,_){var _1J=E(_1I);if(!_1J[0]){return _k;}else{var _1K=B(A(_1D,[[0,_1J[1]],_])),_1L=_1K,_1M=B(_1H(_1J[2],_)),_1N=_1M;return [1,_1L,_1N];}},_1O=B(_1H(B(_1x(_1G,0)),_)),_1P=_1O;return _1E;},_1Q=function(_1R){return function(_12,_13){return new F(function(){return _1B(_1p,function(_1S,_){var _1T=E(_1S),_1U=jsClearChildren(_1T[1]),_1V=B(_b(new T(function(){return B(_15(_1m,_1R));}),_1T,_)),_1W=_1V;return _1T;},_12,_13);});};},_1X=new T(function(){return B(unCStr("#status"));}),_1Y=new T(function(){return B(unCStr("\u306e\u756a\u3067\u3059\u3001"));}),_1Z=new T(function(){return B(unCStr(" .deck"));}),_20=new T(function(){return B(unCStr("\u306e\u6b8b\u308a\u5c71\u672d: "));}),_21=[0,35],_22=new T(function(){return B(unCStr(" .hand"));}),_23=function(_24,_25){while(1){var _26=E(_24);if(!_26[0]){return E(_25);}else{_24=_26[2];var _27=_25+1|0;_25=_27;continue;}}},_28=new T(function(){return B(unCStr("li"));}),_29=function(_2a,_2b,_2c,_){var _2d=jsCreateElem(toJSStr(E(_28))),_2e=_2d,_2f=jsAppendChild(_2e,E(_2c)[1]),_2g=[0,_2e],_2h=B(A(_2a,[_2b,_2g,_])),_2i=_2h;return _2g;},_2j=function(_2k){return function(_2l,_){var _2m=B(_1B([1,_21,new T(function(){return B(_u(E(_2k)[4],_1Z));})],function(_2n,_){var _2o=E(_2n),_2p=jsClearChildren(_2o[1]),_2q=B(_0(new T(function(){var _2r=E(_2k);return B(_u(_2r[3],new T(function(){return B(_u(_20,new T(function(){return B(_F(0,B(_23(_2r[2],0)),_k));},1)));},1)));}),_2o,_)),_2s=_2q;return _2o;},_2l,_)),_2t=_2m,_2u=B(_1B([1,_21,new T(function(){return B(_u(E(_2k)[4],_22));})],function(_2v,_){var _2w=E(_2v),_2x=jsClearChildren(_2w[1]),_2y=B(_b(new T(function(){var _2z=E(_2k);return B(_15(function(_2A){return function(_12,_13){return new F(function(){return _29(_0,new T(function(){return B(A(_2z[5],[_2A]));}),_12,_13);});};},_2z[1]));}),_2w,_)),_2B=_2y;return _2w;},_2l,_)),_2C=_2u;return _2l;};},_2D=function(_2E){var _2F=new T(function(){return E(E(_2E)[1]);});return function(_2G,_){var _2H=B(A(new T(function(){return B(_1Q(new T(function(){return E(E(_2E)[4]);},1)));}),[_2G,_])),_2I=_2H,_2J=B(_1B(_1X,function(_2K,_){var _2L=E(_2K),_2M=jsClearChildren(_2L[1]),_2N=B(_0(new T(function(){return B(unAppCStr("-- ",new T(function(){var _2O=E(_2E),_2P=_2O[1],_2Q=new T(function(){return B(_u(_1Y,new T(function(){switch(E(_2O[3])[0]){case 0:var _2R=E(_a);break;case 1:var _2R=E(_9);break;case 2:var _2R=E(_8);break;case 3:var _2R=E(_7);break;default:var _2R=E(_6);}return _2R;},1)));},1);if(!E(_2O[2])){var _2S=B(_u(E(E(_2P)[2])[3],_2Q));}else{var _2S=B(_u(E(E(_2P)[1])[3],_2Q));}var _2T=_2S;return _2T;})));}),_2L,_)),_2U=_2N;return _2L;},_2G,_)),_2V=_2J,_2W=B(A(new T(function(){return B(_2j(new T(function(){return E(E(_2F)[1]);})));}),[_2G,_])),_2X=_2W,_2Y=B(A(new T(function(){return B(_2j(new T(function(){return E(E(_2F)[2]);})));}),[_2G,_])),_2Z=_2Y;return _2G;};},_30=0,_31=function(_){var _32=B(A(_1v,["(function(){return document.body;})",_])),_33=_32;return [0,_33];},_34=[0],_35=true,_36=function(_37,_38){while(1){var _39=E(_37);if(!_39){return E(_38);}else{var _3a=E(_38);if(!_3a[0]){return [0];}else{_37=_39-1|0;_38=_3a[2];continue;}}}},_3b=function(_3c){return [0,E(E(_3c))];},_3d=function(_3e,_3f){return [0,imul(E(_3e)[1],E(_3f)[1])|0];},_3g=function(_3h,_3i){return [0,E(_3h)[1]+E(_3i)[1]|0];},_3j=function(_3k,_3l){return [0,E(_3k)[1]-E(_3l)[1]|0];},_3m=function(_3n){var _3o=E(_3n),_3p=_3o[1];return _3p<0?[0, -_3p]:E(_3o);},_3q=function(_3r){var _3s=E(_3r);return _3s[0]==0?E(_3s[1]):I_toInt(_3s[1]);},_3t=function(_3u){return [0,B(_3q(_3u))];},_3v=function(_3w){return [0, -E(_3w)[1]];},_3x=[0,-1],_3y=[0,0],_3z=[0,1],_3A=function(_3B){var _3C=E(_3B)[1];return _3C>=0?E(_3C)==0?E(_3y):E(_3z):E(_3x);},_3D=[0,_3g,_3d,_3j,_3v,_3m,_3A,_3t],_3E=[0,2147483647],_3F=[0,-2147483648],_3G=[0,1],_3H=function(_3I,_3J){return [0,E(_3I)[1],E(_3J)[1]];},_3K=function(_3L,_3M){var _3N=quot(_3M,52774),_3O=(imul(40692,_3M-(imul(_3N,52774)|0)|0)|0)-(imul(_3N,3791)|0)|0,_3P=new T(function(){if(_3O>=0){var _3Q=[0,_3O];}else{var _3Q=[0,_3O+2147483399|0];}var _3R=_3Q;return _3R;}),_3S=quot(_3L,53668),_3T=(imul(40014,_3L-(imul(_3S,53668)|0)|0)|0)-(imul(_3S,12211)|0)|0,_3U=new T(function(){if(_3T>=0){var _3V=[0,_3T];}else{var _3V=[0,_3T+2147483563|0];}var _3W=_3V;return _3W;});return [0,new T(function(){var _3X=E(_3U)[1]-E(_3P)[1]|0;if(_3X>=1){var _3Y=[0,_3X];}else{var _3Y=[0,_3X+2147483562|0];}var _3Z=_3Y,_40=_3Z,_41=_40,_42=_41;return _42;}),new T(function(){return B(_3H(_3U,_3P));})];},_43=[0,2147483562],_44=new T(function(){return B(unCStr("base"));}),_45=new T(function(){return B(unCStr("GHC.Exception"));}),_46=new T(function(){return B(unCStr("ArithException"));}),_47=[0,I_fromBits([4194982440,719304104]),I_fromBits([3110813675,1843557400]),_44,_45,_46],_48=[0,I_fromBits([4194982440,719304104]),I_fromBits([3110813675,1843557400]),_47,_k],_49=function(_4a){return E(_48);},_4b=function(_4c){return E(E(_4c)[1]);},_4d=function(_4e,_4f,_4g){var _4h=B(A(_4e,[_])),_4i=B(A(_4f,[_])),_4j=hs_eqWord64(_4h[1],_4i[1]),_4k=_4j;if(!E(_4k)){return [0];}else{var _4l=hs_eqWord64(_4h[2],_4i[2]),_4m=_4l;return E(_4m)==0?[0]:[1,_4g];}},_4n=function(_4o){var _4p=E(_4o);return new F(function(){return _4d(B(_4b(_4p[1])),_49,_4p[2]);});},_4q=new T(function(){return B(unCStr("arithmetic underflow"));}),_4r=new T(function(){return B(unCStr("arithmetic overflow"));}),_4s=new T(function(){return B(unCStr("Ratio has zero denominator"));}),_4t=new T(function(){return B(unCStr("denormal"));}),_4u=new T(function(){return B(unCStr("divide by zero"));}),_4v=new T(function(){return B(unCStr("loss of precision"));}),_4w=function(_4x){switch(E(_4x)){case 0:return E(_4r);case 1:return E(_4q);case 2:return E(_4v);case 3:return E(_4u);case 4:return E(_4t);default:return E(_4s);}},_4y=function(_4z){return new F(function(){return _u(_4q,_4z);});},_4A=function(_4z){return new F(function(){return _u(_4r,_4z);});},_4B=function(_4z){return new F(function(){return _u(_4s,_4z);});},_4C=function(_4z){return new F(function(){return _u(_4t,_4z);});},_4D=function(_4z){return new F(function(){return _u(_4u,_4z);});},_4E=function(_4z){return new F(function(){return _u(_4v,_4z);});},_4F=function(_4G){switch(E(_4G)){case 0:return E(_4A);case 1:return E(_4y);case 2:return E(_4E);case 3:return E(_4D);case 4:return E(_4C);default:return E(_4B);}},_4H=[0,44],_4I=[0,93],_4J=[0,91],_4K=function(_4L,_4M,_4N){var _4O=E(_4M);return _4O[0]==0?B(unAppCStr("[]",_4N)):[1,_4J,new T(function(){return B(A(_4L,[_4O[1],new T(function(){var _4P=function(_4Q){var _4R=E(_4Q);return _4R[0]==0?E([1,_4I,_4N]):[1,_4H,new T(function(){return B(A(_4L,[_4R[1],new T(function(){return B(_4P(_4R[2]));})]));})];};return B(_4P(_4O[2]));})]));})];},_4S=function(_4T,_4U){return new F(function(){return _4K(_4F,_4T,_4U);});},_4V=function(_4W,_4X){switch(E(_4X)){case 0:return E(_4A);case 1:return E(_4y);case 2:return E(_4E);case 3:return E(_4D);case 4:return E(_4C);default:return E(_4B);}},_4Y=[0,_4V,_4w,_4S],_4Z=new T(function(){return [0,_49,_4Y,_50,_4n];}),_50=function(_4z){return [0,_4Z,_4z];},_51=3,_52=new T(function(){return B(_50(_51));}),_53=new T(function(){return die(_52);}),_54=function(_55,_56){var _57=E(_55);if(!_57[0]){var _58=_57[1],_59=E(_56);return _59[0]==0?_58==_59[1]:I_compareInt(_59[1],_58)==0?true:false;}else{var _5a=_57[1],_5b=E(_56);return _5b[0]==0?I_compareInt(_5a,_5b[1])==0?true:false:I_compare(_5a,_5b[1])==0?true:false;}},_5c=function(_5d){return E(E(_5d)[7]);},_5e=function(_5f,_5g){var _5h=E(_5f);if(!_5h[0]){var _5i=_5h[1],_5j=E(_5g);return _5j[0]==0?_5i>=_5j[1]:I_compareInt(_5j[1],_5i)<=0;}else{var _5k=_5h[1],_5l=E(_5g);return _5l[0]==0?I_compareInt(_5k,_5l[1])>=0:I_compare(_5k,_5l[1])>=0;}},_5m=[0,0],_5n=function(_5o,_5p){var _5q=E(_5o);if(!_5q[0]){var _5r=_5q[1],_5s=E(_5p);return _5s[0]==0?_5r>_5s[1]:I_compareInt(_5s[1],_5r)<0;}else{var _5t=_5q[1],_5u=E(_5p);return _5u[0]==0?I_compareInt(_5t,_5u[1])>0:I_compare(_5t,_5u[1])>0;}},_5v=[0,1000],_5w=function(_5x,_5y){while(1){var _5z=E(_5x);if(!_5z[0]){var _5A=_5z[1],_5B=E(_5y);if(!_5B[0]){var _5C=_5B[1],_5D=subC(_5A,_5C);if(!E(_5D[2])){return [0,_5D[1]];}else{_5x=[1,I_fromInt(_5A)];_5y=[1,I_fromInt(_5C)];continue;}}else{_5x=[1,I_fromInt(_5A)];_5y=_5B;continue;}}else{var _5E=E(_5y);if(!_5E[0]){_5x=_5z;_5y=[1,I_fromInt(_5E[1])];continue;}else{return [1,I_sub(_5z[1],_5E[1])];}}}},_5F=function(_5G,_5H){var _5I=_5G%_5H;if(_5G<=0){if(_5G>=0){return E(_5I);}else{if(_5H<=0){return E(_5I);}else{var _5J=E(_5I);return _5J==0?0:_5J+_5H|0;}}}else{if(_5H>=0){if(_5G>=0){return E(_5I);}else{if(_5H<=0){return E(_5I);}else{var _5K=E(_5I);return _5K==0?0:_5K+_5H|0;}}}else{var _5L=E(_5I);return _5L==0?0:_5L+_5H|0;}}},_5M=function(_5N,_5O){while(1){var _5P=E(_5N);if(!_5P[0]){var _5Q=E(_5P[1]);if(_5Q==(-2147483648)){_5N=[1,I_fromInt(-2147483648)];continue;}else{var _5R=E(_5O);if(!_5R[0]){return [0,B(_5F(_5Q,_5R[1]))];}else{_5N=[1,I_fromInt(_5Q)];_5O=_5R;continue;}}}else{var _5S=_5P[1],_5T=E(_5O);return _5T[0]==0?[0,I_toInt(I_mod(_5S,I_fromInt(_5T[1])))]:[1,I_mod(_5S,_5T[1])];}}},_5U=function(_5V,_5W){while(1){var _5X=E(_5V);if(!_5X[0]){var _5Y=_5X[1],_5Z=E(_5W);if(!_5Z[0]){var _60=_5Z[1],_61=addC(_5Y,_60);if(!E(_61[2])){return [0,_61[1]];}else{_5V=[1,I_fromInt(_5Y)];_5W=[1,I_fromInt(_60)];continue;}}else{_5V=[1,I_fromInt(_5Y)];_5W=_5Z;continue;}}else{var _62=E(_5W);if(!_62[0]){_5V=_5X;_5W=[1,I_fromInt(_62[1])];continue;}else{return [1,I_add(_5X[1],_62[1])];}}}},_63=function(_64){return [0,_64];},_65=function(_66,_67){while(1){var _68=E(_66);if(!_68[0]){var _69=_68[1],_6a=E(_67);if(!_6a[0]){var _6b=_6a[1];if(!(imul(_69,_6b)|0)){return [0,imul(_69,_6b)|0];}else{_66=[1,I_fromInt(_69)];_67=[1,I_fromInt(_6b)];continue;}}else{_66=[1,I_fromInt(_69)];_67=_6a;continue;}}else{var _6c=E(_67);if(!_6c[0]){_66=_68;_67=[1,I_fromInt(_6c[1])];continue;}else{return [1,I_mul(_68[1],_6c[1])];}}}},_6d=function(_6e,_6f,_6g,_6h){while(1){var _6i=(function(_6j,_6k,_6l,_6m){if(!B(_5n(_6k,_6l))){var _6n=B(_5U(B(_5w(_6l,_6k)),_3G)),_6o=B((function(_6p,_6q,_6r){while(1){if(!B(_5e(_6p,B(_65(_6n,_5v))))){var _6s=E(_6r),_6t=B(_3K(_6s[1],_6s[2])),_6u=B(_65(_6p,_43)),_6v=B(_5U(B(_65(_6q,_43)),B(_5w(B(_63(E(_6t[1])[1])),_3G))));_6r=_6t[2];_6p=_6u;_6q=_6v;continue;}else{return [0,_6q,_6r];}}})(_3G,_5m,_6m));return [0,new T(function(){return B(A(_5c,[_6j,new T(function(){if(!B(_54(_6n,_5m))){var _6w=B(_5U(_6k,B(_5M(_6o[1],_6n))));}else{var _6w=E(_53);}return _6w;})]));}),_6o[2]];}else{var _6x=_6j,_6y=_6l,_6z=_6k,_6A=_6m;_6e=_6x;_6f=_6y;_6g=_6z;_6h=_6A;return null;}})(_6e,_6f,_6g,_6h);if(_6i!=null){return _6i;}}},_6B=function(_6C){var _6D=new T(function(){var _6E=B(_6d(_3D,_3F,_3E,_6C));return [0,_6E[2],_6E[1]];}),_6F=new T(function(){return E(E(_6D)[1]);});return [0,_6F,new T(function(){var _6G=E(_6F);return E(E(_6D)[2]);})];},_6H=[0,1],_6I=[0,0],_6J=function(_6K,_6L){while(1){var _6M=E(_6K);if(!_6M[0]){var _6N=_6M[1],_6O=E(_6L);if(!_6O[0]){return [0,(_6N>>>0|_6O[1]>>>0)>>>0&4.294967295e9];}else{_6K=[1,I_fromInt(_6N)];_6L=_6O;continue;}}else{var _6P=E(_6L);if(!_6P[0]){_6K=_6M;_6L=[1,I_fromInt(_6P[1])];continue;}else{return [1,I_or(_6M[1],_6P[1])];}}}},_6Q=function(_6R,_6S){while(1){var _6T=E(_6R);if(!_6T[0]){_6R=[1,I_fromInt(_6T[1])];continue;}else{return [1,I_shiftLeft(_6T[1],_6S)];}}},_6U=function(_6V){var _6W=E(_6V);if(!_6W[0]){return E(_6I);}else{return new F(function(){return _6J([0,E(_6W[1])[1]],B(_6Q(B(_6U(_6W[2])),31)));});}},_6X=[0,1],_6Y=[0,2147483647],_6Z=new T(function(){return B(_5U(_6Y,_6X));}),_70=function(_71){var _72=E(_71);if(!_72[0]){var _73=E(_72[1]);return _73==(-2147483648)?E(_6Z):[0, -_73];}else{return [1,I_negate(_72[1])];}},_74=function(_75,_76){if(!E(_75)){return new F(function(){return _70(B(_6U(_76)));});}else{return new F(function(){return _6U(_76);});}},_77=[0,1420103680],_78=[0,465],_79=[1,_78,_k],_7a=[1,_77,_79],_7b=new T(function(){return B(_74(_35,_7a));}),_7c=[0,0],_7d=function(_7e,_7f){while(1){var _7g=E(_7e);if(!_7g[0]){var _7h=E(_7g[1]);if(_7h==(-2147483648)){_7e=[1,I_fromInt(-2147483648)];continue;}else{var _7i=E(_7f);if(!_7i[0]){return [0,_7h%_7i[1]];}else{_7e=[1,I_fromInt(_7h)];_7f=_7i;continue;}}}else{var _7j=_7g[1],_7k=E(_7f);return _7k[0]==0?[0,I_toInt(I_rem(_7j,I_fromInt(_7k[1])))]:[1,I_rem(_7j,_7k[1])];}}},_7l=function(_7m,_7n){return !B(_54(_7n,_7c))?B(_7d(_7m,_7n)):E(_53);},_7o=function(_7p,_7q){while(1){if(!B(_54(_7q,_7c))){var _7r=_7q,_7s=B(_7l(_7p,_7q));_7p=_7r;_7q=_7s;continue;}else{return E(_7p);}}},_7t=function(_7u){var _7v=E(_7u);if(!_7v[0]){var _7w=E(_7v[1]);return _7w==(-2147483648)?E(_6Z):_7w<0?[0, -_7w]:E(_7v);}else{var _7x=_7v[1];return I_compareInt(_7x,0)>=0?E(_7v):[1,I_negate(_7x)];}},_7y=function(_7z,_7A){while(1){var _7B=E(_7z);if(!_7B[0]){var _7C=E(_7B[1]);if(_7C==(-2147483648)){_7z=[1,I_fromInt(-2147483648)];continue;}else{var _7D=E(_7A);if(!_7D[0]){return [0,quot(_7C,_7D[1])];}else{_7z=[1,I_fromInt(_7C)];_7A=_7D;continue;}}}else{var _7E=_7B[1],_7F=E(_7A);return _7F[0]==0?[0,I_toInt(I_quot(_7E,I_fromInt(_7F[1])))]:[1,I_quot(_7E,_7F[1])];}}},_7G=5,_7H=new T(function(){return B(_50(_7G));}),_7I=new T(function(){return die(_7H);}),_7J=function(_7K,_7L){if(!B(_54(_7L,_7c))){var _7M=B(_7o(B(_7t(_7K)),B(_7t(_7L))));return !B(_54(_7M,_7c))?[0,B(_7y(_7K,_7M)),B(_7y(_7L,_7M))]:E(_53);}else{return E(_7I);}},_7N=[0,-1],_7O=function(_7P){var _7Q=E(_7P);if(!_7Q[0]){var _7R=_7Q[1];return _7R>=0?E(_7R)==0?E(_6I):E(_6X):E(_7N);}else{var _7S=I_compareInt(_7Q[1],0);return _7S<=0?E(_7S)==0?E(_6I):E(_7N):E(_6X);}},_7T=function(_7U,_7V,_7W,_7X){var _7Y=B(_65(_7V,_7W));return new F(function(){return _7J(B(_65(B(_65(_7U,_7X)),B(_7O(_7Y)))),B(_7t(_7Y)));});},_7Z=[0,0],_80=0,_81=new T(function(){return B(_50(_80));}),_82=new T(function(){return die(_81);}),_83=function(_84,_85){var _86=E(_85);if(!_86){return E(_53);}else{var _87=function(_88){if(_84<=0){if(_84>=0){var _89=quotRemI(_84,_86);return [0,[0,_89[1]],[0,_89[2]]];}else{if(_86<=0){var _8a=quotRemI(_84,_86);return [0,[0,_8a[1]],[0,_8a[2]]];}else{var _8b=quotRemI(_84+1|0,_86);return [0,[0,_8b[1]-1|0],[0,(_8b[2]+_86|0)-1|0]];}}}else{if(_86>=0){if(_84>=0){var _8c=quotRemI(_84,_86);return [0,[0,_8c[1]],[0,_8c[2]]];}else{if(_86<=0){var _8d=quotRemI(_84,_86);return [0,[0,_8d[1]],[0,_8d[2]]];}else{var _8e=quotRemI(_84+1|0,_86);return [0,[0,_8e[1]-1|0],[0,(_8e[2]+_86|0)-1|0]];}}}else{var _8f=quotRemI(_84-1|0,_86);return [0,[0,_8f[1]-1|0],[0,(_8f[2]+_86|0)+1|0]];}}};return E(_86)==(-1)?E(_84)==(-2147483648)?[0,_82,_7Z]:B(_87(_)):B(_87(_));}},_8g=function(_8h){var _8i=B(_83((_8h>>>0&2147483647>>>0)>>>0&4.294967295e9,2147483562));return [0,E(_8i[2])[1]+1|0,B(_5F(E(_8i[1])[1],2147483398))+1|0];},_8j=function(_8k){return E(_7b);},_8l=[0,1],_8m=function(_8n,_8o){var _8p=E(_8n);return [0,_8p,new T(function(){var _8q=B(_8m(B(_5U(_8p,_8o)),_8o));return [1,_8q[1],_8q[2]];})];},_8r=function(_8s){var _8t=B(_8m(_8s,_8l));return [1,_8t[1],_8t[2]];},_8u=function(_8v,_8w){var _8x=B(_8m(_8v,new T(function(){return B(_5w(_8w,_8v));})));return [1,_8x[1],_8x[2]];},_8y=[0,0],_8z=function(_8A,_8B){var _8C=E(_8A);if(!_8C[0]){var _8D=_8C[1],_8E=E(_8B);return _8E[0]==0?_8D<_8E[1]:I_compareInt(_8E[1],_8D)>0;}else{var _8F=_8C[1],_8G=E(_8B);return _8G[0]==0?I_compareInt(_8F,_8G[1])<0:I_compare(_8F,_8G[1])<0;}},_8H=function(_8I,_8J,_8K){if(!B(_5e(_8J,_8y))){var _8L=function(_8M){return !B(_8z(_8M,_8K))?[1,_8M,new T(function(){return B(_8L(B(_5U(_8M,_8J))));})]:[0];};return new F(function(){return _8L(_8I);});}else{var _8N=function(_8O){return !B(_5n(_8O,_8K))?[1,_8O,new T(function(){return B(_8N(B(_5U(_8O,_8J))));})]:[0];};return new F(function(){return _8N(_8I);});}},_8P=function(_8Q,_8R,_8S){return new F(function(){return _8H(_8Q,B(_5w(_8R,_8Q)),_8S);});},_8T=function(_8U,_8V){return new F(function(){return _8H(_8U,_8l,_8V);});},_8W=function(_8X){return [0,B(_3q(_8X))];},_8Y=function(_8Z){return new F(function(){return _5w(_8Z,_8l);});},_90=function(_91){return new F(function(){return _5U(_91,_8l);});},_92=function(_93){return new F(function(){return _63(E(_93)[1]);});},_94=[0,_90,_8Y,_92,_8W,_8r,_8u,_8T,_8P],_95=function(_96,_97){if(_96<=0){if(_96>=0){return new F(function(){return quot(_96,_97);});}else{if(_97<=0){return new F(function(){return quot(_96,_97);});}else{return quot(_96+1|0,_97)-1|0;}}}else{if(_97>=0){if(_96>=0){return new F(function(){return quot(_96,_97);});}else{if(_97<=0){return new F(function(){return quot(_96,_97);});}else{return quot(_96+1|0,_97)-1|0;}}}else{return quot(_96-1|0,_97)-1|0;}}},_98=function(_99,_9a){while(1){var _9b=E(_99);if(!_9b[0]){var _9c=E(_9b[1]);if(_9c==(-2147483648)){_99=[1,I_fromInt(-2147483648)];continue;}else{var _9d=E(_9a);if(!_9d[0]){return [0,B(_95(_9c,_9d[1]))];}else{_99=[1,I_fromInt(_9c)];_9a=_9d;continue;}}}else{var _9e=_9b[1],_9f=E(_9a);return _9f[0]==0?[0,I_toInt(I_div(_9e,I_fromInt(_9f[1])))]:[1,I_div(_9e,_9f[1])];}}},_9g=function(_9h,_9i){return !B(_54(_9i,_7c))?B(_98(_9h,_9i)):E(_53);},_9j=function(_9k,_9l){while(1){var _9m=E(_9k);if(!_9m[0]){var _9n=E(_9m[1]);if(_9n==(-2147483648)){_9k=[1,I_fromInt(-2147483648)];continue;}else{var _9o=E(_9l);if(!_9o[0]){var _9p=_9o[1];return [0,[0,B(_95(_9n,_9p))],[0,B(_5F(_9n,_9p))]];}else{_9k=[1,I_fromInt(_9n)];_9l=_9o;continue;}}}else{var _9q=E(_9l);if(!_9q[0]){_9k=_9m;_9l=[1,I_fromInt(_9q[1])];continue;}else{var _9r=I_divMod(_9m[1],_9q[1]);return [0,[1,_9r[1]],[1,_9r[2]]];}}}},_9s=function(_9t,_9u){if(!B(_54(_9u,_7c))){var _9v=B(_9j(_9t,_9u));return [0,_9v[1],_9v[2]];}else{return E(_53);}},_9w=function(_9x,_9y){return !B(_54(_9y,_7c))?B(_5M(_9x,_9y)):E(_53);},_9z=function(_9A,_9B){return !B(_54(_9B,_7c))?B(_7y(_9A,_9B)):E(_53);},_9C=function(_9D,_9E){while(1){var _9F=E(_9D);if(!_9F[0]){var _9G=E(_9F[1]);if(_9G==(-2147483648)){_9D=[1,I_fromInt(-2147483648)];continue;}else{var _9H=E(_9E);if(!_9H[0]){var _9I=_9H[1];return [0,[0,quot(_9G,_9I)],[0,_9G%_9I]];}else{_9D=[1,I_fromInt(_9G)];_9E=_9H;continue;}}}else{var _9J=E(_9E);if(!_9J[0]){_9D=_9F;_9E=[1,I_fromInt(_9J[1])];continue;}else{var _9K=I_quotRem(_9F[1],_9J[1]);return [0,[1,_9K[1]],[1,_9K[2]]];}}}},_9L=function(_9M,_9N){if(!B(_54(_9N,_7c))){var _9O=B(_9C(_9M,_9N));return [0,_9O[1],_9O[2]];}else{return E(_53);}},_9P=function(_9Q){return E(_9Q);},_9R=function(_9S){return E(_9S);},_9T=[0,_5U,_65,_5w,_70,_7t,_7O,_9R],_9U=function(_9V){return [0,E(E(_9V)),E(_6H)];},_9W=function(_9X,_9Y){var _9Z=E(_9X);if(!_9Z[0]){var _a0=_9Z[1],_a1=E(_9Y);return _a1[0]==0?_a0!=_a1[1]:I_compareInt(_a1[1],_a0)==0?false:true;}else{var _a2=_9Z[1],_a3=E(_9Y);return _a3[0]==0?I_compareInt(_a2,_a3[1])==0?false:true:I_compare(_a2,_a3[1])==0?false:true;}},_a4=[0,_54,_9W],_a5=function(_a6,_a7){var _a8=E(_a6);if(!_a8[0]){var _a9=_a8[1],_aa=E(_a7);return _aa[0]==0?_a9<=_aa[1]:I_compareInt(_aa[1],_a9)>=0;}else{var _ab=_a8[1],_ac=E(_a7);return _ac[0]==0?I_compareInt(_ab,_ac[1])<=0:I_compare(_ab,_ac[1])<=0;}},_ad=function(_ae,_af){return !B(_a5(_ae,_af))?E(_ae):E(_af);},_ag=function(_ah,_ai){return !B(_a5(_ah,_ai))?E(_ai):E(_ah);},_aj=function(_ak,_al){var _am=E(_ak);if(!_am[0]){var _an=_am[1],_ao=E(_al);if(!_ao[0]){var _ap=_ao[1];return _an!=_ap?_an>_ap?2:0:1;}else{var _aq=I_compareInt(_ao[1],_an);return _aq<=0?_aq>=0?1:2:0;}}else{var _ar=_am[1],_as=E(_al);if(!_as[0]){var _at=I_compareInt(_ar,_as[1]);return _at>=0?_at<=0?1:2:0;}else{var _au=I_compare(_ar,_as[1]);return _au>=0?_au<=0?1:2:0;}}},_av=[0,_a4,_aj,_8z,_5e,_5n,_a5,_ad,_ag],_aw=[0,_9T,_av,_9U],_ax=[0,_aw,_94,_9z,_7l,_9g,_9w,_9L,_9s,_9P],_ay=[0,0],_az=function(_aA,_aB,_aC){var _aD=B(A(_aA,[_aB]));if(!B(_54(_aD,_ay))){return new F(function(){return _98(B(_65(_aB,_aC)),_aD);});}else{return E(_53);}},_aE=function(_aF){return E(E(_aF)[1]);},_aG=function(_aH){return E(E(_aH)[1]);},_aI=function(_aJ,_aK,_aL){var _aM=new T(function(){if(!B(_54(_aL,_7c))){var _aN=B(_9C(_aK,_aL)),_aO=[0,_aN[1],_aN[2]];}else{var _aO=E(_53);}return _aO;});return [0,new T(function(){return B(A(_5c,[B(_aG(B(_aE(_aJ)))),new T(function(){return E(E(_aM)[1]);})]));}),new T(function(){return [0,E(E(E(_aM)[2])),E(_aL)];})];},_aP=function(_aQ,_aR,_aS){var _aT=B(_aI(_aQ,_aR,_aS)),_aU=_aT[1],_aV=E(_aT[2]);if(!B(_8z(B(_65(_aV[1],_6H)),B(_65(_7c,_aV[2]))))){return E(_aU);}else{var _aW=E(B(_aE(_aQ))[1]);return new F(function(){return A(_aW[3],[_aU,new T(function(){return B(A(_aW[7],[_6H]));})]);});}},_aX=[0,479723520],_aY=[0,40233135],_aZ=[1,_aY,_k],_b0=[1,_aX,_aZ],_b1=new T(function(){return B(_74(_35,_b0));}),_b2=[0,40587],_b3=function(_b4){var _b5=new T(function(){var _b6=B(_7T(E(_b4),_6H,_7b,_6H)),_b7=B(_7T(_b1,_6H,_7b,_6H)),_b8=B(_7T(_b6[1],_b6[2],_b7[1],_b7[2]));return B(_aP(_ax,_b8[1],_b8[2]));});return [0,new T(function(){return B(_5U(_b2,_b5));}),new T(function(){return B(_5w(_b4,B(_az(_8j,B(_65(_b5,_7b)),_b1))));})];},_b9=[0,0],_ba=function(_bb,_bc,_){var _=writeOffAddr("w32",4,E(_bb)[1],0,E(_bc)[1]);return _30;},_bd=function(_be,_){var _bf=readOffAddr("w32",4,E(_be)[1],0),_bg=_bf;return [0,_bg];},_bh=function(_bi,_bj,_bk,_){var _=writeOffAddr("w32",4,plusAddr(E(_bi)[1],E(_bj)[1]),0,E(_bk)[1]);return _30;},_bl=function(_bm,_bn,_){var _bo=readOffAddr("w32",4,plusAddr(E(_bm)[1],E(_bn)[1]),0),_bp=_bo;return [0,_bp];},_bq=[0,4],_br=function(_bs){return E(_bq);},_bt=function(_bu,_bv,_){var _bw=readOffAddr("w32",4,E(_bu)[1],E(_bv)[1]),_bx=_bw;return [0,_bx];},_by=function(_bz,_bA,_bB,_){var _=writeOffAddr("w32",4,E(_bz)[1],E(_bA)[1],E(_bB)[1]);return _30;},_bC=[0,_br,_br,_bt,_by,_bl,_bh,_bd,_ba],_bD=[0,0],_bE=function(_bF){return E(E(_bF)[3]);},_bG=function(_bH,_bI,_bJ,_){if(_bI>0){return new F(function(){return (function(_bK,_bL,_){while(1){var _bM=E(_bK);if(!_bM){var _bN=B(A(new T(function(){return B(A(_bE,[_bH,_bJ,_bD]));}),[_])),_bO=_bN;return [1,_bO,_bL];}else{var _bP=B(A(new T(function(){return B(_bE(_bH));}),[_bJ,[0,_bM],_])),_bQ=_bP;_bK=_bM-1|0;var _bR=[1,_bQ,_bL];_bL=_bR;continue;}}})(_bI-1|0,_k,_);});}else{return _k;}},_bS=0,_bT=1,_bU=function(_bV,_bW,_bX,_){var _bY=0,_bZ=_bY;switch(E(_bZ)){case 0:return new F(function(){return (function(_){var _c0=B(A(_bV,[_])),_c1=_c0,_c2=jsCatch(function(_){return new F(function(){return new T(function(){return B(A(_bX,[_c1]));})();});},function(_c3,_){var _c4=B(A(_bW,[_c1,_])),_c5=_c4;return new F(function(){return die(_c3);});}),_c6=_c2,_c7=B(A(_bW,[_c1,_])),_c8=_c7;return _c6;})();});break;case 1:var _c9=B(A(_bV,[_])),_ca=_c9,_cb=jsCatch(new T(function(){return B(A(_bX,[_ca]));}),function(_cc,_){var _cd=B(A(_bW,[_ca,_])),_ce=_cd;return new F(function(){return die(_cc);});}),_cf=_cb,_cg=B(A(_bW,[_ca,_])),_ch=_cg;return _cf;default:var _ci=B(A(_bV,[_])),_cj=_ci,_ck=jsCatch(new T(function(){return B(A(_bX,[_cj]));}),function(_cl,_){var _cm=B(A(_bW,[_cj,_])),_cn=_cm;return new F(function(){return die(_cl);});}),_co=_ck,_cp=B(A(_bW,[_cj,_])),_cq=_cp;return _co;}},_cr=function(_cs){return E(E(_cs)[3]);},_ct=0,_cu=[0,_ct,_k],_cv=new T(function(){return B(unCStr("mallocForeignPtrBytes: size must be >= 0"));}),_cw=new T(function(){return B(err(_cv));}),_cx=function(_cy,_cz,_){var _cA=B((function(_cB,_){while(1){var _cC=readOffAddr("i8",1,_cz,_cB),_cD=_cC;if(!E(_cD)){return [0,_cB];}else{var _cE=_cB+1|0;_cB=_cE;continue;}}})(0,_)),_cF=_cA;return new F(function(){return _bU(E(_cy)[2],_cr,function(_cG,_){var _cH=nMV(_cu),_cI=_cH,_cJ=E(_cF)[1],_cK=function(_cL){var _cM=imul(_cL,4)|0;if(_cM>=0){var _cN=nMV(_cu),_cO=_cN,_cP=newByteArr(_cM),_cQ=_cP,_cR=function(_cS,_){var _cT=E(_cG),_cU=B(A(_cT[1],[_cS,[0,_cQ,[1,_cQ,_cO],_bT,_cL,0,0],_])),_cV=_cU,_cW=E(_cV),_cX=_cW[3],_cY=E(_cW[2]);if(_cY[5]!=_cY[6]){if(E(_cW[1])==1){var _cZ=E(_cX),_d0=_cZ[2],_d1=B(_bG(_bC,_cZ[6]-_cZ[5]|0,[0,_cZ[1]],_)),_d2=_d1,_=0,_d3=B(_cR(_cY,_)),_d4=_d3;return new T(function(){return B(_u(_d2,_d4));});}else{var _d5=B(A(_cT[2],[_cY,_cX,_])),_d6=_d5,_d7=E(_d6),_d8=E(_d7[2]),_d9=_d8[2],_da=B(_bG(_bC,_d8[6]-_d8[5]|0,[0,_d8[1]],_)),_db=_da,_=0,_dc=B(_cR(_d7[1],_)),_dd=_dc;return new T(function(){return B(_u(_db,_dd));});}}else{var _de=E(_cX),_df=_de[2],_dg=B(_bG(_bC,_de[6]-_de[5]|0,[0,_de[1]],_)),_dh=_dg,_=0;return _dh;}};return new F(function(){return _cR([0,_cz,[0,_cI],_bS,_cJ,0,_cJ],_);});}else{return E(_cw);}};return _cJ>1?B(_cK(_cJ)):B(_cK(1));},_);});},_di=1,_dj=function(_dk,_dl){while(1){var _dm=E(_dk);if(!_dm[0]){return E(_dl)[0]==0?true:false;}else{var _dn=E(_dl);if(!_dn[0]){return false;}else{if(E(_dm[1])[1]!=E(_dn[1])[1]){return false;}else{_dk=_dm[2];_dl=_dn[2];continue;}}}}},_do=new T(function(){return B(unCStr("UTF16LE"));}),_dp=new T(function(){return B(unCStr("UTF16BE"));}),_dq=new T(function(){return B(unCStr("UTF16"));}),_dr=new T(function(){return B(unCStr("UTF8"));}),_ds=new T(function(){return B(unCStr("UTF32LE"));}),_dt=new T(function(){return B(unCStr("UTF32BE"));}),_du=new T(function(){return B(unCStr("UTF32"));}),_dv=function(_dw){return new F(function(){return err(B(unAppCStr("Prelude.chr: bad argument: ",new T(function(){return B(_F(9,_dw,_k));}))));});},_dx=function(_dy){var _dz=u_towupper(_dy),_dA=_dz;return _dA>>>0>1114111?B(_dv(_dA)):_dA;},_dB=function(_dC){while(1){var _dD=(function(_dE){var _dF=E(_dE);if(!_dF[0]){return [0];}else{var _dG=_dF[2],_dH=E(E(_dF[1])[1]);if(_dH==45){_dC=_dG;return null;}else{return [1,new T(function(){return [0,B(_dx(_dH))];}),new T(function(){return B(_dB(_dG));})];}}})(_dC);if(_dD!=null){return _dD;}}},_dI=function(_dJ,_dK){var _dL=E(_dK);if(!_dL[0]){return [0,_k,_k];}else{var _dM=_dL[1];if(!B(A(_dJ,[_dM]))){return [0,_k,_dL];}else{var _dN=new T(function(){var _dO=B(_dI(_dJ,_dL[2]));return [0,_dO[1],_dO[2]];});return [0,[1,_dM,new T(function(){return E(E(_dN)[1]);})],new T(function(){return E(E(_dN)[2]);})];}}},_dP=new T(function(){return B(unCStr("UTF-32LE"));}),_dQ=0,_dR=[0],_dS=1,_dT=new T(function(){return [0, -(1&4.294967295e9)>>>0];}),_dU=[0,0],_dV=new T(function(){return B(unCStr("base"));}),_dW=new T(function(){return B(unCStr("GHC.IO.Exception"));}),_dX=new T(function(){return B(unCStr("IOException"));}),_dY=[0,I_fromBits([4053623282,1685460941]),I_fromBits([3693590983,2507416641]),_dV,_dW,_dX],_dZ=[0,I_fromBits([4053623282,1685460941]),I_fromBits([3693590983,2507416641]),_dY,_k],_e0=function(_e1){return E(_dZ);},_e2=function(_e3){var _e4=E(_e3);return new F(function(){return _4d(B(_4b(_e4[1])),_e0,_e4[2]);});},_e5=new T(function(){return B(unCStr(": "));}),_e6=[0,41],_e7=new T(function(){return B(unCStr(" ("));}),_e8=new T(function(){return B(unCStr("already exists"));}),_e9=new T(function(){return B(unCStr("does not exist"));}),_ea=new T(function(){return B(unCStr("protocol error"));}),_eb=new T(function(){return B(unCStr("failed"));}),_ec=new T(function(){return B(unCStr("invalid argument"));}),_ed=new T(function(){return B(unCStr("inappropriate type"));}),_ee=new T(function(){return B(unCStr("hardware fault"));}),_ef=new T(function(){return B(unCStr("unsupported operation"));}),_eg=new T(function(){return B(unCStr("timeout"));}),_eh=new T(function(){return B(unCStr("resource vanished"));}),_ei=new T(function(){return B(unCStr("interrupted"));}),_ej=new T(function(){return B(unCStr("resource busy"));}),_ek=new T(function(){return B(unCStr("resource exhausted"));}),_el=new T(function(){return B(unCStr("end of file"));}),_em=new T(function(){return B(unCStr("illegal operation"));}),_en=new T(function(){return B(unCStr("permission denied"));}),_eo=new T(function(){return B(unCStr("user error"));}),_ep=new T(function(){return B(unCStr("unsatisified constraints"));}),_eq=new T(function(){return B(unCStr("system error"));}),_er=function(_es,_et){switch(E(_es)){case 0:return new F(function(){return _u(_e8,_et);});break;case 1:return new F(function(){return _u(_e9,_et);});break;case 2:return new F(function(){return _u(_ej,_et);});break;case 3:return new F(function(){return _u(_ek,_et);});break;case 4:return new F(function(){return _u(_el,_et);});break;case 5:return new F(function(){return _u(_em,_et);});break;case 6:return new F(function(){return _u(_en,_et);});break;case 7:return new F(function(){return _u(_eo,_et);});break;case 8:return new F(function(){return _u(_ep,_et);});break;case 9:return new F(function(){return _u(_eq,_et);});break;case 10:return new F(function(){return _u(_ea,_et);});break;case 11:return new F(function(){return _u(_eb,_et);});break;case 12:return new F(function(){return _u(_ec,_et);});break;case 13:return new F(function(){return _u(_ed,_et);});break;case 14:return new F(function(){return _u(_ee,_et);});break;case 15:return new F(function(){return _u(_ef,_et);});break;case 16:return new F(function(){return _u(_eg,_et);});break;case 17:return new F(function(){return _u(_eh,_et);});break;default:return new F(function(){return _u(_ei,_et);});}},_eu=[0,125],_ev=new T(function(){return B(unCStr("{handle: "));}),_ew=function(_ex,_ey,_ez,_eA,_eB,_eC){var _eD=new T(function(){var _eE=new T(function(){return B(_er(_ey,new T(function(){var _eF=E(_eA);return _eF[0]==0?E(_eC):B(_u(_e7,new T(function(){return B(_u(_eF,[1,_e6,_eC]));},1)));},1)));},1),_eG=E(_ez);return _eG[0]==0?E(_eE):B(_u(_eG,new T(function(){return B(_u(_e5,_eE));},1)));},1),_eH=E(_eB);if(!_eH[0]){var _eI=E(_ex);if(!_eI[0]){return E(_eD);}else{var _eJ=E(_eI[1]);return _eJ[0]==0?B(_u(_ev,new T(function(){return B(_u(_eJ[1],[1,_eu,new T(function(){return B(_u(_e5,_eD));})]));},1))):B(_u(_ev,new T(function(){return B(_u(_eJ[1],[1,_eu,new T(function(){return B(_u(_e5,_eD));})]));},1)));}}else{return new F(function(){return _u(_eH[1],new T(function(){return B(_u(_e5,_eD));},1));});}},_eK=function(_eL){var _eM=E(_eL);return new F(function(){return _ew(_eM[1],_eM[2],_eM[3],_eM[4],_eM[6],_k);});},_eN=function(_eO,_eP){var _eQ=E(_eO);return new F(function(){return _ew(_eQ[1],_eQ[2],_eQ[3],_eQ[4],_eQ[6],_eP);});},_eR=function(_eS,_eT){return new F(function(){return _4K(_eN,_eS,_eT);});},_eU=function(_eV,_eW,_eX){var _eY=E(_eW);return new F(function(){return _ew(_eY[1],_eY[2],_eY[3],_eY[4],_eY[6],_eX);});},_eZ=[0,_eU,_eK,_eR],_f0=new T(function(){return [0,_e0,_eZ,_f1,_e2];}),_f1=function(_f2){return [0,_f0,_f2];},_f3=function(_f4,_){return new F(function(){return die(new T(function(){return B(_f1(_f4));}));});},_f5=function(_f6,_){return new F(function(){return _f3(_f6,_);});},_f7=new T(function(){return B(unCStr("iconvRecoder"));}),_f8=[0,-1],_f9=function(_fa,_fb,_fc,_fd,_fe,_ff,_fg,_fh,_fi,_fj,_fk,_fl,_fm,_fn,_fo,_){var _fp=newByteArr(4),_fq=_fp,_fr=_fq,_fs=_fr,_ft=E(_fh)[1],_fu=function(_fv){var _fw=plusAddr(_fb,_fv),_=die("Unsupported PrimOp: writeAddrOffAddr#"),_fx=newByteArr(4),_fy=_fx,_fz=_fy,_fA=_fz,_fB=E(_fo)[1],_fC=function(_fD){var _fE=plusAddr(_fi,_fD),_=die("Unsupported PrimOp: writeAddrOffAddr#"),_fF=newByteArr(4),_fG=_fF,_fH=_fG,_fI=_fH,_fJ=function(_fK){var _fL=_fI,_=writeOffAddr("w32",4,_fL,0,_fK),_fM=newByteArr(4),_fN=_fM,_fO=_fN,_fP=_fO,_fQ=function(_fR){var _fS=_fP,_=writeOffAddr("w32",4,_fS,0,_fR),_fT=hs_iconv(E(_fa)[1],_fs,_fL,_fA,_fS),_fU=_fT,_fV=readOffAddr("w32",4,_fL,0),_fW=_fV,_fX=readOffAddr("w32",4,_fS,0),_fY=_fX,_fZ=new T(function(){if(_fB<32){var _g0=[0,(_fY&4.294967295e9)>>_fB];}else{var _g0=(_fY&4.294967295e9)>=0?E(_dU):E(_f8);}var _g1=_g0;return _g1;}),_g2=new T(function(){var _g3=E(_fW);if(!_g3){var _g4=[0,_fb,_fc,_fd,_fe,0,0];}else{if(_ft<32){var _g5=[0,_fb,_fc,_fd,_fe,_fg-((_g3&4.294967295e9)>>_ft)|0,_fg];}else{if((_g3&4.294967295e9)>=0){var _g6=[0,_fb,_fc,_fd,_fe,_fg,_fg];}else{var _g6=[0,_fb,_fc,_fd,_fe,_fg+1|0,_fg];}var _g7=_g6,_g8=_g7,_g5=_g8;}var _g9=_g5,_g4=_g9;}return _g4;});if(_fU!=E(_dT)[1]){var _=0,_=0,_=0,_=0,_=0,_=0;return [0,_dQ,_g2,new T(function(){return [0,_fi,_fj,_fk,_fl,_fm,_fl-E(_fZ)[1]|0];})];}else{var _ga=__hscore_get_errno(),_gb=_ga;switch(E(_gb)){case 7:var _=0,_=0,_=0,_=0,_=0,_=0;return [0,_dS,_g2,new T(function(){return [0,_fi,_fj,_fk,_fl,_fm,_fl-E(_fZ)[1]|0];})];case 22:var _=0,_=0,_=0,_=0,_=0,_=0;return [0,_dQ,_g2,new T(function(){return [0,_fi,_fj,_fk,_fl,_fm,_fl-E(_fZ)[1]|0];})];case 84:var _=0,_=0,_=0,_=0,_=0,_=0;return [0,new T(function(){return E(E(_fZ)[1])==0?1:2;}),_g2,new T(function(){return [0,_fi,_fj,_fk,_fl,_fm,_fl-E(_fZ)[1]|0];})];default:var _gc=__hscore_get_errno(),_gd=_gc;return new F(function(){return _f5(B(_ge(_f7,_gd,_dR,_dR)),_);});}}};if(_fB<32){return new F(function(){return _fQ((_fl-_fn|0)<<_fB>>>0);});}else{return new F(function(){return _fQ(0);});}};if(_ft<32){return new F(function(){return _fJ((_fg-_ff|0)<<_ft>>>0);});}else{return new F(function(){return _fJ(0);});}};if(_fB<32){return new F(function(){return _fC(_fn<<_fB);});}else{return new F(function(){return _fC(0);});}};if(_ft<32){return new F(function(){return _fu(_ff<<_ft);});}else{return new F(function(){return _fu(0);});}},_gf=[0,2],_gg=function(_gh,_gi,_gj,_){var _gk=E(_gi),_gl=E(_gj);return new F(function(){return _f9(_gh,_gk[1],_gk[2],_gk[3],_gk[4],_gk[5],_gk[6],_gf,_gl[1],_gl[2],_gl[3],_gl[4],_gl[5],_gl[6],_dU,_);});},_gm=function(_gn,_go,_gp,_){var _gq=E(_go),_gr=E(_gp);return new F(function(){return _f9(_gn,_gq[1],_gq[2],_gq[3],_gq[4],_gq[5],_gq[6],_dU,_gr[1],_gr[2],_gr[3],_gr[4],_gr[5],_gr[6],_gf,_);});},_gs=function(_gt){return E(E(_gt)[1])==47?false:true;},_gu=function(_gv,_){return _30;},_gw=function(_){return _30;},_gx=new T(function(){return B(unCStr("mkTextEncoding"));}),_gy=new T(function(){return B(unCStr("Iconv.close"));}),_gz=function(_gA,_gB,_){var _gC=newByteArr(B(_23(_gA,0))+1|0),_gD=_gC,_gE=_gD,_gF=_gE,_gG=_gF,_gH=B((function(_gI,_gJ,_){while(1){var _gK=E(_gI);if(!_gK[0]){var _=writeOffAddr("i8",1,_gG,_gJ,0);return _30;}else{var _=writeOffAddr("i8",1,_gG,_gJ,E(_gK[1])[1]&255);_gI=_gK[2];var _gL=_gJ+1|0;_gJ=_gL;continue;}}})(_gA,0,_)),_gM=_gH,_gN=B(A(_gB,[[0,_gG],_])),_gO=_gN,_=0;return _gO;},_gP=function(_gQ,_gR,_){return new F(function(){return _gz(_gQ,_gR,_);});},_gS=function(_gT,_gU,_gV,_gW){return new F(function(){return _gP(_gT,function(_gX){return new F(function(){return _gP(_gU,function(_gY,_){var _gZ=hs_iconv_open(E(_gY)[1],E(_gX)[1]),_h0=_gZ,_h1=E(_h0);if(_h1==(-1)){var _h2=__hscore_get_errno(),_h3=_h2;return new F(function(){return _f5(B(_ge(_gx,_h3,_dR,_dR)),_);});}else{return [0,new T(function(){return B(A(_gW,[[0,_h1]]));}),_gV,function(_){var _h4=hs_iconv_close(_h1),_h5=_h4;if(E(_h5)==(-1)){var _h6=__hscore_get_errno(),_h7=_h6;return new F(function(){return _f5(B(_ge(_gy,_h7,_dR,_dR)),_);});}else{return _30;}},_gw,_gu];}});});});});},_h8=function(_f6,_){return new F(function(){return _f3(_f6,_);});},_h9=12,_ha=new T(function(){return B(unCStr("invalid byte sequence"));}),_hb=new T(function(){return B(unCStr("recoverDecode"));}),_hc=[0,_dR,_h9,_hb,_ha,_dR,_dR],_hd=function(_he,_hf,_hg,_hh,_hi,_hj,_hk,_hl,_hm,_hn,_ho,_hp,_hq,_){switch(E(_he)){case 0:return new F(function(){return _h8(_hc,_);});break;case 1:return [0,[0,_hf,_hg,_hh,_hi,_hj+1|0,_hk],[0,_hl,_hm,_hn,_ho,_hp,_hq]];case 2:var _=writeOffAddr("w32",4,_hl,_hq,65533),_=0;return [0,[0,_hf,_hg,_hh,_hi,_hj+1|0,_hk],[0,_hl,_hm,_hn,_ho,_hp,_hq+1|0]];default:var _hr=readOffAddr("w8",1,plusAddr(_hf,_hj),0),_hs=_hr,_=0;if(_hs>=128){var _ht=56320+(_hs&4.294967295e9)|0;if(_ht>>>0>1114111){return new F(function(){return _dv(_ht);});}else{var _=writeOffAddr("w32",4,_hl,_hq,_ht),_=0;return [0,[0,_hf,_hg,_hh,_hi,_hj+1|0,_hk],[0,_hl,_hm,_hn,_ho,_hp,_hq+1|0]];}}else{var _hu=_hs&4.294967295e9;if(_hu>>>0>1114111){return new F(function(){return _dv(_hu);});}else{var _=writeOffAddr("w32",4,_hl,_hq,_hu),_=0;return [0,[0,_hf,_hg,_hh,_hi,_hj+1|0,_hk],[0,_hl,_hm,_hn,_ho,_hp,_hq+1|0]];}}}},_hv=function(_hw,_hx,_hy,_){var _hz=E(_hx),_hA=E(_hy);return new F(function(){return _hd(_hw,_hz[1],_hz[2],_hz[3],_hz[4],_hz[5],_hz[6],_hA[1],_hA[2],_hA[3],_hA[4],_hA[5],_hA[6],_);});},_hB=new T(function(){return B(unCStr("recoverEncode"));}),_hC=new T(function(){return B(unCStr("invalid character"));}),_hD=[0,_dR,_h9,_hB,_hC,_dR,_dR],_hE=function(_){return new F(function(){return _h8(_hD,_);});},_hF=function(_hG,_hH,_hI,_hJ,_hK,_hL,_hM,_hN,_hO,_hP,_hQ,_hR,_hS,_){var _hT=readOffAddr("w32",4,_hH,_hL),_hU=_hT,_=0;switch(E(_hG)){case 0:return new F(function(){return _hE(_);});break;case 1:return [0,[0,_hH,_hI,_hJ,_hK,_hL+1|0,_hM],[0,_hN,_hO,_hP,_hQ,_hR,_hS]];case 2:if(E(_hU)==63){return [0,[0,_hH,_hI,_hJ,_hK,_hL+1|0,_hM],[0,_hN,_hO,_hP,_hQ,_hR,_hS]];}else{var _=writeOffAddr("w32",4,_hH,_hL,63),_=0;return [0,[0,_hH,_hI,_hJ,_hK,_hL,_hM],[0,_hN,_hO,_hP,_hQ,_hR,_hS]];}break;default:var _hV=_hU;if(56448>_hV){return new F(function(){return _hE(_);});}else{if(_hV>=56576){return new F(function(){return _hE(_);});}else{var _=writeOffAddr("w8",1,plusAddr(_hN,_hS),0,_hV>>>0&255),_=0;return [0,[0,_hH,_hI,_hJ,_hK,_hL+1|0,_hM],[0,_hN,_hO,_hP,_hQ,_hR,_hS+1|0]];}}}},_hW=function(_hX,_hY,_hZ,_){var _i0=E(_hY),_i1=E(_hZ);return new F(function(){return _hF(_hX,_i0[1],_i0[2],_i0[3],_i0[4],_i0[5],_i0[6],_i1[1],_i1[2],_i1[3],_i1[4],_i1[5],_i1[6],_);});},_i2=function(_i3,_i4,_){return [0,_i4,new T(function(){var _i5=new T(function(){var _i6=B(_dI(_gs,_i4));return [0,_i6[1],_i6[2]];});return B(_gS(new T(function(){return E(E(_i5)[1]);}),new T(function(){return B(_u(_dP,new T(function(){return E(E(_i5)[2]);},1)));}),function(_i7,_i8,_){return new F(function(){return _hv(_i3,_i7,_i8,_);});},_gm));}),new T(function(){return B(_gS(_dP,_i4,function(_i7,_i8,_){return new F(function(){return _hW(_i3,_i7,_i8,_);});},_gg));})];},_i9=2,_ia=function(_ib,_ic,_id,_ie,_if,_ig,_ih,_ii,_ij,_ik,_il,_im,_){var _in=[0,_ib,_ic,_id,_ie,0,0],_io=function(_ip,_iq,_){while(1){var _ir=(function(_is,_it,_){if(_is<_ig){if((_ik-_it|0)>=2){var _iu=readOffAddr("w32",4,_ib,_is),_iv=_iu,_=0,_iw=_iv;if(_iw>=65536){if((_ik-_it|0)>=4){var _ix=_iw-65536|0,_=writeOffAddr("w8",1,plusAddr(_ih,_it),0,((_ix>>18)+216|0)>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_ih,_it+1|0),0,_ix>>10>>>0&255),_=0,_iy=(_ix>>>0&1023>>>0)>>>0&4.294967295e9,_=writeOffAddr("w8",1,plusAddr(_ih,_it+2|0),0,((_iy>>8)+220|0)>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_ih,_it+3|0),0,_iy>>>0&255),_=0,_iz=_is+1|0,_iA=_it+4|0;_ip=_iz;_iq=_iA;return null;}else{return [0,_dS,new T(function(){return _is!=_ig?[0,_ib,_ic,_id,_ie,_is,_ig]:E(_in);}),[0,_ih,_ii,_ij,_ik,_il,_it]];}}else{var _iB=function(_iC){if(56320>_iw){var _=writeOffAddr("w8",1,plusAddr(_ih,_it),0,_iw>>8>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_ih,_it+1|0),0,_iw>>>0&255),_=0;return new F(function(){return _io(_is+1|0,_it+2|0,_);});}else{if(_iw>57343){var _=writeOffAddr("w8",1,plusAddr(_ih,_it),0,_iw>>8>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_ih,_it+1|0),0,_iw>>>0&255),_=0;return new F(function(){return _io(_is+1|0,_it+2|0,_);});}else{return [0,_i9,new T(function(){return _is!=_ig?[0,_ib,_ic,_id,_ie,_is,_ig]:E(_in);}),[0,_ih,_ii,_ij,_ik,_il,_it]];}}};if(55296>_iw){return new F(function(){return _iB(_);});}else{return _iw>56319?B(_iB(_)):[0,_i9,new T(function(){return _is!=_ig?[0,_ib,_ic,_id,_ie,_is,_ig]:E(_in);}),[0,_ih,_ii,_ij,_ik,_il,_it]];}}}else{return [0,_dS,new T(function(){return _is!=_ig?[0,_ib,_ic,_id,_ie,_is,_ig]:E(_in);}),[0,_ih,_ii,_ij,_ik,_il,_it]];}}else{return [0,_dQ,new T(function(){return _is!=_ig?[0,_ib,_ic,_id,_ie,_is,_ig]:E(_in);}),[0,_ih,_ii,_ij,_ik,_il,_it]];}})(_ip,_iq,_);if(_ir!=null){return _ir;}}};return new F(function(){return _io(_if,_im,_);});},_iD=function(_iE,_iF,_iG,_iH,_iI,_iJ,_iK,_iL,_){var _iM=rMV(_iE),_iN=_iM;if(!E(_iN)){if((_iJ-_iL|0)>=2){var _=wMV(_iE,_35),_=writeOffAddr("w8",1,plusAddr(_iG,_iL),0,254),_=0,_=writeOffAddr("w8",1,plusAddr(_iG,_iL+1|0),0,255),_=0,_iO=E(_iF);return new F(function(){return _ia(_iO[1],_iO[2],_iO[3],_iO[4],_iO[5],_iO[6],_iG,_iH,_iI,_iJ,_iK,_iL+2|0,_);});}else{return [0,_dS,_iF,[0,_iG,_iH,_iI,_iJ,_iK,_iL]];}}else{var _iP=E(_iF);return new F(function(){return _ia(_iP[1],_iP[2],_iP[3],_iP[4],_iP[5],_iP[6],_iG,_iH,_iI,_iJ,_iK,_iL,_);});}},_iQ=function(_iR,_iS,_iT,_iU,_iV,_iW,_iX,_iY,_iZ,_j0,_j1,_j2,_){var _j3=[0,_iR,_iS,_iT,_iU,0,0];return new F(function(){return (function(_j4,_j5,_){while(1){var _j6=(function(_j7,_j8,_){if(_j8<_j0){if(_j7<_iW){if((_j7+1|0)!=_iW){var _j9=readOffAddr("w8",1,plusAddr(_iR,_j7),0),_ja=_j9,_=0,_jb=readOffAddr("w8",1,plusAddr(_iR,_j7+1|0),0),_jc=_jb,_=0,_jd=(_ja<<8>>>0&65535)+_jc>>>0&65535;if(_jd>=55296){if(_jd<=57343){if((_iW-_j7|0)>=4){var _je=readOffAddr("w8",1,plusAddr(_iR,_j7+2|0),0),_jf=_je,_=0,_jg=readOffAddr("w8",1,plusAddr(_iR,_j7+3|0),0),_jh=_jg,_=0;if(_jd<55296){return [0,_i9,new T(function(){return _j7!=_iW?[0,_iR,_iS,_iT,_iU,_j7,_iW]:E(_j3);}),[0,_iX,_iY,_iZ,_j0,_j1,_j8]];}else{if(_jd>56319){return [0,_i9,new T(function(){return _j7!=_iW?[0,_iR,_iS,_iT,_iU,_j7,_iW]:E(_j3);}),[0,_iX,_iY,_iZ,_j0,_j1,_j8]];}else{var _ji=(_jf<<8>>>0&65535)+_jh>>>0&65535;if(_ji<56320){return [0,_i9,new T(function(){return _j7!=_iW?[0,_iR,_iS,_iT,_iU,_j7,_iW]:E(_j3);}),[0,_iX,_iY,_iZ,_j0,_j1,_j8]];}else{if(_ji>57343){return [0,_i9,new T(function(){return _j7!=_iW?[0,_iR,_iS,_iT,_iU,_j7,_iW]:E(_j3);}),[0,_iX,_iY,_iZ,_j0,_j1,_j8]];}else{var _=writeOffAddr("w32",4,_iX,_j8,((((_jd&4.294967295e9)-55296|0)<<10)+((_ji&4.294967295e9)-56320|0)|0)+65536|0),_=0,_jj=_j7+4|0,_jk=_j8+1|0;_j4=_jj;_j5=_jk;return null;}}}}}else{return [0,_dQ,new T(function(){return _j7!=_iW?[0,_iR,_iS,_iT,_iU,_j7,_iW]:E(_j3);}),[0,_iX,_iY,_iZ,_j0,_j1,_j8]];}}else{var _=writeOffAddr("w32",4,_iX,_j8,_jd&4.294967295e9),_=0,_jj=_j7+2|0,_jk=_j8+1|0;_j4=_jj;_j5=_jk;return null;}}else{var _=writeOffAddr("w32",4,_iX,_j8,_jd&4.294967295e9),_=0,_jj=_j7+2|0,_jk=_j8+1|0;_j4=_jj;_j5=_jk;return null;}}else{return [0,_dQ,new T(function(){return _j7!=_iW?[0,_iR,_iS,_iT,_iU,_j7,_iW]:E(_j3);}),[0,_iX,_iY,_iZ,_j0,_j1,_j8]];}}else{return [0,_dQ,new T(function(){return _j7!=_iW?[0,_iR,_iS,_iT,_iU,_j7,_iW]:E(_j3);}),[0,_iX,_iY,_iZ,_j0,_j1,_j8]];}}else{return [0,_dS,new T(function(){return _j7!=_iW?[0,_iR,_iS,_iT,_iU,_j7,_iW]:E(_j3);}),[0,_iX,_iY,_iZ,_j0,_j1,_j8]];}})(_j4,_j5,_);if(_j6!=null){return _j6;}}})(_iV,_j2,_);});},_jl=function(_jm,_jn,_jo,_jp,_jq,_jr,_js,_jt,_ju,_jv,_jw,_jx,_){var _jy=[0,_jm,_jn,_jo,_jp,0,0];return new F(function(){return (function(_jz,_jA,_){while(1){var _jB=(function(_jC,_jD,_){if(_jD<_jv){if(_jC<_jr){if((_jC+1|0)!=_jr){var _jE=readOffAddr("w8",1,plusAddr(_jm,_jC),0),_jF=_jE,_=0,_jG=readOffAddr("w8",1,plusAddr(_jm,_jC+1|0),0),_jH=_jG,_=0,_jI=(_jH<<8>>>0&65535)+_jF>>>0&65535;if(_jI>=55296){if(_jI<=57343){if((_jr-_jC|0)>=4){var _jJ=readOffAddr("w8",1,plusAddr(_jm,_jC+2|0),0),_jK=_jJ,_=0,_jL=readOffAddr("w8",1,plusAddr(_jm,_jC+3|0),0),_jM=_jL,_=0;if(_jI<55296){return [0,_i9,new T(function(){return _jC!=_jr?[0,_jm,_jn,_jo,_jp,_jC,_jr]:E(_jy);}),[0,_js,_jt,_ju,_jv,_jw,_jD]];}else{if(_jI>56319){return [0,_i9,new T(function(){return _jC!=_jr?[0,_jm,_jn,_jo,_jp,_jC,_jr]:E(_jy);}),[0,_js,_jt,_ju,_jv,_jw,_jD]];}else{var _jN=(_jM<<8>>>0&65535)+_jK>>>0&65535;if(_jN<56320){return [0,_i9,new T(function(){return _jC!=_jr?[0,_jm,_jn,_jo,_jp,_jC,_jr]:E(_jy);}),[0,_js,_jt,_ju,_jv,_jw,_jD]];}else{if(_jN>57343){return [0,_i9,new T(function(){return _jC!=_jr?[0,_jm,_jn,_jo,_jp,_jC,_jr]:E(_jy);}),[0,_js,_jt,_ju,_jv,_jw,_jD]];}else{var _=writeOffAddr("w32",4,_js,_jD,((((_jI&4.294967295e9)-55296|0)<<10)+((_jN&4.294967295e9)-56320|0)|0)+65536|0),_=0,_jO=_jC+4|0,_jP=_jD+1|0;_jz=_jO;_jA=_jP;return null;}}}}}else{return [0,_dQ,new T(function(){return _jC!=_jr?[0,_jm,_jn,_jo,_jp,_jC,_jr]:E(_jy);}),[0,_js,_jt,_ju,_jv,_jw,_jD]];}}else{var _=writeOffAddr("w32",4,_js,_jD,_jI&4.294967295e9),_=0,_jO=_jC+2|0,_jP=_jD+1|0;_jz=_jO;_jA=_jP;return null;}}else{var _=writeOffAddr("w32",4,_js,_jD,_jI&4.294967295e9),_=0,_jO=_jC+2|0,_jP=_jD+1|0;_jz=_jO;_jA=_jP;return null;}}else{return [0,_dQ,new T(function(){return _jC!=_jr?[0,_jm,_jn,_jo,_jp,_jC,_jr]:E(_jy);}),[0,_js,_jt,_ju,_jv,_jw,_jD]];}}else{return [0,_dQ,new T(function(){return _jC!=_jr?[0,_jm,_jn,_jo,_jp,_jC,_jr]:E(_jy);}),[0,_js,_jt,_ju,_jv,_jw,_jD]];}}else{return [0,_dS,new T(function(){return _jC!=_jr?[0,_jm,_jn,_jo,_jp,_jC,_jr]:E(_jy);}),[0,_js,_jt,_ju,_jv,_jw,_jD]];}})(_jz,_jA,_);if(_jB!=null){return _jB;}}})(_jq,_jx,_);});},_jQ=function(_jR,_jS,_){var _jT=E(_jR),_jU=E(_jS);return new F(function(){return _iQ(_jT[1],_jT[2],_jT[3],_jT[4],_jT[5],_jT[6],_jU[1],_jU[2],_jU[3],_jU[4],_jU[5],_jU[6],_);});},_jV=[1,_jQ],_jW=function(_jX,_jY,_){var _jZ=E(_jX),_k0=E(_jY);return new F(function(){return _jl(_jZ[1],_jZ[2],_jZ[3],_jZ[4],_jZ[5],_jZ[6],_k0[1],_k0[2],_k0[3],_k0[4],_k0[5],_k0[6],_);});},_k1=[1,_jW],_k2=function(_k3,_k4,_k5,_k6,_k7,_k8,_k9,_ka,_){var _kb=rMV(_k3),_kc=_kb,_kd=E(_kc);if(!_kd[0]){if((_k9-_k8|0)>=2){var _ke=readOffAddr("w8",1,plusAddr(_k4,_k8),0),_kf=_ke,_=0,_kg=readOffAddr("w8",1,plusAddr(_k4,_k8+1|0),0),_kh=_kg,_=0,_ki=function(_kj){if(E(_kf)==255){if(E(_kh)==254){var _=wMV(_k3,_k1),_kk=E(_ka);return new F(function(){return _jl(_k4,_k5,_k6,_k7,_k8+2|0,_k9,_kk[1],_kk[2],_kk[3],_kk[4],_kk[5],_kk[6],_);});}else{var _=wMV(_k3,_jV),_kl=E(_ka);return new F(function(){return _iQ(_k4,_k5,_k6,_k7,_k8,_k9,_kl[1],_kl[2],_kl[3],_kl[4],_kl[5],_kl[6],_);});}}else{var _=wMV(_k3,_jV),_km=E(_ka);return new F(function(){return _iQ(_k4,_k5,_k6,_k7,_k8,_k9,_km[1],_km[2],_km[3],_km[4],_km[5],_km[6],_);});}};if(E(_kf)==254){if(E(_kh)==255){var _=wMV(_k3,_jV),_kn=E(_ka);return new F(function(){return _iQ(_k4,_k5,_k6,_k7,_k8+2|0,_k9,_kn[1],_kn[2],_kn[3],_kn[4],_kn[5],_kn[6],_);});}else{return new F(function(){return _ki(_);});}}else{return new F(function(){return _ki(_);});}}else{return [0,_dQ,[0,_k4,_k5,_k6,_k7,_k8,_k9],_ka];}}else{return new F(function(){return A(_kd[1],[[0,_k4,_k5,_k6,_k7,_k8,_k9],_ka,_]);});}},_ko=false,_kp=function(_){return _30;},_kq=new T(function(){return B(unCStr("UTF-16"));}),_kr=function(_ks){return [0,_kq,function(_){var _kt=nMV(_dR),_ku=_kt;return [0,function(_kv,_kw,_){var _kx=E(_kv);return new F(function(){return _k2(_ku,_kx[1],_kx[2],_kx[3],_kx[4],_kx[5],_kx[6],_kw,_);});},function(_ky,_kz,_){return new F(function(){return _hv(_ks,_ky,_kz,_);});},_kp,function(_){return new F(function(){return rMV(_ku);});},function(_kA,_){var _=wMV(_ku,_kA);return _30;}];},function(_){var _kB=nMV(_ko),_kC=_kB;return [0,function(_kD,_kE,_){var _kF=E(_kE);return new F(function(){return _iD(_kC,_kD,_kF[1],_kF[2],_kF[3],_kF[4],_kF[5],_kF[6],_);});},function(_ky,_kz,_){return new F(function(){return _hW(_ks,_ky,_kz,_);});},_kp,function(_){return new F(function(){return rMV(_kC);});},function(_kG,_){var _=wMV(_kC,_kG);return _30;}];}];},_kH=function(_kI,_kJ,_){var _kK=E(_kI),_kL=E(_kJ);return new F(function(){return _ia(_kK[1],_kK[2],_kK[3],_kK[4],_kK[5],_kK[6],_kL[1],_kL[2],_kL[3],_kL[4],_kL[5],_kL[6],_);});},_kM=function(_kN,_){return _30;},_kO=new T(function(){return B(unCStr("UTF-16BE"));}),_kP=function(_kQ){return [0,_kO,function(_){return [0,_jQ,function(_ky,_kz,_){return new F(function(){return _hv(_kQ,_ky,_kz,_);});},_kp,_kp,_kM];},function(_){return [0,_kH,function(_ky,_kz,_){return new F(function(){return _hW(_kQ,_ky,_kz,_);});},_kp,_kp,_kM];}];},_kR=function(_kS,_kT,_kU,_kV,_kW,_kX,_kY,_kZ,_l0,_l1,_l2,_l3,_){var _l4=[0,_kS,_kT,_kU,_kV,0,0],_l5=function(_l6,_l7,_){while(1){var _l8=(function(_l9,_la,_){if(_l9<_kX){if((_l1-_la|0)>=2){var _lb=readOffAddr("w32",4,_kS,_l9),_lc=_lb,_=0,_ld=_lc;if(_ld>=65536){if((_l1-_la|0)>=4){var _le=_ld-65536|0,_=writeOffAddr("w8",1,plusAddr(_kY,_la),0,_le>>10>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_kY,_la+1|0),0,((_le>>18)+216|0)>>>0&255),_=0,_lf=(_le>>>0&1023>>>0)>>>0&4.294967295e9,_=writeOffAddr("w8",1,plusAddr(_kY,_la+2|0),0,_lf>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_kY,_la+3|0),0,((_lf>>8)+220|0)>>>0&255),_=0,_lg=_l9+1|0,_lh=_la+4|0;_l6=_lg;_l7=_lh;return null;}else{return [0,_dS,new T(function(){return _l9!=_kX?[0,_kS,_kT,_kU,_kV,_l9,_kX]:E(_l4);}),[0,_kY,_kZ,_l0,_l1,_l2,_la]];}}else{var _li=function(_lj){if(56320>_ld){var _=writeOffAddr("w8",1,plusAddr(_kY,_la),0,_ld>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_kY,_la+1|0),0,_ld>>8>>>0&255),_=0;return new F(function(){return _l5(_l9+1|0,_la+2|0,_);});}else{if(_ld>57343){var _=writeOffAddr("w8",1,plusAddr(_kY,_la),0,_ld>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_kY,_la+1|0),0,_ld>>8>>>0&255),_=0;return new F(function(){return _l5(_l9+1|0,_la+2|0,_);});}else{return [0,_i9,new T(function(){return _l9!=_kX?[0,_kS,_kT,_kU,_kV,_l9,_kX]:E(_l4);}),[0,_kY,_kZ,_l0,_l1,_l2,_la]];}}};if(55296>_ld){return new F(function(){return _li(_);});}else{return _ld>56319?B(_li(_)):[0,_i9,new T(function(){return _l9!=_kX?[0,_kS,_kT,_kU,_kV,_l9,_kX]:E(_l4);}),[0,_kY,_kZ,_l0,_l1,_l2,_la]];}}}else{return [0,_dS,new T(function(){return _l9!=_kX?[0,_kS,_kT,_kU,_kV,_l9,_kX]:E(_l4);}),[0,_kY,_kZ,_l0,_l1,_l2,_la]];}}else{return [0,_dQ,new T(function(){return _l9!=_kX?[0,_kS,_kT,_kU,_kV,_l9,_kX]:E(_l4);}),[0,_kY,_kZ,_l0,_l1,_l2,_la]];}})(_l6,_l7,_);if(_l8!=null){return _l8;}}};return new F(function(){return _l5(_kW,_l3,_);});},_lk=function(_ll,_lm,_){var _ln=E(_ll),_lo=E(_lm);return new F(function(){return _kR(_ln[1],_ln[2],_ln[3],_ln[4],_ln[5],_ln[6],_lo[1],_lo[2],_lo[3],_lo[4],_lo[5],_lo[6],_);});},_lp=new T(function(){return B(unCStr("UTF16-LE"));}),_lq=function(_lr){return [0,_lp,function(_){return [0,_jW,function(_ky,_kz,_){return new F(function(){return _hv(_lr,_ky,_kz,_);});},_kp,_kp,_kM];},function(_){return [0,_lk,function(_ky,_kz,_){return new F(function(){return _hW(_lr,_ky,_kz,_);});},_kp,_kp,_kM];}];},_ls=function(_lt,_lu,_lv,_lw,_lx,_ly,_lz,_lA,_lB,_lC,_lD,_lE,_){var _lF=[0,_lt,_lu,_lv,_lw,0,0],_lG=function(_lH,_lI,_){if(_lH<_ly){if((_lC-_lI|0)>=4){var _lJ=readOffAddr("w32",4,_lt,_lH),_lK=_lJ,_=0,_lL=_lK,_lM=function(_lN){if(56320>_lL){var _=writeOffAddr("w8",1,plusAddr(_lz,_lI),0,_lL>>24>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_lz,_lI+1|0),0,_lL>>16>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_lz,_lI+2|0),0,_lL>>8>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_lz,_lI+3|0),0,_lL>>>0&255),_=0;return new F(function(){return _lG(_lH+1|0,_lI+4|0,_);});}else{if(_lL>57343){var _=writeOffAddr("w8",1,plusAddr(_lz,_lI),0,_lL>>24>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_lz,_lI+1|0),0,_lL>>16>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_lz,_lI+2|0),0,_lL>>8>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_lz,_lI+3|0),0,_lL>>>0&255),_=0;return new F(function(){return _lG(_lH+1|0,_lI+4|0,_);});}else{return [0,_i9,new T(function(){return _lH!=_ly?[0,_lt,_lu,_lv,_lw,_lH,_ly]:E(_lF);}),[0,_lz,_lA,_lB,_lC,_lD,_lI]];}}};if(55296>_lL){return new F(function(){return _lM(_);});}else{return _lL>56319?B(_lM(_)):[0,_i9,new T(function(){return _lH!=_ly?[0,_lt,_lu,_lv,_lw,_lH,_ly]:E(_lF);}),[0,_lz,_lA,_lB,_lC,_lD,_lI]];}}else{return [0,_dS,new T(function(){return _lH!=_ly?[0,_lt,_lu,_lv,_lw,_lH,_ly]:E(_lF);}),[0,_lz,_lA,_lB,_lC,_lD,_lI]];}}else{return [0,_dQ,new T(function(){return _lH!=_ly?[0,_lt,_lu,_lv,_lw,_lH,_ly]:E(_lF);}),[0,_lz,_lA,_lB,_lC,_lD,_lI]];}};return new F(function(){return _lG(_lx,_lE,_);});},_lO=function(_lP,_lQ,_lR,_lS,_lT,_lU,_lV,_lW,_){var _lX=rMV(_lP),_lY=_lX;if(!E(_lY)){if((_lU-_lW|0)>=4){var _=wMV(_lP,_35),_=writeOffAddr("w8",1,plusAddr(_lR,_lW),0,0),_=0,_=writeOffAddr("w8",1,plusAddr(_lR,_lW+1|0),0,0),_=0,_=writeOffAddr("w8",1,plusAddr(_lR,_lW+2|0),0,254),_=0,_=writeOffAddr("w8",1,plusAddr(_lR,_lW+3|0),0,255),_=0,_lZ=E(_lQ);return new F(function(){return _ls(_lZ[1],_lZ[2],_lZ[3],_lZ[4],_lZ[5],_lZ[6],_lR,_lS,_lT,_lU,_lV,_lW+4|0,_);});}else{return [0,_dS,_lQ,[0,_lR,_lS,_lT,_lU,_lV,_lW]];}}else{var _m0=E(_lQ);return new F(function(){return _ls(_m0[1],_m0[2],_m0[3],_m0[4],_m0[5],_m0[6],_lR,_lS,_lT,_lU,_lV,_lW,_);});}},_m1=function(_m2,_m3,_m4,_m5,_m6,_m7,_m8,_m9,_ma,_mb,_mc,_md,_){var _me=[0,_m2,_m3,_m4,_m5,0,0],_mf=function(_mg,_mh,_){while(1){var _mi=(function(_mj,_mk,_){if(_mk<_mb){if((_m7-_mj|0)>=4){var _ml=readOffAddr("w8",1,plusAddr(_m2,_mj),0),_mm=_ml,_=0,_mn=readOffAddr("w8",1,plusAddr(_m2,_mj+1|0),0),_mo=_mn,_=0,_mp=readOffAddr("w8",1,plusAddr(_m2,_mj+2|0),0),_mq=_mp,_=0,_mr=readOffAddr("w8",1,plusAddr(_m2,_mj+3|0),0),_ms=_mr,_=0,_mt=((((_mm&4.294967295e9)<<24)+((_mo&4.294967295e9)<<16)|0)+((_mq&4.294967295e9)<<8)|0)+(_ms&4.294967295e9)|0,_mu=_mt,_mv=function(_mw){if(_mu<=57343){return [0,_i9,new T(function(){return _mj!=_m7?[0,_m2,_m3,_m4,_m5,_mj,_m7]:E(_me);}),[0,_m8,_m9,_ma,_mb,_mc,_mk]];}else{if(_mu>1114111){return [0,_i9,new T(function(){return _mj!=_m7?[0,_m2,_m3,_m4,_m5,_mj,_m7]:E(_me);}),[0,_m8,_m9,_ma,_mb,_mc,_mk]];}else{var _=writeOffAddr("w32",4,_m8,_mk,_mt),_=0;return new F(function(){return _mf(_mj+4|0,_mk+1|0,_);});}}};if(_mu<0){return new F(function(){return _mv(_);});}else{if(_mu>=55296){return new F(function(){return _mv(_);});}else{var _=writeOffAddr("w32",4,_m8,_mk,_mt),_=0,_mx=_mj+4|0,_my=_mk+1|0;_mg=_mx;_mh=_my;return null;}}}else{return [0,_dQ,new T(function(){return _mj!=_m7?[0,_m2,_m3,_m4,_m5,_mj,_m7]:E(_me);}),[0,_m8,_m9,_ma,_mb,_mc,_mk]];}}else{return [0,_dS,new T(function(){return _mj!=_m7?[0,_m2,_m3,_m4,_m5,_mj,_m7]:E(_me);}),[0,_m8,_m9,_ma,_mb,_mc,_mk]];}})(_mg,_mh,_);if(_mi!=null){return _mi;}}};return new F(function(){return _mf(_m6,_md,_);});},_mz=function(_mA,_mB,_mC,_mD,_mE,_mF,_mG,_mH,_mI,_mJ,_mK,_mL,_){var _mM=[0,_mA,_mB,_mC,_mD,0,0],_mN=function(_mO,_mP,_){while(1){var _mQ=(function(_mR,_mS,_){if(_mS<_mJ){if((_mF-_mR|0)>=4){var _mT=readOffAddr("w8",1,plusAddr(_mA,_mR),0),_mU=_mT,_=0,_mV=readOffAddr("w8",1,plusAddr(_mA,_mR+1|0),0),_mW=_mV,_=0,_mX=readOffAddr("w8",1,plusAddr(_mA,_mR+2|0),0),_mY=_mX,_=0,_mZ=readOffAddr("w8",1,plusAddr(_mA,_mR+3|0),0),_n0=_mZ,_=0,_n1=((((_n0&4.294967295e9)<<24)+((_mY&4.294967295e9)<<16)|0)+((_mW&4.294967295e9)<<8)|0)+(_mU&4.294967295e9)|0,_n2=_n1,_n3=function(_n4){if(_n2<=57343){return [0,_i9,new T(function(){return _mR!=_mF?[0,_mA,_mB,_mC,_mD,_mR,_mF]:E(_mM);}),[0,_mG,_mH,_mI,_mJ,_mK,_mS]];}else{if(_n2>1114111){return [0,_i9,new T(function(){return _mR!=_mF?[0,_mA,_mB,_mC,_mD,_mR,_mF]:E(_mM);}),[0,_mG,_mH,_mI,_mJ,_mK,_mS]];}else{var _=writeOffAddr("w32",4,_mG,_mS,_n1),_=0;return new F(function(){return _mN(_mR+4|0,_mS+1|0,_);});}}};if(_n2<0){return new F(function(){return _n3(_);});}else{if(_n2>=55296){return new F(function(){return _n3(_);});}else{var _=writeOffAddr("w32",4,_mG,_mS,_n1),_=0,_n5=_mR+4|0,_n6=_mS+1|0;_mO=_n5;_mP=_n6;return null;}}}else{return [0,_dQ,new T(function(){return _mR!=_mF?[0,_mA,_mB,_mC,_mD,_mR,_mF]:E(_mM);}),[0,_mG,_mH,_mI,_mJ,_mK,_mS]];}}else{return [0,_dS,new T(function(){return _mR!=_mF?[0,_mA,_mB,_mC,_mD,_mR,_mF]:E(_mM);}),[0,_mG,_mH,_mI,_mJ,_mK,_mS]];}})(_mO,_mP,_);if(_mQ!=null){return _mQ;}}};return new F(function(){return _mN(_mE,_mL,_);});},_n7=function(_n8,_n9,_){var _na=E(_n8),_nb=E(_n9);return new F(function(){return _m1(_na[1],_na[2],_na[3],_na[4],_na[5],_na[6],_nb[1],_nb[2],_nb[3],_nb[4],_nb[5],_nb[6],_);});},_nc=[1,_n7],_nd=function(_ne,_nf,_){var _ng=E(_ne),_nh=E(_nf);return new F(function(){return _mz(_ng[1],_ng[2],_ng[3],_ng[4],_ng[5],_ng[6],_nh[1],_nh[2],_nh[3],_nh[4],_nh[5],_nh[6],_);});},_ni=[1,_nd],_nj=function(_nk,_nl,_nm,_nn,_no,_np,_nq,_nr,_){var _ns=rMV(_nk),_nt=_ns,_nu=E(_nt);if(!_nu[0]){if((_nq-_np|0)>=4){var _nv=readOffAddr("w8",1,plusAddr(_nl,_np),0),_nw=_nv,_=0,_nx=readOffAddr("w8",1,plusAddr(_nl,_np+1|0),0),_ny=_nx,_=0,_nz=readOffAddr("w8",1,plusAddr(_nl,_np+2|0),0),_nA=_nz,_=0,_nB=readOffAddr("w8",1,plusAddr(_nl,_np+3|0),0),_nC=_nB,_=0,_nD=function(_nE){if(E(_nw)==255){if(E(_ny)==254){if(!E(_nA)){if(!E(_nC)){var _=wMV(_nk,_ni),_nF=E(_nr);return new F(function(){return _mz(_nl,_nm,_nn,_no,_np+4|0,_nq,_nF[1],_nF[2],_nF[3],_nF[4],_nF[5],_nF[6],_);});}else{var _=wMV(_nk,_nc),_nG=E(_nr);return new F(function(){return _m1(_nl,_nm,_nn,_no,_np,_nq,_nG[1],_nG[2],_nG[3],_nG[4],_nG[5],_nG[6],_);});}}else{var _=wMV(_nk,_nc),_nH=E(_nr);return new F(function(){return _m1(_nl,_nm,_nn,_no,_np,_nq,_nH[1],_nH[2],_nH[3],_nH[4],_nH[5],_nH[6],_);});}}else{var _=wMV(_nk,_nc),_nI=E(_nr);return new F(function(){return _m1(_nl,_nm,_nn,_no,_np,_nq,_nI[1],_nI[2],_nI[3],_nI[4],_nI[5],_nI[6],_);});}}else{var _=wMV(_nk,_nc),_nJ=E(_nr);return new F(function(){return _m1(_nl,_nm,_nn,_no,_np,_nq,_nJ[1],_nJ[2],_nJ[3],_nJ[4],_nJ[5],_nJ[6],_);});}};if(!E(_nw)){if(!E(_ny)){if(E(_nA)==254){if(E(_nC)==255){var _=wMV(_nk,_nc),_nK=E(_nr);return new F(function(){return _m1(_nl,_nm,_nn,_no,_np+4|0,_nq,_nK[1],_nK[2],_nK[3],_nK[4],_nK[5],_nK[6],_);});}else{return new F(function(){return _nD(_);});}}else{return new F(function(){return _nD(_);});}}else{return new F(function(){return _nD(_);});}}else{return new F(function(){return _nD(_);});}}else{return [0,_dQ,[0,_nl,_nm,_nn,_no,_np,_nq],_nr];}}else{return new F(function(){return A(_nu[1],[[0,_nl,_nm,_nn,_no,_np,_nq],_nr,_]);});}},_nL=function(_){return _30;},_nM=new T(function(){return B(unCStr("UTF-32"));}),_nN=function(_nO){return [0,_nM,function(_){var _nP=nMV(_dR),_nQ=_nP;return [0,function(_nR,_nS,_){var _nT=E(_nR);return new F(function(){return _nj(_nQ,_nT[1],_nT[2],_nT[3],_nT[4],_nT[5],_nT[6],_nS,_);});},function(_nU,_nV,_){return new F(function(){return _hv(_nO,_nU,_nV,_);});},_nL,function(_){return new F(function(){return rMV(_nQ);});},function(_nW,_){var _=wMV(_nQ,_nW);return _30;}];},function(_){var _nX=nMV(_ko),_nY=_nX;return [0,function(_nZ,_o0,_){var _o1=E(_o0);return new F(function(){return _lO(_nY,_nZ,_o1[1],_o1[2],_o1[3],_o1[4],_o1[5],_o1[6],_);});},function(_nU,_nV,_){return new F(function(){return _hW(_nO,_nU,_nV,_);});},_nL,function(_){return new F(function(){return rMV(_nY);});},function(_o2,_){var _=wMV(_nY,_o2);return _30;}];}];},_o3=function(_o4,_o5,_){var _o6=E(_o4),_o7=E(_o5);return new F(function(){return _ls(_o6[1],_o6[2],_o6[3],_o6[4],_o6[5],_o6[6],_o7[1],_o7[2],_o7[3],_o7[4],_o7[5],_o7[6],_);});},_o8=function(_o9,_){return _30;},_oa=new T(function(){return B(unCStr("UTF-32BE"));}),_ob=function(_oc){return [0,_oa,function(_){return [0,_n7,function(_nU,_nV,_){return new F(function(){return _hv(_oc,_nU,_nV,_);});},_nL,_nL,_o8];},function(_){return [0,_o3,function(_nU,_nV,_){return new F(function(){return _hW(_oc,_nU,_nV,_);});},_nL,_nL,_o8];}];},_od=function(_oe,_of,_og,_oh,_oi,_oj,_ok,_ol,_om,_on,_oo,_op,_){var _oq=[0,_oe,_of,_og,_oh,0,0],_or=function(_os,_ot,_){if(_os<_oj){if((_on-_ot|0)>=4){var _ou=readOffAddr("w32",4,_oe,_os),_ov=_ou,_=0,_ow=_ov,_ox=function(_oy){if(56320>_ow){var _=writeOffAddr("w8",1,plusAddr(_ok,_ot),0,_ow>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_ok,_ot+1|0),0,_ow>>8>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_ok,_ot+2|0),0,_ow>>16>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_ok,_ot+3|0),0,_ow>>24>>>0&255),_=0;return new F(function(){return _or(_os+1|0,_ot+4|0,_);});}else{if(_ow>57343){var _=writeOffAddr("w8",1,plusAddr(_ok,_ot),0,_ow>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_ok,_ot+1|0),0,_ow>>8>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_ok,_ot+2|0),0,_ow>>16>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_ok,_ot+3|0),0,_ow>>24>>>0&255),_=0;return new F(function(){return _or(_os+1|0,_ot+4|0,_);});}else{return [0,_i9,new T(function(){return _os!=_oj?[0,_oe,_of,_og,_oh,_os,_oj]:E(_oq);}),[0,_ok,_ol,_om,_on,_oo,_ot]];}}};if(55296>_ow){return new F(function(){return _ox(_);});}else{return _ow>56319?B(_ox(_)):[0,_i9,new T(function(){return _os!=_oj?[0,_oe,_of,_og,_oh,_os,_oj]:E(_oq);}),[0,_ok,_ol,_om,_on,_oo,_ot]];}}else{return [0,_dS,new T(function(){return _os!=_oj?[0,_oe,_of,_og,_oh,_os,_oj]:E(_oq);}),[0,_ok,_ol,_om,_on,_oo,_ot]];}}else{return [0,_dQ,new T(function(){return _os!=_oj?[0,_oe,_of,_og,_oh,_os,_oj]:E(_oq);}),[0,_ok,_ol,_om,_on,_oo,_ot]];}};return new F(function(){return _or(_oi,_op,_);});},_oz=function(_oA,_oB,_){var _oC=E(_oA),_oD=E(_oB);return new F(function(){return _od(_oC[1],_oC[2],_oC[3],_oC[4],_oC[5],_oC[6],_oD[1],_oD[2],_oD[3],_oD[4],_oD[5],_oD[6],_);});},_oE=new T(function(){return B(unCStr("UTF-32LE"));}),_oF=function(_oG){return [0,_oE,function(_){return [0,_nd,function(_nU,_nV,_){return new F(function(){return _hv(_oG,_nU,_nV,_);});},_nL,_nL,_o8];},function(_){return [0,_oz,function(_nU,_nV,_){return new F(function(){return _hW(_oG,_nU,_nV,_);});},_nL,_nL,_o8];}];},_oH=function(_oI,_oJ,_oK,_oL,_oM,_oN,_oO,_oP,_oQ,_oR,_oS,_oT,_){var _oU=[0,_oI,_oJ,_oK,_oL,0,0],_oV=function(_oW,_oX,_){while(1){var _oY=(function(_oZ,_p0,_){if(_p0<_oR){if(_oZ<_oN){var _p1=readOffAddr("w32",4,_oI,_oZ),_p2=_p1,_=0,_p3=_p2;if(_p3>127){if(_p3>2047){if(_p3>65535){if((_oR-_p0|0)>=4){var _=writeOffAddr("w8",1,plusAddr(_oO,_p0),0,((_p3>>18)+240|0)>>>0&255),_=0,_p4=63>>>0,_=writeOffAddr("w8",1,plusAddr(_oO,_p0+1|0),0,(((_p3>>12>>>0&_p4)>>>0&4.294967295e9)+128|0)>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_oO,_p0+2|0),0,(((_p3>>6>>>0&_p4)>>>0&4.294967295e9)+128|0)>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_oO,_p0+3|0),0,(((_p3>>>0&_p4)>>>0&4.294967295e9)+128|0)>>>0&255),_=0,_p5=_oZ+1|0,_p6=_p0+4|0;_oW=_p5;_oX=_p6;return null;}else{return [0,_dS,new T(function(){return _oZ!=_oN?[0,_oI,_oJ,_oK,_oL,_oZ,_oN]:E(_oU);}),[0,_oO,_oP,_oQ,_oR,_oS,_p0]];}}else{var _p7=function(_p8){var _p9=function(_pa){if((_oR-_p0|0)>=3){var _=writeOffAddr("w8",1,plusAddr(_oO,_p0),0,((_p3>>12)+224|0)>>>0&255),_=0,_pb=63>>>0,_=writeOffAddr("w8",1,plusAddr(_oO,_p0+1|0),0,(((_p3>>6>>>0&_pb)>>>0&4.294967295e9)+128|0)>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_oO,_p0+2|0),0,(((_p3>>>0&_pb)>>>0&4.294967295e9)+128|0)>>>0&255),_=0;return new F(function(){return _oV(_oZ+1|0,_p0+3|0,_);});}else{return [0,_dS,new T(function(){return _oZ!=_oN?[0,_oI,_oJ,_oK,_oL,_oZ,_oN]:E(_oU);}),[0,_oO,_oP,_oQ,_oR,_oS,_p0]];}};if(56320>_p3){return new F(function(){return _p9(_);});}else{return _p3>57343?B(_p9(_)):[0,_i9,new T(function(){return _oZ!=_oN?[0,_oI,_oJ,_oK,_oL,_oZ,_oN]:E(_oU);}),[0,_oO,_oP,_oQ,_oR,_oS,_p0]];}};if(55296>_p3){return new F(function(){return _p7(_);});}else{return _p3>56319?B(_p7(_)):[0,_i9,new T(function(){return _oZ!=_oN?[0,_oI,_oJ,_oK,_oL,_oZ,_oN]:E(_oU);}),[0,_oO,_oP,_oQ,_oR,_oS,_p0]];}}}else{if((_oR-_p0|0)>=2){var _=writeOffAddr("w8",1,plusAddr(_oO,_p0),0,((_p3>>6)+192|0)>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_oO,_p0+1|0),0,(((_p3>>>0&63>>>0)>>>0&4.294967295e9)+128|0)>>>0&255),_=0,_p5=_oZ+1|0,_p6=_p0+2|0;_oW=_p5;_oX=_p6;return null;}else{return [0,_dS,new T(function(){return _oZ!=_oN?[0,_oI,_oJ,_oK,_oL,_oZ,_oN]:E(_oU);}),[0,_oO,_oP,_oQ,_oR,_oS,_p0]];}}}else{var _=writeOffAddr("w8",1,plusAddr(_oO,_p0),0,_p3>>>0&255),_=0,_p5=_oZ+1|0,_p6=_p0+1|0;_oW=_p5;_oX=_p6;return null;}}else{return [0,_dQ,new T(function(){return _oZ!=_oN?[0,_oI,_oJ,_oK,_oL,_oZ,_oN]:E(_oU);}),[0,_oO,_oP,_oQ,_oR,_oS,_p0]];}}else{return [0,_dS,new T(function(){return _oZ!=_oN?[0,_oI,_oJ,_oK,_oL,_oZ,_oN]:E(_oU);}),[0,_oO,_oP,_oQ,_oR,_oS,_p0]];}})(_oW,_oX,_);if(_oY!=null){return _oY;}}};return new F(function(){return _oV(_oM,_oT,_);});},_pc=function(_pd,_pe,_){var _pf=E(_pd),_pg=E(_pe);return new F(function(){return _oH(_pf[1],_pf[2],_pf[3],_pf[4],_pf[5],_pf[6],_pg[1],_pg[2],_pg[3],_pg[4],_pg[5],_pg[6],_);});},_ph=function(_pi,_){return _30;},_pj=function(_){return _30;},_pk=function(_pl,_pm,_pn,_po,_pp,_pq,_pr,_ps,_pt,_pu,_pv,_pw,_){var _px=[0,_pl,_pm,_pn,_po,0,0],_py=function(_pz,_pA,_){while(1){var _pB=(function(_pC,_pD,_){if(_pD<_pu){if(_pC<_pq){var _pE=readOffAddr("w8",1,plusAddr(_pl,_pC),0),_pF=_pE,_=0;if(_pF>127){var _pG=function(_pH){var _pI=function(_pJ){if(_pF<240){return [0,_i9,new T(function(){return _pC!=_pq?[0,_pl,_pm,_pn,_po,_pC,_pq]:E(_px);}),[0,_pr,_ps,_pt,_pu,_pv,_pD]];}else{switch(_pq-_pC|0){case 1:return [0,_dQ,new T(function(){return _pC!=_pq?[0,_pl,_pm,_pn,_po,_pC,_pq]:E(_px);}),[0,_pr,_ps,_pt,_pu,_pv,_pD]];case 2:var _pK=readOffAddr("w8",1,plusAddr(_pl,_pC+1|0),0),_pL=_pK,_=0,_pM=function(_pN){var _pO=function(_pP){return E(_pF)==244?_pL<128?[0,_i9,new T(function(){return _pC!=_pq?[0,_pl,_pm,_pn,_po,_pC,_pq]:E(_px);}),[0,_pr,_ps,_pt,_pu,_pv,_pD]]:_pL>143?[0,_i9,new T(function(){return _pC!=_pq?[0,_pl,_pm,_pn,_po,_pC,_pq]:E(_px);}),[0,_pr,_ps,_pt,_pu,_pv,_pD]]:[0,_dQ,new T(function(){return _pC!=_pq?[0,_pl,_pm,_pn,_po,_pC,_pq]:E(_px);}),[0,_pr,_ps,_pt,_pu,_pv,_pD]]:[0,_i9,new T(function(){return _pC!=_pq?[0,_pl,_pm,_pn,_po,_pC,_pq]:E(_px);}),[0,_pr,_ps,_pt,_pu,_pv,_pD]];};if(_pF<241){return new F(function(){return _pO(_);});}else{if(_pF>243){return new F(function(){return _pO(_);});}else{if(_pL<128){return new F(function(){return _pO(_);});}else{return _pL>191?B(_pO(_)):[0,_dQ,new T(function(){return _pC!=_pq?[0,_pl,_pm,_pn,_po,_pC,_pq]:E(_px);}),[0,_pr,_ps,_pt,_pu,_pv,_pD]];}}}};if(E(_pF)==240){if(_pL<144){return new F(function(){return _pM(_);});}else{return _pL>191?B(_pM(_)):[0,_dQ,new T(function(){return _pC!=_pq?[0,_pl,_pm,_pn,_po,_pC,_pq]:E(_px);}),[0,_pr,_ps,_pt,_pu,_pv,_pD]];}}else{return new F(function(){return _pM(_);});}break;case 3:var _pQ=readOffAddr("w8",1,plusAddr(_pl,_pC+1|0),0),_pR=_pQ,_=0,_pS=readOffAddr("w8",1,plusAddr(_pl,_pC+2|0),0),_pT=_pS,_=0,_pU=function(_pV){var _pW=function(_pX){return E(_pF)==244?_pR<128?[0,_i9,new T(function(){return _pC!=_pq?[0,_pl,_pm,_pn,_po,_pC,_pq]:E(_px);}),[0,_pr,_ps,_pt,_pu,_pv,_pD]]:_pR>143?[0,_i9,new T(function(){return _pC!=_pq?[0,_pl,_pm,_pn,_po,_pC,_pq]:E(_px);}),[0,_pr,_ps,_pt,_pu,_pv,_pD]]:_pT<128?[0,_i9,new T(function(){return _pC!=_pq?[0,_pl,_pm,_pn,_po,_pC,_pq]:E(_px);}),[0,_pr,_ps,_pt,_pu,_pv,_pD]]:_pT>191?[0,_i9,new T(function(){return _pC!=_pq?[0,_pl,_pm,_pn,_po,_pC,_pq]:E(_px);}),[0,_pr,_ps,_pt,_pu,_pv,_pD]]:[0,_dQ,new T(function(){return _pC!=_pq?[0,_pl,_pm,_pn,_po,_pC,_pq]:E(_px);}),[0,_pr,_ps,_pt,_pu,_pv,_pD]]:[0,_i9,new T(function(){return _pC!=_pq?[0,_pl,_pm,_pn,_po,_pC,_pq]:E(_px);}),[0,_pr,_ps,_pt,_pu,_pv,_pD]];};if(_pF<241){return new F(function(){return _pW(_);});}else{if(_pF>243){return new F(function(){return _pW(_);});}else{if(_pR<128){return new F(function(){return _pW(_);});}else{if(_pR>191){return new F(function(){return _pW(_);});}else{if(_pT<128){return new F(function(){return _pW(_);});}else{return _pT>191?B(_pW(_)):[0,_dQ,new T(function(){return _pC!=_pq?[0,_pl,_pm,_pn,_po,_pC,_pq]:E(_px);}),[0,_pr,_ps,_pt,_pu,_pv,_pD]];}}}}}};if(E(_pF)==240){if(_pR<144){return new F(function(){return _pU(_);});}else{if(_pR>191){return new F(function(){return _pU(_);});}else{if(_pT<128){return new F(function(){return _pU(_);});}else{return _pT>191?B(_pU(_)):[0,_dQ,new T(function(){return _pC!=_pq?[0,_pl,_pm,_pn,_po,_pC,_pq]:E(_px);}),[0,_pr,_ps,_pt,_pu,_pv,_pD]];}}}}else{return new F(function(){return _pU(_);});}break;default:var _pY=readOffAddr("w8",1,plusAddr(_pl,_pC+1|0),0),_pZ=_pY,_=0,_q0=readOffAddr("w8",1,plusAddr(_pl,_pC+2|0),0),_q1=_q0,_=0,_q2=readOffAddr("w8",1,plusAddr(_pl,_pC+3|0),0),_q3=_q2,_=0,_q4=function(_q5){var _=writeOffAddr("w32",4,_pr,_pD,(((((_pF&4.294967295e9)-240|0)<<18)+(((_pZ&4.294967295e9)-128|0)<<12)|0)+(((_q1&4.294967295e9)-128|0)<<6)|0)+((_q3&4.294967295e9)-128|0)|0),_=0;return new F(function(){return _py(_pC+4|0,_pD+1|0,_);});},_q6=function(_q7){var _q8=function(_q9){return E(_pF)==244?_pZ<128?[0,_i9,new T(function(){return _pC!=_pq?[0,_pl,_pm,_pn,_po,_pC,_pq]:E(_px);}),[0,_pr,_ps,_pt,_pu,_pv,_pD]]:_pZ>143?[0,_i9,new T(function(){return _pC!=_pq?[0,_pl,_pm,_pn,_po,_pC,_pq]:E(_px);}),[0,_pr,_ps,_pt,_pu,_pv,_pD]]:_q1<128?[0,_i9,new T(function(){return _pC!=_pq?[0,_pl,_pm,_pn,_po,_pC,_pq]:E(_px);}),[0,_pr,_ps,_pt,_pu,_pv,_pD]]:_q1>191?[0,_i9,new T(function(){return _pC!=_pq?[0,_pl,_pm,_pn,_po,_pC,_pq]:E(_px);}),[0,_pr,_ps,_pt,_pu,_pv,_pD]]:_q3<128?[0,_i9,new T(function(){return _pC!=_pq?[0,_pl,_pm,_pn,_po,_pC,_pq]:E(_px);}),[0,_pr,_ps,_pt,_pu,_pv,_pD]]:_q3>191?[0,_i9,new T(function(){return _pC!=_pq?[0,_pl,_pm,_pn,_po,_pC,_pq]:E(_px);}),[0,_pr,_ps,_pt,_pu,_pv,_pD]]:B(_q4(_)):[0,_i9,new T(function(){return _pC!=_pq?[0,_pl,_pm,_pn,_po,_pC,_pq]:E(_px);}),[0,_pr,_ps,_pt,_pu,_pv,_pD]];};if(_pF<241){return new F(function(){return _q8(_);});}else{if(_pF>243){return new F(function(){return _q8(_);});}else{if(_pZ<128){return new F(function(){return _q8(_);});}else{if(_pZ>191){return new F(function(){return _q8(_);});}else{if(_q1<128){return new F(function(){return _q8(_);});}else{if(_q1>191){return new F(function(){return _q8(_);});}else{if(_q3<128){return new F(function(){return _q8(_);});}else{return _q3>191?B(_q8(_)):B(_q4(_));}}}}}}}};if(E(_pF)==240){if(_pZ<144){return new F(function(){return _q6(_);});}else{if(_pZ>191){return new F(function(){return _q6(_);});}else{if(_q1<128){return new F(function(){return _q6(_);});}else{if(_q1>191){return new F(function(){return _q6(_);});}else{if(_q3<128){return new F(function(){return _q6(_);});}else{return _q3>191?B(_q6(_)):B(_q4(_));}}}}}}else{return new F(function(){return _q6(_);});}}}};if(_pF<224){return new F(function(){return _pI(_);});}else{if(_pF>239){return new F(function(){return _pI(_);});}else{switch(_pq-_pC|0){case 1:return [0,_dQ,new T(function(){return _pC!=_pq?[0,_pl,_pm,_pn,_po,_pC,_pq]:E(_px);}),[0,_pr,_ps,_pt,_pu,_pv,_pD]];case 2:var _qa=readOffAddr("w8",1,plusAddr(_pl,_pC+1|0),0),_qb=_qa,_=0,_qc=function(_qd){var _qe=function(_qf){var _qg=function(_qh){return _pF<238?[0,_i9,new T(function(){return _pC!=_pq?[0,_pl,_pm,_pn,_po,_pC,_pq]:E(_px);}),[0,_pr,_ps,_pt,_pu,_pv,_pD]]:_qb<128?[0,_i9,new T(function(){return _pC!=_pq?[0,_pl,_pm,_pn,_po,_pC,_pq]:E(_px);}),[0,_pr,_ps,_pt,_pu,_pv,_pD]]:_qb>191?[0,_i9,new T(function(){return _pC!=_pq?[0,_pl,_pm,_pn,_po,_pC,_pq]:E(_px);}),[0,_pr,_ps,_pt,_pu,_pv,_pD]]:[0,_dQ,new T(function(){return _pC!=_pq?[0,_pl,_pm,_pn,_po,_pC,_pq]:E(_px);}),[0,_pr,_ps,_pt,_pu,_pv,_pD]];};if(E(_pF)==237){if(_qb<128){return new F(function(){return _qg(_);});}else{return _qb>159?B(_qg(_)):[0,_dQ,new T(function(){return _pC!=_pq?[0,_pl,_pm,_pn,_po,_pC,_pq]:E(_px);}),[0,_pr,_ps,_pt,_pu,_pv,_pD]];}}else{return new F(function(){return _qg(_);});}};if(_pF<225){return new F(function(){return _qe(_);});}else{if(_pF>236){return new F(function(){return _qe(_);});}else{if(_qb<128){return new F(function(){return _qe(_);});}else{return _qb>191?B(_qe(_)):[0,_dQ,new T(function(){return _pC!=_pq?[0,_pl,_pm,_pn,_po,_pC,_pq]:E(_px);}),[0,_pr,_ps,_pt,_pu,_pv,_pD]];}}}};if(E(_pF)==224){if(_qb<160){return new F(function(){return _qc(_);});}else{return _qb>191?B(_qc(_)):[0,_dQ,new T(function(){return _pC!=_pq?[0,_pl,_pm,_pn,_po,_pC,_pq]:E(_px);}),[0,_pr,_ps,_pt,_pu,_pv,_pD]];}}else{return new F(function(){return _qc(_);});}break;default:var _qi=readOffAddr("w8",1,plusAddr(_pl,_pC+1|0),0),_qj=_qi,_=0,_qk=readOffAddr("w8",1,plusAddr(_pl,_pC+2|0),0),_ql=_qk,_=0,_qm=function(_qn){var _=writeOffAddr("w32",4,_pr,_pD,((((_pF&4.294967295e9)-224|0)<<12)+(((_qj&4.294967295e9)-128|0)<<6)|0)+((_ql&4.294967295e9)-128|0)|0),_=0;return new F(function(){return _py(_pC+3|0,_pD+1|0,_);});},_qo=function(_qp){var _qq=function(_qr){var _qs=function(_qt){return _pF<238?[0,_i9,new T(function(){return _pC!=_pq?[0,_pl,_pm,_pn,_po,_pC,_pq]:E(_px);}),[0,_pr,_ps,_pt,_pu,_pv,_pD]]:_qj<128?[0,_i9,new T(function(){return _pC!=_pq?[0,_pl,_pm,_pn,_po,_pC,_pq]:E(_px);}),[0,_pr,_ps,_pt,_pu,_pv,_pD]]:_qj>191?[0,_i9,new T(function(){return _pC!=_pq?[0,_pl,_pm,_pn,_po,_pC,_pq]:E(_px);}),[0,_pr,_ps,_pt,_pu,_pv,_pD]]:_ql<128?[0,_i9,new T(function(){return _pC!=_pq?[0,_pl,_pm,_pn,_po,_pC,_pq]:E(_px);}),[0,_pr,_ps,_pt,_pu,_pv,_pD]]:_ql>191?[0,_i9,new T(function(){return _pC!=_pq?[0,_pl,_pm,_pn,_po,_pC,_pq]:E(_px);}),[0,_pr,_ps,_pt,_pu,_pv,_pD]]:B(_qm(_));};if(E(_pF)==237){if(_qj<128){return new F(function(){return _qs(_);});}else{if(_qj>159){return new F(function(){return _qs(_);});}else{if(_ql<128){return new F(function(){return _qs(_);});}else{return _ql>191?B(_qs(_)):B(_qm(_));}}}}else{return new F(function(){return _qs(_);});}};if(_pF<225){return new F(function(){return _qq(_);});}else{if(_pF>236){return new F(function(){return _qq(_);});}else{if(_qj<128){return new F(function(){return _qq(_);});}else{if(_qj>191){return new F(function(){return _qq(_);});}else{if(_ql<128){return new F(function(){return _qq(_);});}else{return _ql>191?B(_qq(_)):B(_qm(_));}}}}}};if(E(_pF)==224){if(_qj<160){return new F(function(){return _qo(_);});}else{if(_qj>191){return new F(function(){return _qo(_);});}else{if(_ql<128){return new F(function(){return _qo(_);});}else{return _ql>191?B(_qo(_)):B(_qm(_));}}}}else{return new F(function(){return _qo(_);});}}}}};if(_pF<192){return new F(function(){return _pG(_);});}else{if(_pF>223){return new F(function(){return _pG(_);});}else{if((_pq-_pC|0)>=2){var _qu=readOffAddr("w8",1,plusAddr(_pl,_pC+1|0),0),_qv=_qu,_=0;if(_qv>=128){if(_qv<192){var _=writeOffAddr("w32",4,_pr,_pD,(((_pF&4.294967295e9)-192|0)<<6)+((_qv&4.294967295e9)-128|0)|0),_=0,_qw=_pC+2|0,_qx=_pD+1|0;_pz=_qw;_pA=_qx;return null;}else{return [0,_i9,new T(function(){return _pC!=_pq?[0,_pl,_pm,_pn,_po,_pC,_pq]:E(_px);}),[0,_pr,_ps,_pt,_pu,_pv,_pD]];}}else{return [0,_i9,new T(function(){return _pC!=_pq?[0,_pl,_pm,_pn,_po,_pC,_pq]:E(_px);}),[0,_pr,_ps,_pt,_pu,_pv,_pD]];}}else{return [0,_dQ,new T(function(){return _pC!=_pq?[0,_pl,_pm,_pn,_po,_pC,_pq]:E(_px);}),[0,_pr,_ps,_pt,_pu,_pv,_pD]];}}}}else{var _=writeOffAddr("w32",4,_pr,_pD,_pF&4.294967295e9),_=0,_qw=_pC+1|0,_qx=_pD+1|0;_pz=_qw;_pA=_qx;return null;}}else{return [0,_dQ,new T(function(){return _pC!=_pq?[0,_pl,_pm,_pn,_po,_pC,_pq]:E(_px);}),[0,_pr,_ps,_pt,_pu,_pv,_pD]];}}else{return [0,_dS,new T(function(){return _pC!=_pq?[0,_pl,_pm,_pn,_po,_pC,_pq]:E(_px);}),[0,_pr,_ps,_pt,_pu,_pv,_pD]];}})(_pz,_pA,_);if(_pB!=null){return _pB;}}};return new F(function(){return _py(_pp,_pw,_);});},_qy=function(_qz,_qA,_){var _qB=E(_qz),_qC=E(_qA);return new F(function(){return _pk(_qB[1],_qB[2],_qB[3],_qB[4],_qB[5],_qB[6],_qC[1],_qC[2],_qC[3],_qC[4],_qC[5],_qC[6],_);});},_qD=new T(function(){return B(unCStr("UTF-8"));}),_qE=function(_qF){return [0,_qD,function(_){return [0,_qy,function(_qG,_qH,_){return new F(function(){return _hv(_qF,_qG,_qH,_);});},_pj,_pj,_ph];},function(_){return [0,_pc,function(_qG,_qH,_){return new F(function(){return _hW(_qF,_qG,_qH,_);});},_pj,_pj,_ph];}];},_qI=function(_qJ,_qK,_){var _qL=B(_dB(_qK));return !B(_dj(_qL,_dq))?!B(_dj(_qL,_dp))?!B(_dj(_qL,_do))?!B(_dj(_qL,_du))?!B(_dj(_qL,_dt))?!B(_dj(_qL,_ds))?!B(_dj(_qL,_dr))?B(_i2(_qJ,_qK,_)):new T(function(){return B(_qE(_qJ));}):new T(function(){return B(_oF(_qJ));}):new T(function(){return B(_ob(_qJ));}):new T(function(){return B(_nN(_qJ));}):new T(function(){return B(_lq(_qJ));}):new T(function(){return B(_kP(_qJ));}):new T(function(){return B(_kr(_qJ));});},_qM=function(_qN,_){var _qO=B((function(_qP,_){while(1){var _qQ=readOffAddr("i8",1,_qN,_qP),_qR=_qQ;if(!E(_qR)){return [0,_qP];}else{var _qS=_qP+1|0;_qP=_qS;continue;}}})(0,_)),_qT=_qO,_qU=E(_qT)[1];if(_qU>0){return new F(function(){return (function(_qV,_qW,_){while(1){var _qX=readOffAddr("i8",1,_qN,_qW),_qY=_qX;if(_qW>0){var _qZ=[1,[0,_qY>>>0&255&4.294967295e9],_qV],_r0=_qW-1|0;_qV=_qZ;_qW=_r0;continue;}else{return [1,[0,_qY>>>0&255&4.294967295e9],_qV];}}})(_k,_qU-1|0,_);});}else{return _k;}},_r1=function(_){var _=0,_r2=localeEncoding(),_r3=_r2;return new F(function(){return _qM(_r3,_);});},_r4=new T(function(){return B(_1r(_r1));}),_r5=function(_){var _=0;return new F(function(){return _qI(_di,_r4,_);});},_r6=new T(function(){return B(_1r(_r5));}),_r7=function(_){var _=0,_r8=nMV(_r6),_r9=_r8;return [0,function(_){return new F(function(){return rMV(_r9);});},function(_ra,_){var _=wMV(_r9,_ra);return _30;}];},_rb=new T(function(){return B(_1r(_r7));}),_ge=function(_rc,_rd,_re,_rf){return new F(function(){return _1r(function(_){var _=0,_rg=strerror(_rd),_rh=_rg,_ri=B(A(E(_rb)[1],[_])),_rj=_ri,_rk=B(_cx(_rj,_rh,_)),_rl=_rk;return [0,_re,new T(function(){switch(E(_rd)){case 1:var _rm=6;break;case 2:var _rm=1;break;case 3:var _rm=1;break;case 4:var _rm=18;break;case 5:var _rm=14;break;case 6:var _rm=1;break;case 7:var _rm=3;break;case 8:var _rm=12;break;case 9:var _rm=12;break;case 10:var _rm=1;break;case 11:var _rm=3;break;case 12:var _rm=3;break;case 13:var _rm=6;break;case 15:var _rm=12;break;case 16:var _rm=2;break;case 17:var _rm=0;break;case 18:var _rm=15;break;case 19:var _rm=15;break;case 20:var _rm=13;break;case 21:var _rm=13;break;case 22:var _rm=12;break;case 23:var _rm=3;break;case 24:var _rm=3;break;case 25:var _rm=5;break;case 26:var _rm=2;break;case 27:var _rm=6;break;case 28:var _rm=3;break;case 29:var _rm=15;break;case 30:var _rm=6;break;case 31:var _rm=3;break;case 32:var _rm=17;break;case 33:var _rm=12;break;case 34:var _rm=15;break;case 35:var _rm=2;break;case 36:var _rm=12;break;case 37:var _rm=3;break;case 38:var _rm=15;break;case 39:var _rm=8;break;case 40:var _rm=12;break;case 42:var _rm=1;break;case 43:var _rm=17;break;case 60:var _rm=12;break;case 61:var _rm=1;break;case 62:var _rm=16;break;case 63:var _rm=3;break;case 64:var _rm=1;break;case 66:var _rm=5;break;case 67:var _rm=17;break;case 69:var _rm=8;break;case 70:var _rm=17;break;case 71:var _rm=10;break;case 72:var _rm=15;break;case 74:var _rm=13;break;case 78:var _rm=17;break;case 84:var _rm=12;break;case 87:var _rm=3;break;case 88:var _rm=12;break;case 89:var _rm=12;break;case 90:var _rm=3;break;case 91:var _rm=10;break;case 92:var _rm=15;break;case 93:var _rm=10;break;case 94:var _rm=15;break;case 95:var _rm=15;break;case 96:var _rm=15;break;case 97:var _rm=15;break;case 98:var _rm=2;break;case 99:var _rm=15;break;case 100:var _rm=17;break;case 101:var _rm=1;break;case 102:var _rm=17;break;case 104:var _rm=17;break;case 105:var _rm=3;break;case 106:var _rm=0;break;case 107:var _rm=12;break;case 108:var _rm=5;break;case 109:var _rm=3;break;case 110:var _rm=16;break;case 111:var _rm=1;break;case 112:var _rm=1;break;case 113:var _rm=1;break;case 114:var _rm=0;break;case 115:var _rm=0;break;case 116:var _rm=17;break;case 122:var _rm=6;break;default:var _rm=11;}return _rm;}),_rc,_rl,[1,[0,_rd]],_rf];});});},_rn=new T(function(){return B(unCStr("gettimeofday"));}),_ro=function(_){var _rp=newByteArr(8),_rq=_rp,_rr=_rq,_rs=_rr,_rt=_rs,_=writeOffAddr("i32",4,_rt,0,0),_=writeOffAddr("i32",4,_rt,1,0),_ru=gettimeofday(_rt,0),_rv=_ru;if(E(_rv)==(-1)){var _rw=__hscore_get_errno(),_rx=_rw;return new F(function(){return _f5(B(_ge(_rn,_rx,_dR,_dR)),_);});}else{var _ry=readOffAddr("i32",4,_rt,0),_rz=_ry,_rA=readOffAddr("i32",4,_rt,1),_rB=_rA,_=0;return [0,[0,_rz],[0,_rB]];}},_rC=[0,660865024],_rD=[0,465661287],_rE=[1,_rD,_k],_rF=[1,_rC,_rE],_rG=new T(function(){return B(_74(_35,_rF));}),_rH=function(_){var _rI=B(_ro(_)),_rJ=_rI;return new T(function(){var _rK=E(_rJ);if(!B(_54(_rG,_ay))){var _rL=B(_5U(B(_65(B(_63(E(_rK[1])[1])),_7b)),B(_98(B(_65(B(_65(B(_63(E(_rK[2])[1])),_7b)),_7b)),_rG))));}else{var _rL=E(_53);}var _rM=_rL,_rN=_rM;return _rN;});},_rO=[0,12345],_rP=function(_){var _=0,_rQ=B(_rH(_)),_rR=_rQ,_rS=B(_7T(E(B(_b3(_rR))[2]),_6H,_7b,_6H)),_rT=_rS[2];if(!B(_54(_rT,_5m))){var _rU=B(_9C(_rS[1],_rT)),_rV=nMV(new T(function(){var _rW=B(_8g(B(_3q(B(_5U(B(_5U(B(_5U(B(_65(_rU[1],_rO)),_rU[2])),_b9)),_5m))))));return [0,_rW[1],_rW[2]];})),_rX=_rV;return [0,_rX];}else{return E(_53);}},_rY=new T(function(){return B(_1r(_rP));}),_rZ=function(_){var _s0=mMV(E(_rY)[1],_6B),_s1=_s0,_s2=E(_s1);return E(_s0);},_s3=function(_s4,_){var _s5=mMV(E(_rY)[1],function(_s6){var _s7=new T(function(){var _s8=B(A(new T(function(){var _s9=E(_s4);return function(_sa){var _sb=B(_6d(_3D,new T(function(){return B(_63(E(_s9[1])[1]));}),new T(function(){return B(_63(E(_s9[2])[1]));}),_sa));return [0,_sb[1],_sb[2]];};}),[_s6]));return [0,_s8[2],_s8[1]];}),_sc=new T(function(){return E(E(_s7)[1]);});return [0,_sc,new T(function(){var _sd=E(_sc);return E(E(_s7)[2]);})];}),_se=_s5,_sf=E(_se);return E(_s5);},_sg=function(_sh){return E(E(_sh)[2]);},_si=function(_sj){return E(E(_sj)[1]);},_sk=function(_sl,_sm,_sn,_so,_sp){while(1){var _sq=(function(_sr,_ss,_st,_su,_sv){if(!B(_5n(_st,_su))){var _sw=B(_5U(B(_5w(_su,_st)),_3G)),_sx=new T(function(){return B(A(_sg,[_sr,_sv]));}),_sy=new T(function(){return E(E(_sx)[1]);}),_sz=new T(function(){return B(_5U(B(_5w(B(_63(E(E(_sx)[2])[1])),B(_63(E(_sy)[1])))),_3G));}),_sA=B((function(_sB,_sC,_sD){while(1){if(!B(_5e(_sB,B(_65(_sw,_5v))))){var _sE=B(A(new T(function(){return B(_si(_sr));}),[_sD])),_sF=B(_65(_sB,_sz)),_sG=B(_5U(B(_65(_sC,_sz)),B(_5w(B(_63(E(_sE[1])[1])),new T(function(){return B(_63(E(_sy)[1]));})))));_sD=_sE[2];_sB=_sF;_sC=_sG;continue;}else{return [0,_sC,_sD];}}})(_3G,_5m,_sv));return [0,new T(function(){return B(A(_5c,[_ss,new T(function(){if(!B(_54(_sw,_5m))){var _sH=B(_5U(_st,B(_5M(_sA[1],_sw))));}else{var _sH=E(_53);}return _sH;})]));}),_sA[2]];}else{var _sI=_sr,_sJ=_ss,_sK=_su,_sL=_st,_sM=_sv;_sl=_sI;_sm=_sJ;_sn=_sK;_so=_sL;_sp=_sM;return null;}})(_sl,_sm,_sn,_so,_sp);if(_sq!=null){return _sq;}}},_sN=function(_sO,_sP){var _sQ=B(_sk(_sO,_3D,_3F,_3E,_sP));return [0,_sQ[1],_sQ[2]];},_sR=function(_sS,_sT,_sU){return function(_sV){var _sW=B(_sk(_sS,_3D,new T(function(){return B(_63(E(_sT)[1]));}),new T(function(){return B(_63(E(_sU)[1]));}),_sV));return [0,_sW[1],_sW[2]];};},_sX=function(_sY,_sZ){var _t0=E(_sZ);return new F(function(){return _sR(_sY,_t0[1],_t0[2]);});},_t1=function(_t2,_t3,_t4){var _t5=E(_t3),_t6=function(_t7){var _t8=B(_sk(_t2,_3D,new T(function(){return B(_63(E(_t5[1])[1]));}),new T(function(){return B(_63(E(_t5[2])[1]));}),_t7));return [1,E(_t8[1]),new T(function(){return B(_t6(_t8[2]));})];};return new F(function(){return _t6(_t4);});},_t9=function(_ta,_tb){var _tc=function(_td){var _te=B(_sk(_ta,_3D,_3F,_3E,_td));return [1,E(_te[1]),new T(function(){return B(_tc(_te[2]));})];};return new F(function(){return _tc(_tb);});},_tf=[0,_sX,_sN,_t1,_t9,_s3,_rZ],_tg=[0,0],_th=function(_ti,_tj,_tk,_tl){var _tm=E(_tl);if(!_tm){return new F(function(){return A(_tj,[_k]);});}else{return new F(function(){return A(_ti,[new T(function(){return B(A(_tk,[_tf,[0,_tg,[0,_tm]]]));}),function(_tn){return new F(function(){return A(_ti,[new T(function(){return B(_th(_ti,_tj,_tk,_tm-1|0));}),function(_to){return new F(function(){return A(_tj,[[1,_tn,_to]]);});}]);});}]);});}},_tp=function(_tq){return E(_tq);},_tr=function(_ts){return [1,[1,[0,_ts]],new T(function(){var _tt=E(_ts);if(_tt==13){var _tu=[0];}else{var _tu=B(_tr(_tt+1|0));}return _tu;})];},_tv=new T(function(){return B(_tr(1));}),_tw=[1,_dR,_tv],_tx=new T(function(){return B(_15(_tp,_tw));}),_ty=function(_tz){return _tz>1?[0,_tx,new T(function(){var _tA=B(_ty(_tz-1|0));return [1,_tA[1],_tA[2]];})]:[0,_tx,_k];},_tB=function(_tC,_tD,_){var _tE=B(A(_tC,[_])),_tF=_tE;return new F(function(){return A(_tD,[_tF,_]);});},_tG=function(_tH){var _tI=E(_tH);if(!_tI[0]){return [0];}else{return new F(function(){return _u(_tI[1],new T(function(){return B(_tG(_tI[2]));},1));});}},_tJ=function(_tK){return E(E(_tK)[5]);},_tL=function(_tM,_){return _tM;},_tN=new T(function(){return B(unCStr("[extractTree] impossible"));}),_tO=new T(function(){return B(err(_tN));}),_tP=function(_tQ,_tR){var _tS=function(_tT){var _tU=E(_tR);if(!_tU[0]){return E(_tO);}else{var _tV=_tU[1],_tW=_tU[3],_tX=E(_tU[2]);if(!_tX[0]){var _tY=new T(function(){var _tZ=B(_tP(_tQ-1|0,_tW));return [0,_tZ[1],_tZ[2]];});return [0,new T(function(){return E(E(_tY)[1]);}),new T(function(){return [1,_tV-1|0,E(_tX),E(E(E(_tY)[2]))];})];}else{var _u0=_tX[1],_u1=function(_u2){if(_tQ>=_u0){var _u3=new T(function(){var _u4=B(_tP(_tQ-_u0|0,_tW));return [0,_u4[1],_u4[2]];});return [0,new T(function(){return E(E(_u3)[1]);}),new T(function(){return [1,_tV-1|0,E(_tX),E(E(E(_u3)[2]))];})];}else{var _u5=new T(function(){var _u6=B(_tP(_tQ,_tX));return [0,_u6[1],_u6[2]];});return [0,new T(function(){return E(E(_u5)[1]);}),new T(function(){return [1,_tV-1|0,E(E(E(_u5)[2])),E(_tW)];})];}},_u7=E(_tW);if(!_u7[0]){return (_tQ+1|0)!=_tV?B(_u1(_)):[0,_u7[1],_tX];}else{return new F(function(){return _u1(_);});}}}};switch(E(_tQ)){case 0:var _u8=E(_tR);if(!_u8[0]){return new F(function(){return _tS(_);});}else{var _u9=E(_u8[2]);return _u9[0]==0?[0,_u9[1],_u8[3]]:B(_tS(_));}break;case 1:var _ua=E(_tR);if(!_ua[0]){return new F(function(){return _tS(_);});}else{if(E(_ua[1])==2){var _ub=E(_ua[2]);if(!_ub[0]){var _uc=E(_ua[3]);return _uc[0]==0?[0,_uc[1],_ub]:B(_tS(_));}else{return new F(function(){return _tS(_);});}}else{return new F(function(){return _tS(_);});}}break;default:return new F(function(){return _tS(_);});}},_ud=new T(function(){return B(unCStr("[shuffle] called with lists of different lengths"));}),_ue=new T(function(){return B(err(_ud));}),_uf=function(_ug,_uh){var _ui=function(_uj){var _uk=E(_uh);if(!_uk[0]){return E(_ue);}else{var _ul=new T(function(){var _um=B(_tP(E(_uk[1])[1],_ug));return [0,_um[1],_um[2]];});return [1,new T(function(){return E(E(_ul)[1]);}),new T(function(){return B(_uf(E(_ul)[2],_uk[2]));})];}},_un=E(_ug);return _un[0]==0?E(_uh)[0]==0?[1,_un[1],_k]:B(_ui(_)):B(_ui(_));},_uo=function(_up){var _uq=E(_up);if(!_uq[0]){return [0];}else{var _ur=_uq[1],_us=E(_uq[2]);if(!_us[0]){return [1,_ur,_k];}else{var _ut=E(_us[1]);return [1,new T(function(){var _uu=E(E(_ur));if(!_uu[0]){var _uv=E(_ut);if(!_uv[0]){var _uw=[1,2,E(_uu),E(_uv)];}else{var _uw=[1,_uv[1]+1|0,E(_uu),E(_uv)];}var _ux=_uw;}else{var _uy=_uu[1],_uz=E(_ut);if(!_uz[0]){var _uA=[1,_uy+1|0,E(_uu),E(_uz)];}else{var _uA=[1,_uy+_uz[1]|0,E(_uu),E(_uz)];}var _ux=_uA;}return _ux;}),new T(function(){return B(_uo(_us[2]));})];}}},_uB=new T(function(){return B(_uo(_k));}),_uC=new T(function(){return B(_uD(_uB));}),_uD=function(_uE){while(1){var _uF=E(_uE);if(!_uF[0]){return E(_uC);}else{if(!E(_uF[2])[0]){return E(_uF[1]);}else{_uE=B(_uo(_uF));continue;}}}},_uG=function(_){var _uH=B(_ty(4)),_uI=B(_tG([1,_uH[1],_uH[2]]));if(!_uI[0]){return _k;}else{var _uJ=B(A(_th,[_tB,_tL,_tJ,B(_23(_uI,0))-1|0,_])),_uK=_uJ;return new T(function(){return B(_uf(B(_uD(B(_15(_3b,_uI)))),_uK));});}},_uL=[1,_dR,_k],_uM=function(_uN){return _uN>1?[1,_dR,new T(function(){return B(_uM(_uN-1|0));})]:E(_uL);},_uO=new T(function(){return B(_uM(3));}),_uP=[1,_uO,_k],_uQ=function(_uR){return _uR>1?[1,_uO,new T(function(){return B(_uQ(_uR-1|0));})]:E(_uP);},_uS=new T(function(){return B(_uQ(5));}),_uT=[0,63],_uU=[1,_uT,_k],_uV=function(_uW){return E(_uU);},_uX=new T(function(){return B(unCStr("computers"));}),_uY=new T(function(){return B(unCStr("\u30b3\u30f3\u30d4\u30e5\u30fc\u30bf"));}),_uZ=new T(function(){return B(unCStr("yours"));}),_v0=new T(function(){return B(unCStr("\u3042\u306a\u305f"));}),_v1=function(_v2,_v3){var _v4=E(_v2);if(!_v4){return [0];}else{var _v5=E(_v3);return _v5[0]==0?[0]:[1,_v5[1],new T(function(){return B(_v1(_v4-1|0,_v5[2]));})];}},_v6=function(_){var _v7=B(_uG(_)),_v8=_v7,_v9=B(_uG(_)),_va=_v9;return [0,[0,[0,new T(function(){return B(_v1(3,_v8));}),new T(function(){return B(_36(3,_v8));}),_v0,_uZ,_L],[0,new T(function(){return B(_v1(3,_va));}),new T(function(){return B(_36(3,_va));}),_uY,_uX,_uV]],_35,_34,_uS];},_vb=function(_){var _vc=B(_v6(_)),_vd=_vc,_ve=newMVar(),_vf=_ve,_=putMVar(_vf,_vd),_vg=B(_31(_)),_vh=_vg,_vi=B(A(_2D,[_vd,_vh,_])),_vj=_vi;return _30;},_vk=function(_){return new F(function(){return _vb(_);});};
var hasteMain = function() {B(A(_vk, [0]));};window.onload = hasteMain;