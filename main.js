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

var _0=new T(function(){return B(unCStr("Prelude.(!!): negative index\n"));}),_1=new T(function(){return B(err(_0));}),_2=new T(function(){return B(unCStr("Prelude.(!!): index too large\n"));}),_3=new T(function(){return B(err(_2));}),_4=function(_5,_6){while(1){var _7=E(_5);if(!_7[0]){return E(_3);}else{var _8=E(_6);if(!_8){return E(_7[1]);}else{_5=_7[2];_6=_8-1|0;continue;}}}},_9=function(_a,_b){return E(_b);},_c=function(_d,_e,_){var _f=jsCreateTextNode(toJSStr(E(_d))),_g=_f,_h=jsAppendChild(_g,E(_e)[1]);return [0,_g];},_i=function(_j,_k,_){var _l=E(_j);if(!_l[0]){return _k;}else{var _m=B(A(_l[1],[_k,_])),_n=_m,_o=B(_i(_l[2],_k,_)),_p=_o;return _k;}},_q=new T(function(){return B(unCStr("\u884c\u52d5\u3092\u9078\u629e"));}),_r=new T(function(){return B(unCStr("\u30c9\u30ed\u30fc"));}),_s=new T(function(){return B(unCStr("\u3042\u306a\u305f\u306e\u52dd\u3061\u3067\u3059!"));}),_t=new T(function(){return B(unCStr("\u30b3\u30f3\u30d4\u30e5\u30fc\u30bf\u304c\u52dd\u3061!"));}),_u=new T(function(){return B(unCStr("\u624b\u756a\u3092\u4ea4\u4ee3"));}),_v=new T(function(){return B(unCStr("\u53ec\u559a\u3059\u308b\u4f4d\u7f6e\u3092\u9078\u629e"));}),_w=new T(function(){return B(unCStr("\u751f\u8d04\u3092\u9078\u629e: \u30a8\u30cd\u30eb\u30ae\u30fc\u304c\u3042\u3068"));}),_x=new T(function(){return B(unCStr("\u5fc5\u8981"));}),_y=new T(function(){return B(unCStr("\u30d1\u30b9\u3057\u307e\u3059"));}),_z=new T(function(){return B(unCStr("\u79fb\u52d5\u3059\u308b\u4f4d\u7f6e\u3092\u9078\u629e"));}),_A=new T(function(){return B(unCStr(" .deck"));}),_B=new T(function(){return B(unCStr("\u306e\u6b8b\u308a\u5c71\u672d: "));}),_C=[0,35],_D=new T(function(){return B(unCStr(" .hand"));}),_E=function(_F,_G){while(1){var _H=E(_F);if(!_H[0]){return E(_G);}else{_F=_H[2];var _I=_G+1|0;_G=_I;continue;}}},_J=function(_K,_L){var _M=E(_K);return _M[0]==0?E(_L):[1,_M[1],new T(function(){return B(_J(_M[2],_L));})];},_N=function(_O,_P){var _Q=jsShowI(_O),_R=_Q;return new F(function(){return _J(fromJSStr(_R),_P);});},_S=[0,41],_T=[0,40],_U=function(_V,_W,_X){if(_W>=0){return new F(function(){return _N(_W,_X);});}else{return _V<=6?B(_N(_W,_X)):[1,_T,new T(function(){var _Y=jsShowI(_W),_Z=_Y;return B(_J(fromJSStr(_Z),[1,_S,_X]));})];}},_10=[0],_11=new T(function(){return [0,"arr2lst"];}),_12=function(_13){var _14=B(A(_13,[_])),_15=_14;return E(_15);},_16=function(_17){return new F(function(){return _12(function(_){var _=0;return new F(function(){return eval(_17);});});});},_18=function(_19,_1a){return new F(function(){return _12(function(_){var _=0;return new F(function(){return A(_16,[E(_11)[1],E(_19),E(_1a),_]);});});});},_1b=new T(function(){return B(_16("(function(sel){return document.querySelectorAll(sel);})"));}),_1c=function(_1d,_1e,_1f,_){var _1g=B(A(_1b,[E(toJSStr(E(_1d))),_])),_1h=_1g,_1i=function(_1j,_){var _1k=E(_1j);if(!_1k[0]){return _10;}else{var _1l=B(A(_1e,[[0,_1k[1]],_])),_1m=_1l,_1n=B(_1i(_1k[2],_)),_1o=_1n;return [1,_1m,_1o];}},_1p=B(_1i(B(_18(_1h,0)),_)),_1q=_1p;return _1f;},_1r=new T(function(){return B(unCStr("li"));}),_1s=function(_1t,_1u,_1v,_){var _1w=jsCreateElem(toJSStr(E(_1r))),_1x=_1w,_1y=jsAppendChild(_1x,E(_1v)[1]),_1z=[0,_1x],_1A=B(A(_1t,[_1u,_1z,_])),_1B=_1A;return _1z;},_1C=function(_1D,_1E){var _1F=E(_1E);return _1F[0]==0?[0]:[1,new T(function(){return B(A(_1D,[_1F[1]]));}),new T(function(){return B(_1C(_1D,_1F[2]));})];},_1G=function(_1H){return function(_1I,_){var _1J=B(_1c([1,_C,new T(function(){return B(_J(E(_1H)[4],_A));})],function(_1K,_){var _1L=E(_1K),_1M=jsClearChildren(_1L[1]),_1N=B(_c(new T(function(){var _1O=E(_1H);return B(_J(_1O[3],new T(function(){return B(_J(_B,new T(function(){return B(_U(0,B(_E(_1O[2],0)),_10));},1)));},1)));}),_1L,_)),_1P=_1N;return _1L;},_1I,_)),_1Q=_1J,_1R=B(_1c([1,_C,new T(function(){return B(_J(E(_1H)[4],_D));})],function(_1S,_){var _1T=E(_1S),_1U=jsClearChildren(_1T[1]),_1V=B(_i(new T(function(){var _1W=E(_1H);return B(_1C(function(_1X){return function(_1Y,_1Z){return new F(function(){return _1s(_c,new T(function(){return B(A(_1W[5],[_1X]));}),_1Y,_1Z);});};},_1W[1]));}),_1T,_)),_20=_1V;return _1T;},_1I,_)),_21=_1R;return _1I;};},_22=new T(function(){return B(unCStr("Prelude.Enum.succ{Int}: tried to take `succ\' of maxBound"));}),_23=new T(function(){return B(err(_22));}),_24=function(_25){var _26=E(E(_25)[1]);return _26==2147483647?E(_23):[0,_26+1|0];},_27=[0],_28=new T(function(){return [0,"(function(e){ return e.previousSibling })"];}),_29=new T(function(){return B(_16("(function(x) {return x === null})"));}),_2a=new T(function(){return B(_16("(function(node) {return node.nodeType === 1})"));}),_2b=function(_2c,_){var _2d=E(_28)[1],_2e=B(A(_16,[_2d,E(_2c),_])),_2f=_2e,_2g=E(_2f),_2h=B(A(_29,[_2g,_])),_2i=_2h;if(_2i<=0){var _2j=B(A(_2a,[_2g,_])),_2k=_2j;if(_2k<=0){return new F(function(){return (function(_2l,_){while(1){var _2m=B(A(_16,[_2d,E(_2l),_])),_2n=_2m,_2o=E(_2n),_2p=B(A(_29,[_2o,_])),_2q=_2p;if(_2q<=0){var _2r=B(A(_2a,[_2o,_])),_2s=_2r;if(_2s<=0){_2l=_2n;continue;}else{return [1,[0,_2o]];}}else{return _27;}}})(_2f,_);});}else{return [1,[0,_2g]];}}else{return _27;}},_2t=function(_2u,_2v,_){while(1){var _2w=(function(_2x,_2y,_){var _2z=B(_2b(_2y,_)),_2A=_2z,_2B=E(_2A);if(!_2B[0]){return _2x;}else{_2u=new T(function(){return B(_24(_2x));});_2v=E(_2B[1])[1];return null;}})(_2u,_2v,_);if(_2w!=null){return _2w;}}},_2C=function(_2D,_2E){while(1){var _2F=E(_2D);if(!_2F){return E(_2E);}else{var _2G=E(_2E);if(!_2G[0]){return [0];}else{_2D=_2F-1|0;_2E=_2G[2];continue;}}}},_2H=function(_2I,_2J,_2K,_2L){return new F(function(){return A(_2J,[function(_2M){var _2N=E(_2I)[1],_2O=[1,_2M,new T(function(){var _2P=E(_2I)[1]+1|0;return _2P>=0?B(_2C(_2P,_2L)):E(_2L);})];if(_2N>0){var _2Q=function(_2R,_2S){var _2T=E(_2R);if(!_2T[0]){return E(_2O);}else{var _2U=_2T[1];return _2S>1?[1,_2U,new T(function(){return B(_2Q(_2T[2],_2S-1|0));})]:[1,_2U,_2O];}};return new F(function(){return _2Q(_2L,_2N);});}else{return E(_2O);}},new T(function(){return B(A(_2K,[new T(function(){var _2V=E(_2I)[1];return _2V>=0?B(_4(_2L,_2V)):E(_1);})]));})]);});},_2W=function(_2X){return E(_2X);},_2Y=function(_2Z){return E(E(_2Z)[1]);},_30=function(_31,_32,_33){while(1){var _34=E(_33);if(!_34[0]){return false;}else{if(!B(A(_2Y,[_31,_32,_34[1]]))){_33=_34[2];continue;}else{return true;}}}},_35=function(_36){var _37=E(_36);return [0,new T(function(){return [0,-1+E(_37[1])[1]|0];}),_37[2]];},_38=[1,_35,_10],_39=[0,0],_3a=[0,1],_3b=[0,_3a,_39],_3c=[1,_3b,_10],_3d=function(_3e,_3f){return [0,E(_3e)[1]+E(_3f)[1]|0];},_3g=function(_3h,_3i){var _3j=E(_3h),_3k=E(_3i);return [0,new T(function(){return B(_3d(_3j[1],_3k[1]));}),new T(function(){return B(_3d(_3j[2],_3k[2]));})];},_3l=new T(function(){return B(_1C(_3g,_3c));}),_3m=function(_3n,_3o,_3p,_3q,_3r,_3s){return !B(A(_3n,[_3p,_3r]))?true:!B(A(_2Y,[_3o,_3q,_3s]))?true:false;},_3t=function(_3u,_3v,_3w,_3x){var _3y=E(_3w),_3z=E(_3x);return new F(function(){return _3m(E(_3u)[1],_3v,_3y[1],_3y[2],_3z[1],_3z[2]);});},_3A=function(_3B,_3C,_3D,_3E,_3F,_3G){return !B(A(_3B,[_3D,_3F]))?false:B(A(_2Y,[_3C,_3E,_3G]));},_3H=function(_3I,_3J,_3K,_3L){var _3M=E(_3K),_3N=E(_3L);return new F(function(){return _3A(E(_3I)[1],_3J,_3M[1],_3M[2],_3N[1],_3N[2]);});},_3O=function(_3P,_3Q){return [0,function(_3R,_3S){return new F(function(){return _3H(_3P,_3Q,_3R,_3S);});},function(_3R,_3S){return new F(function(){return _3t(_3P,_3Q,_3R,_3S);});}];},_3T=function(_3U,_3V){return E(_3U)[1]==E(_3V)[1];},_3W=function(_3X,_3Y){return E(_3X)[1]!=E(_3Y)[1];},_3Z=[0,_3T,_3W],_40=new T(function(){return B(_3O(_3Z,_3Z));}),_41=function(_42,_43,_44,_45,_46){var _47=B(_2H(_43,_9,function(_48){return new F(function(){return _2H(_44,_9,_2W,_48);});},new T(function(){return E(E(_42)[4]);})));if(!_47[0]){return false;}else{var _49=E(_42),_4a=function(_4b){return new F(function(){return A(_4b,[[0,_43,_44]]);});},_4c=function(_4d){var _4e=B(_2H(_45,_9,function(_4f){return new F(function(){return _2H(_46,_9,_2W,_4f);});},_49[4]));if(!_4e[0]){return true;}else{var _4g=E(_47[1]),_4h=_4g[1],_4i=E(_4g[2])[1],_4j=E(_4e[1]),_4k=E(_4j[2])[1];return _4i>=_4k?_4i!=_4k?!E(_4j[1])?E(_4h):!E(_4h)?true:false:false:false;}};return !E(_49[2])?!B(_30(_40,[0,_45,_46],B(_1C(_4a,_3l))))?false:B(_4c(_)):!B(_30(_40,[0,_45,_46],B(_1C(_4a,_38))))?false:B(_4c(_));}},_4l=function(_4m){return new F(function(){return err(B(unAppCStr("Oops!  Entered absent arg ",new T(function(){return B(unCStr(_4m));}))));});},_4n=new T(function(){return B(_4l("ww_s8Qa{v} [lid] random-1.1:System.Random.StdGen{tc r15D}"));}),_4o=new T(function(){return B(_4l("ww_s8Q8{v} [lid] main:Game.BoardTrump.GameState.Phase{tc r1WV}"));}),_4p=new T(function(){return B(_4l("ww_s8Q6{v} [lid] (main:Game.BoardTrump.Player.Player{tc r1IO},\n                  main:Game.BoardTrump.Player.Player{tc r1IO})"));}),_4q=function(_4r,_4s,_4t,_4u){var _4v=[0,_4p,_4r,_4o,_4s,_4n],_4w=function(_4x){while(1){var _4y=(function(_4z){var _4A=E(_4z);if(!_4A[0]){return [0];}else{var _4B=_4A[2],_4C=new T(function(){return B(A(_4A[1],[[0,_4t,_4u]]));});if(!B(_41(_4v,_4t,_4u,new T(function(){return E(E(_4C)[1]);}),new T(function(){return E(E(_4C)[2]);})))){_4x=_4B;return null;}else{return [1,_4C,new T(function(){return B(_4w(_4B));})];}}})(_4x);if(_4y!=null){return _4y;}}};return !E(_4r)?B(_4w(_3l)):B((function(_4D){var _4E=new T(function(){return [0,-1+E(_4t)[1]|0];});return !B(_41(_4v,_4t,_4u,_4E,_4u))?B(_4w(_4D)):[1,[0,_4E,_4u],new T(function(){return B(_4w(_4D));})];})(_10));},_4F=function(_4G,_4H){while(1){var _4I=E(_4H);if(!_4I[0]){return E(_4G);}else{var _4J=_4G+E(_4I[1])[1]|0;_4H=_4I[2];_4G=_4J;continue;}}},_4K=[0,2],_4L=function(_4M){return E(E(_4M)[1])==1?E(_4K):E(_3a);},_4N=function(_4O,_4P){var _4Q=function(_4R){return E(_4O)==1?_4R<=B(_4F(-2,B(_1C(_4L,_4P)))):_4R<=B(_4F(-1,B(_1C(_4L,_4P))));};return _4O<=10?E(_4O)==1?B(_4Q(114514)):B(_4Q(0)):B(_4Q(2));},_4S=0,_4T=false,_4U=true,_4V=[0,75],_4W=[1,_4V,_10],_4X=[0,81],_4Y=[1,_4X,_10],_4Z=[0,74],_50=[1,_4Z,_10],_51=[0,65],_52=[1,_51,_10],_53=function(_54){var _55=E(_54);switch(_55){case 1:return E(_52);case 11:return E(_50);case 12:return E(_4Y);case 13:return E(_4W);default:return new F(function(){return _U(0,_55,_10);});}},_56=function(_57,_58,_59,_5a){return new F(function(){return A(_57,[function(_){var _5b=jsSetAttr(E(_58)[1],toJSStr(E(_59)),toJSStr(E(_5a)));return _4S;}]);});},_5c=new T(function(){return B(unCStr("td"));}),_5d=function(_5e,_5f,_5g,_){var _5h=jsCreateElem(toJSStr(E(_5c))),_5i=_5h,_5j=jsAppendChild(_5i,E(_5g)[1]),_5k=[0,_5i],_5l=B(A(_5e,[_5f,_5k,_])),_5m=_5l;return _5k;},_5n=function(_5o,_){return new F(function(){return _5d(_c,_10,_5o,_);});},_5p=function(_5q){return E(_5q);},_5r=new T(function(){return B(unCStr("class"));}),_5s=new T(function(){return B(unCStr("computers-card"));}),_5t=new T(function(){return B(unCStr("your-card"));}),_5u=function(_5v){var _5w=E(_5v);if(!_5w[0]){return E(_5n);}else{var _5x=E(_5w[1]);return function(_5y,_){var _5z=B(_5d(_c,new T(function(){return B(_53(E(_5x[2])[1]));}),_5y,_)),_5A=_5z,_5B=B(A(_56,[_5p,_5A,_5r,new T(function(){return !E(_5x[1])?E(_5s):E(_5t);}),_])),_5C=_5B;return _5A;};}},_5D=new T(function(){return B(unCStr("tr"));}),_5E=function(_5F,_5G,_5H,_){var _5I=jsCreateElem(toJSStr(E(_5D))),_5J=_5I,_5K=jsAppendChild(_5J,E(_5H)[1]),_5L=[0,_5J],_5M=B(A(_5F,[_5G,_5L,_])),_5N=_5M;return _5L;},_5O=function(_5P){return E(_5P);},_5Q=function(_5R){return function(_1Y,_1Z){return new F(function(){return _5E(_5O,function(_5S,_){return new F(function(){return _i(new T(function(){return B(_1C(_5u,_5R));}),_5S,_);});},_1Y,_1Z);});};},_5T=function(_5U,_){return _4S;},_5V=new T(function(){return B(unCStr("selectable-hand"));}),_5W=new T(function(){return B(_16("(function(e,c,x){x?e.classList.add(c):e.classList.remove(c);})"));}),_5X=function(_){var _=0;return new F(function(){return A(_16,["false",_]);});},_5Y=new T(function(){return B(_12(_5X));}),_5Z=function(_){var _=0;return new F(function(){return A(_16,["true",_]);});},_60=new T(function(){return B(_12(_5Z));}),_61=function(_62){return function(_63){return function(_64,_){var _65=B(A(new T(function(){return B(A(new T(function(){return B(A(_5W,[E(E(_62)[1])]));}),[E(toJSStr(E(_63)))]));}),[!E(_64)?E(_5Y):E(_60),_])),_66=_65;return _4S;};};},_67=function(_68,_){while(1){var _69=E(_68);if(!_69[0]){return _4S;}else{var _6a=B(A(_61,[_69[1],_5V,_4U,_])),_6b=_6a;_68=_69[2];continue;}}},_6c=new T(function(){return B(unCStr(": empty list"));}),_6d=new T(function(){return B(unCStr("Prelude."));}),_6e=function(_6f){return new F(function(){return err(B(_J(_6d,new T(function(){return B(_J(_6f,_6c));},1))));});},_6g=new T(function(){return B(unCStr("head"));}),_6h=new T(function(){return B(_6e(_6g));}),_6i=function(_6j){var _6k=E(_6j);if(!_6k[0]){return [0];}else{return new F(function(){return _J(_6k[1],new T(function(){return B(_6i(_6k[2]));},1));});}},_6l=new T(function(){return B(_16("(function(e){return e.parentNode;})"));}),_6m=new T(function(){return B(unCStr("td"));}),_6n=function(_6o,_6p){while(1){var _6q=(function(_6r,_6s){var _6t=E(_6s);if(!_6t[0]){return [0];}else{var _6u=_6t[1],_6v=_6t[2];if(!B(A(_6r,[_6u]))){var _6w=_6r;_6p=_6v;_6o=_6w;return null;}else{return [1,_6u,new T(function(){return B(_6n(_6r,_6v));})];}}})(_6o,_6p);if(_6q!=null){return _6q;}}},_6x=function(_6y,_6z,_6A,_6B){var _6C=E(_6A);if(!_6C[0]){return E(_6z);}else{var _6D=E(_6B);if(!_6D[0]){return E(_6z);}else{return new F(function(){return A(_6y,[_6C[1],_6D[1],new T(function(){return B(_6x(_6y,_6z,_6C[2],_6D[2]));})]);});}}},_6E=[0,0],_6F=function(_6G){return E(_6G)[0]==0?true:false;},_6H=function(_6I,_6J){while(1){var _6K=E(_6J);if(!_6K[0]){return E(_6I);}else{_6I=_6K[1];_6J=_6K[2];continue;}}},_6L=new T(function(){return B(unCStr("last"));}),_6M=new T(function(){return B(_6e(_6L));}),_6N=new T(function(){return B(unCStr("table#field"));}),_6O=new T(function(){return B(unCStr("movable-card"));}),_6P=new T(function(){return B(unCStr("id"));}),_6Q=new T(function(){return B(unCStr("moving-subject"));}),_6R=new T(function(){return B(unCStr("motion-scope"));}),_6S=new T(function(){return B(unCStr("obj-of-summon"));}),_6T=new T(function(){return B(unCStr("\u306e\u756a\u3067\u3059\u3001"));}),_6U=new T(function(){return B(unCStr(" ol.hand li"));}),_6V=function(_6W,_6X,_6Y,_){if(!E(_6X)){return new F(function(){return A(_6Y,[_]);});}else{var _6Z=B(A(_61,[_6W,_5V,_4U,_])),_70=_6Z;return new F(function(){return A(_6Y,[_]);});}},_71=function(_){return _4S;},_72=new T(function(){return B(unCStr("summonable-zone"));}),_73=function(_74,_75,_76,_){if(!E(_75)){return new F(function(){return A(_76,[_]);});}else{var _77=B(A(_56,[_5p,_74,_5r,_72,_])),_78=_77;return new F(function(){return A(_76,[_]);});}},_79=new T(function(){return B(unCStr("#status"));}),_7a=[0,35],_7b=new T(function(){return B(unCStr("#field tr"));}),_7c=[0,0],_7d=function(_7e,_){return _7e;},_7f=function(_7g){return function(_1Y,_1Z){return new F(function(){return _i([1,function(_5o,_){return new F(function(){return _1c(_6N,function(_7h,_){var _7i=E(_7h),_7j=jsClearChildren(_7i[1]),_7k=B(_i(new T(function(){return B(_1C(_5Q,E(_7g)[4]));}),_7i,_)),_7l=_7k;return _7i;},_5o,_);});},[1,function(_5o,_){return new F(function(){return _1c(_79,function(_7m,_){var _7n=E(_7m),_7o=jsClearChildren(_7n[1]),_7p=B(A(new T(function(){var _7q=E(_7g),_7r=_7q[1],_7s=E(_7q[3]);if(_7s[0]==7){var _7t=function(_1Y,_1Z){return new F(function(){return _c(new T(function(){return !E(_7s[1])?E(_t):E(_s);}),_1Y,_1Z);});};}else{var _7t=function(_1Y,_1Z){return new F(function(){return _c(new T(function(){return B(unAppCStr("-- ",new T(function(){var _7u=new T(function(){return B(_J(_6T,new T(function(){var _7v=E(_7s);switch(_7v[0]){case 0:var _7w=E(_r);break;case 1:var _7w=E(_q);break;case 2:var _7w=E(_z);break;case 3:var _7w=E(_y);break;case 4:var _7x=function(_7y){var _7z=E(_7y);return _7z[0]==0?E(new T(function(){return B(_J(B(_U(0,E(_7v[1])[1],_10)),_x));})):[1,_7z[1],new T(function(){return B(_7x(_7z[2]));})];},_7w=B(_7x(_w));break;case 5:var _7w=E(_v);break;default:var _7w=E(_u);}return _7w;},1)));},1);if(!E(_7q[2])){var _7A=B(_J(E(E(_7r)[2])[3],_7u));}else{var _7A=B(_J(E(E(_7r)[1])[3],_7u));}return _7A;})));}),_1Y,_1Z);});};}var _7B=_7t;return _7B;}),[_7n,_])),_7C=_7p;return _7n;},_5o,_);});},[1,function(_7D,_){var _7E=E(new T(function(){var _7F=E(E(_7g)[1]);return [0,new T(function(){return B(_1G(_7F[1]));}),new T(function(){return B(_1G(_7F[2]));})];})),_7G=B(A(_7E[1],[_7D,_])),_7H=_7G,_7I=B(A(_7E[2],[_7D,_])),_7J=_7I;return _7D;},[1,new T(function(){var _7K=E(_7g),_7L=_7K[1],_7M=_7K[2],_7N=_7K[4],_7O=E(_7K[3]);switch(_7O[0]){case 1:var _7P=function(_7Q){var _7R=E(_7Q);if(!_7R[0]){return E(_5T);}else{var _7S=function(_7T){var _7U=E(_7T);if(!_7U[0]){return E(new T(function(){return B(_7P(_7R[2]));}));}else{var _7V=new T(function(){return B(_7S(_7U[2]));});return function(_7W,_){var _7X=E(_7W);if(!_7X[0]){return _4S;}else{var _7Y=_7X[2],_7Z=E(_7X[1]);if(!_7Z[0]){return new F(function(){return A(_7V,[_7Y,_]);});}else{var _80=E(_7U[1]),_81=_80[1],_82=B(A(_6l,[E(_81),_])),_83=_82,_84=B(_2t(_6E,_83,_)),_85=_84,_86=B(_2t(_7c,_81,_)),_87=_86;if(!E(E(_7Z[1])[1])){if(!E(_7M)){if(!B(_4q(_4T,_7N,_85,_87))[0]){return new F(function(){return A(_7V,[_7Y,_]);});}else{var _88=B(A(_61,[_80,_6O,_4U,_])),_89=_88;return new F(function(){return A(_7V,[_7Y,_]);});}}else{return new F(function(){return A(_7V,[_7Y,_]);});}}else{if(!E(_7M)){return new F(function(){return A(_7V,[_7Y,_]);});}else{if(!B(_4q(_4U,_7N,_85,_87))[0]){return new F(function(){return A(_7V,[_7Y,_]);});}else{var _8a=B(A(_61,[_80,_6O,_4U,_])),_8b=_8a;return new F(function(){return A(_7V,[_7Y,_]);});}}}}}};}};return new F(function(){return _7S(_7R[1]);});}},_8c=function(_8d,_){var _8e=E(_8d),_8f=_8e[1],_8g=jsQuerySelectorAll(_8f,toJSStr([1,_7a,new T(function(){if(!E(_7M)){var _8h=B(_J(E(E(_7L)[2])[4],_6U));}else{var _8h=B(_J(E(E(_7L)[1])[4],_6U));}return _8h;})])),_8i=_8g,_8j=B(A(_6x,[_6V,_71,_8i,new T(function(){var _8k=function(_8l){return new F(function(){return (function(_8m){if(!E(_7M)){return new F(function(){return _4N(_8m,E(E(_7L)[2])[1]);});}else{return new F(function(){return _4N(_8m,E(E(_7L)[1])[1]);});}})(E(_8l)[1]);});};if(!E(_7M)){var _8n=B(_1C(_8k,E(E(_7L)[2])[1]));}else{var _8n=B(_1C(_8k,E(E(_7L)[1])[1]));}return _8n;}),_])),_8o=_8j,_8p=jsQuerySelectorAll(_8f,toJSStr(E(_7b))),_8q=_8p,_8r=E(_8q);if(!_8r[0]){return _8e;}else{var _8s=E(_6m),_8t=jsQuerySelectorAll(E(_8r[1])[1],toJSStr(_8s)),_8u=_8t,_8v=function(_8w,_){var _8x=E(_8w);if(!_8x[0]){return _10;}else{var _8y=jsQuerySelectorAll(E(_8x[1])[1],toJSStr(_8s)),_8z=_8y,_8A=B(_8v(_8x[2],_)),_8B=_8A;return [1,_8z,_8B];}},_8C=B(_8v(_8r[2],_)),_8D=_8C,_8E=B(A(function(_8F,_8G){var _8H=function(_8I){var _8J=E(_8I);if(!_8J[0]){return E(new T(function(){return B(_7P(_8G));}));}else{var _8K=new T(function(){return B(_8H(_8J[2]));});return function(_8L,_){var _8M=E(_8L);if(!_8M[0]){return _4S;}else{var _8N=_8M[2],_8O=E(_8M[1]);if(!_8O[0]){return new F(function(){return A(_8K,[_8N,_]);});}else{var _8P=E(_8J[1]),_8Q=_8P[1],_8R=B(A(_6l,[E(_8Q),_])),_8S=_8R,_8T=B(_2t(_6E,_8S,_)),_8U=_8T,_8V=B(_2t(_7c,_8Q,_)),_8W=_8V;if(!E(E(_8O[1])[1])){if(!E(_7M)){if(!B(_4q(_4T,_7N,_8U,_8W))[0]){return new F(function(){return A(_8K,[_8N,_]);});}else{var _8X=B(A(_61,[_8P,_6O,_4U,_])),_8Y=_8X;return new F(function(){return A(_8K,[_8N,_]);});}}else{return new F(function(){return A(_8K,[_8N,_]);});}}else{if(!E(_7M)){return new F(function(){return A(_8K,[_8N,_]);});}else{if(!B(_4q(_4U,_7N,_8U,_8W))[0]){return new F(function(){return A(_8K,[_8N,_]);});}else{var _8Z=B(A(_61,[_8P,_6O,_4U,_])),_90=_8Z;return new F(function(){return A(_8K,[_8N,_]);});}}}}}};}};return new F(function(){return _8H(_8F);});},[_8u,_8D,new T(function(){return B(_6i(_7N));}),_])),_91=_8E;return _8e;}};break;case 2:var _92=_7O[1],_8c=function(_93,_){var _94=E(_93),_95=jsQuerySelectorAll(_94[1],toJSStr(E(_7b))),_96=_95,_97=_96,_98=E(_92),_99=E(_98[1])[1];if(_99>=0){var _9a=E(_6m),_9b=jsQuerySelectorAll(B(_4(_97,_99))[1],toJSStr(_9a)),_9c=_9b,_9d=E(_98[2])[1];if(_9d>=0){var _9e=jsSetAttr(B(_4(_9c,_9d))[1],toJSStr(E(_6P)),toJSStr(E(_6Q))),_9f=function(_,_9g){var _9h=B((function(_9i,_){while(1){var _9j=(function(_9k,_){var _9l=E(_9k);if(!_9l[0]){return _4S;}else{var _9m=_9l[1],_9n=B(A(_61,[new T(function(){return B(_2H(new T(function(){return E(B(A(_9m,[_98]))[1]);}),_9,function(_9o){return new F(function(){return _2H(new T(function(){return E(B(A(_9m,[_98]))[2]);}),_9,_2W,_9o);});},_9g));},1),_6R,_4U,_])),_9p=_9n;_9i=_9l[2];return null;}})(_9i,_);if(_9j!=null){return _9j;}}})(new T(function(){var _9q=function(_9r){var _9s=new T(function(){return B(A(_9r,[_92]));});return new F(function(){return _41(_7K,new T(function(){return E(E(_92)[1]);}),new T(function(){return E(E(_92)[2]);}),new T(function(){return E(E(_9s)[1]);}),new T(function(){return E(E(_9s)[2]);}));});};return !E(_7M)?B(_6n(_9q,_3l)):B(_6n(_9q,_38));}),_)),_9t=_9h;return _94;},_9u=E(_97);if(!_9u[0]){return new F(function(){return _9f(_,_10);});}else{var _9v=jsQuerySelectorAll(E(_9u[1])[1],toJSStr(_9a)),_9w=_9v,_9x=function(_9y,_){var _9z=E(_9y);if(!_9z[0]){return _10;}else{var _9A=jsQuerySelectorAll(E(_9z[1])[1],toJSStr(_9a)),_9B=_9A,_9C=B(_9x(_9z[2],_)),_9D=_9C;return [1,_9B,_9D];}},_9E=B(_9x(_9u[2],_)),_9F=_9E;return new F(function(){return _9f(_,[1,_9w,_9F]);});}}else{return E(_1);}}else{return E(_1);}};break;case 4:var _8c=function(_9G,_){var _9H=E(_9G),_9I=jsQuerySelectorAll(_9H[1],toJSStr([1,_7a,new T(function(){if(!E(_7M)){var _9J=B(_J(E(E(_7L)[2])[4],_6U));}else{var _9J=B(_J(E(E(_7L)[1])[4],_6U));}return _9J;})])),_9K=_9I,_9L=B(_67(_9K,_)),_9M=_9L;return _9H;};break;case 5:var _8c=function(_9N,_){var _9O=E(_9N),_9P=_9O[1],_9Q=jsQuerySelectorAll(_9P,toJSStr([1,_7a,new T(function(){if(!E(_7M)){var _9R=B(_J(E(E(_7L)[2])[4],_6U));}else{var _9R=B(_J(E(E(_7L)[1])[4],_6U));}return _9R;})])),_9S=_9Q,_9T=E(_7O[1])[1];if(_9T>=0){var _9U=jsSetAttr(B(_4(_9S,_9T))[1],toJSStr(E(_6P)),toJSStr(E(_6S))),_9V=jsQuerySelectorAll(_9P,toJSStr(E(_7b))),_9W=_9V,_9X=_9W,_9Y=function(_9Z){var _a0=jsQuerySelectorAll(_9Z,toJSStr(E(_6m))),_a1=_a0,_a2=B(A(_6x,[_73,_71,_a1,new T(function(){if(!E(_7M)){var _a3=E(_7N),_a4=_a3[0]==0?E(_6h):B(_1C(_6F,_a3[1]));}else{var _a5=E(_7N);if(!_a5[0]){var _a6=E(_6M);}else{var _a6=B(_1C(_6F,B(_6H(_a5[1],_a5[2]))));}var _a4=_a6;}return _a4;}),_])),_a7=_a2;return _9O;};if(!E(_7M)){var _a8=E(_9X);if(!_a8[0]){return E(_6h);}else{return new F(function(){return _9Y(E(_a8[1])[1]);});}}else{var _a9=E(_9X);if(!_a9[0]){return E(_6M);}else{return new F(function(){return _9Y(B(_6H(_a9[1],_a9[2]))[1]);});}}}else{return E(_1);}};break;default:var _8c=E(_7d);}var _aa=_8c;return _aa;}),_10]]]],_1Y,_1Z);});};},_ab=[1],_ac=function(_ad,_ae){return new F(function(){return A(_ad,[_ae]);});},_af=[6],_ag=[7,_4T],_ah=[7,_4U],_ai=function(_aj){return [0];},_ak=function(_al,_am,_an,_ao,_ap){if(!B(_41(_ap,_al,_am,_an,_ao))){return [0];}else{var _aq=B(_2H(_al,_9,function(_ar){return new F(function(){return _2H(_am,_9,_2W,_ar);});},new T(function(){return E(E(_ap)[4]);})));return _aq[0]==0?[0]:[1,new T(function(){var _as=E(_ap),_at=_as[1],_au=_as[4],_av=_as[5],_aw=new T(function(){return B(_2H(_al,_ac,function(_ax){return new F(function(){return _2H(_am,_ac,_ai,_ax);});},new T(function(){return B(_2H(_an,_ac,function(_ay){return new F(function(){return _2H(_ao,_ac,function(_az){return [1,_aq[1]];},_ay);});},_au));})));});if(!E(_as[2])){var _aA=(E(_an)[1]+1|0)!=B(_E(_au,0))?[0,_at,_4T,_af,_aw,_av]:[0,_at,_4T,_ag,_aw,_av];}else{var _aA=E(E(_an)[1])==0?[0,_at,_4U,_ah,_aw,_av]:[0,_at,_4U,_af,_aw,_av];}var _aB=_aA;return _aB;})];}},_aC=function(_aD,_aE,_aF){var _aG=B(_2H(_aD,_9,function(_aH){return new F(function(){return _2H(_aE,_9,_2W,_aH);});},new T(function(){return E(E(_aF)[4]);})));if(!_aG[0]){return [0];}else{var _aI=_aG[1],_aJ=E(_aF),_aK=_aJ[2],_aL=_aJ[4],_aM=function(_aN){return B(_4q(_aK,_aL,_aD,_aE))[0]==0?[0]:[1,[0,_aJ[1],_aK,[2,[0,_aD,_aE]],_aL,_aJ[5]]];};return !E(_aK)?!E(E(_aI)[1])?B(_aM(_)):[0]:!E(E(_aI)[1])?[0]:B(_aM(_));}},_aO=[0,114514],_aP=function(_aQ,_aR){var _aS=new T(function(){var _aT=_aQ+1|0;return _aT>=0?B(_2C(_aT,_aR)):E(_aR);});if(_aQ>0){var _aU=function(_aV,_aW){var _aX=E(_aV);if(!_aX[0]){return E(_aS);}else{var _aY=_aX[1];return _aW>1?[1,_aY,new T(function(){return B(_aU(_aX[2],_aW-1|0));})]:[1,_aY,_aS];}};return new F(function(){return _aU(_aR,_aQ);});}else{return E(_aS);}},_aZ=function(_b0,_b1){return new F(function(){return _aP(E(_b0)[1],_b1);});},_b2=function(_b3,_b4){var _b5=E(_b4);return _b5[0]==0?[0]:[1,_b3,new T(function(){return B(_b2(_b5[1],_b5[2]));})];},_b6=new T(function(){return B(unCStr("init"));}),_b7=new T(function(){return B(_6e(_b6));}),_b8=new T(function(){return B(unCStr("tail"));}),_b9=new T(function(){return B(_6e(_b8));}),_ba=function(_bb,_bc,_bd,_be){return B(_2H(_bc,_9,_2W,new T(function(){var _bf=E(_be),_bg=_bf[4];if(!E(_bf[2])){var _bh=E(_bg),_bi=_bh[0]==0?E(_6h):E(_bh[1]);}else{var _bj=E(_bg),_bi=_bj[0]==0?E(_6M):B(_6H(_bj[1],_bj[2]));}var _bk=_bi;return _bk;})))[0]==0?[1,new T(function(){var _bl=E(_bd),_bm=_bl[1],_bn=function(_bo){var _bp=E(_be),_bq=_bp[1],_br=_bp[2],_bs=_bp[4],_bt=_bp[5],_bu=new T(function(){var _bv=new T(function(){return B(_2H(_bc,_ac,function(_bw){return E([1,[0,_br,_bl]]);},new T(function(){if(!E(_br)){var _bx=E(_bs),_by=_bx[0]==0?E(_6h):E(_bx[1]);}else{var _bz=E(_bs),_by=_bz[0]==0?E(_6M):B(_6H(_bz[1],_bz[2]));}return _by;})));});if(!E(_br)){var _bA=[1,_bv,new T(function(){var _bB=E(_bs);return _bB[0]==0?E(_b9):E(_bB[2]);})];}else{var _bC=E(_bs);if(!_bC[0]){var _bD=E(_b7);}else{var _bD=B(_J(B(_b2(_bC[1],_bC[2])),[1,_bv,_10]));}var _bA=_bD;}return _bA;}),_bE=new T(function(){if(!E(_br)){var _bF=E(_bq),_bG=[0,_bF[1],new T(function(){var _bH=E(_bF[2]);return [0,new T(function(){return B(_aZ(_bb,_bH[1]));}),_bH[2],_bH[3],_bH[4],_bH[5]];})];}else{var _bI=E(_bq),_bG=[0,new T(function(){var _bJ=E(_bI[1]);return [0,new T(function(){return B(_aZ(_bb,_bJ[1]));}),_bJ[2],_bJ[3],_bJ[4],_bJ[5]];}),_bI[2]];}return _bG;});return E(_bo)==0?[0,_bE,_br,_af,_bu,_bt]:[0,_bE,_br,[4,new T(function(){return _bm<=10?E(_bm)==1?E(_aO):E(_39):E(_4K);})],_bu,_bt];};if(_bm<=10){if(E(_bm)==1){var _bK=B(_bn(114514)),_bL=[0,_bK[1],_bK[2],_bK[3],_bK[4],_bK[5]];}else{var _bM=B(_bn(0)),_bL=[0,_bM[1],_bM[2],_bM[3],_bM[4],_bM[5]];}var _bN=_bL;}else{var _bO=B(_bn(2)),_bN=[0,_bO[1],_bO[2],_bO[3],_bO[4],_bO[5]];}var _bP=_bN,_bQ=_bP;return _bQ;})]:[0];},_bR=function(_bS,_bT,_bU,_bV,_bW,_bX,_bY){var _bZ=[0,_bU,_bV,_bW,_bX,_bY],_c0=E(_bW);switch(_c0[0]){case 1:var _c1=B(_aC(_bS,_bT,_bZ));return _c1[0]==0?E(_bZ):E(_c1[1]);case 2:var _c2=E(_c0[1]),_c3=_c2[2],_c4=E(_c2[1]),_c5=E(_bS);if(_c4[1]!=_c5[1]){var _c6=B(_ak(_c4,_c3,_c5,_bT,_bZ));return _c6[0]==0?E(_bZ):E(_c6[1]);}else{var _c7=E(_c3),_c8=E(_bT);if(_c7[1]!=_c8[1]){var _c9=B(_ak(_c4,_c7,_c5,_c8,_bZ));return _c9[0]==0?E(_bZ):E(_c9[1]);}else{return [0,_bU,_bV,_ab,_bX,_bY];}}break;case 5:var _ca=_c0[1],_cb=function(_cc){var _cd=function(_ce){var _cf=B(_ba(_ca,_bT,new T(function(){var _cg=E(_ca)[1];if(_cg>=0){if(!E(_bV)){var _ch=B(_4(E(E(_bU)[2])[1],_cg));}else{var _ch=B(_4(E(E(_bU)[1])[1],_cg));}var _ci=_ch;}else{var _ci=E(_1);}var _cj=_ci,_ck=_cj;return _ck;},1),_bZ));return _cf[0]==0?E(_bZ):E(_cf[1]);};if(!E(_bV)){return E(E(_bS)[1])==0?B(_cd(_)):E(_bZ);}else{return new F(function(){return _cd(_);});}};if(!E(_bV)){return new F(function(){return _cb(_);});}else{return E(_bS)[1]!=(B(_E(_bX,0))-1|0)?E(_bZ):B(_cb(_));}break;default:return E(_bZ);}},_cl=new T(function(){return B(unCStr("Prelude.Enum.pred{Int}: tried to take `pred\' of minBound"));}),_cm=new T(function(){return B(err(_cl));}),_cn=function(_co,_cp){return [0,imul(E(_co)[1],E(_cp)[1])|0];},_cq=function(_cr,_cs){return [0,E(_cr)[1]-E(_cs)[1]|0];},_ct=function(_cu){var _cv=E(_cu),_cw=_cv[1];return _cw<0?[0, -_cw]:E(_cv);},_cx=function(_cy){var _cz=E(_cy);return _cz[0]==0?E(_cz[1]):I_toInt(_cz[1]);},_cA=function(_cB){return [0,B(_cx(_cB))];},_cC=function(_cD){return [0, -E(_cD)[1]];},_cE=[0,-1],_cF=[0,0],_cG=[0,1],_cH=function(_cI){var _cJ=E(_cI)[1];return _cJ>=0?E(_cJ)==0?E(_cF):E(_cG):E(_cE);},_cK=[0,_3d,_cn,_cq,_cC,_ct,_cH,_cA],_cL=[0,1],_cM=function(_cN,_cO){return [0,E(_cN)[1],E(_cO)[1]];},_cP=function(_cQ,_cR){var _cS=quot(_cR,52774),_cT=(imul(40692,_cR-(imul(_cS,52774)|0)|0)|0)-(imul(_cS,3791)|0)|0,_cU=new T(function(){if(_cT>=0){var _cV=[0,_cT];}else{var _cV=[0,_cT+2147483399|0];}var _cW=_cV;return _cW;}),_cX=quot(_cQ,53668),_cY=(imul(40014,_cQ-(imul(_cX,53668)|0)|0)|0)-(imul(_cX,12211)|0)|0,_cZ=new T(function(){if(_cY>=0){var _d0=[0,_cY];}else{var _d0=[0,_cY+2147483563|0];}var _d1=_d0;return _d1;});return [0,new T(function(){var _d2=E(_cZ)[1]-E(_cU)[1]|0;if(_d2>=1){var _d3=[0,_d2];}else{var _d3=[0,_d2+2147483562|0];}var _d4=_d3,_d5=_d4,_d6=_d5,_d7=_d6;return _d7;}),new T(function(){return B(_cM(_cZ,_cU));})];},_d8=[0,2147483562],_d9=new T(function(){return B(unCStr("base"));}),_da=new T(function(){return B(unCStr("GHC.Exception"));}),_db=new T(function(){return B(unCStr("ArithException"));}),_dc=[0,I_fromBits([4194982440,719304104]),I_fromBits([3110813675,1843557400]),_d9,_da,_db],_dd=[0,I_fromBits([4194982440,719304104]),I_fromBits([3110813675,1843557400]),_dc,_10],_de=function(_df){return E(_dd);},_dg=function(_dh){return E(E(_dh)[1]);},_di=function(_dj,_dk,_dl){var _dm=B(A(_dj,[_])),_dn=B(A(_dk,[_])),_do=hs_eqWord64(_dm[1],_dn[1]),_dp=_do;if(!E(_dp)){return [0];}else{var _dq=hs_eqWord64(_dm[2],_dn[2]),_dr=_dq;return E(_dr)==0?[0]:[1,_dl];}},_ds=function(_dt){var _du=E(_dt);return new F(function(){return _di(B(_dg(_du[1])),_de,_du[2]);});},_dv=new T(function(){return B(unCStr("arithmetic underflow"));}),_dw=new T(function(){return B(unCStr("arithmetic overflow"));}),_dx=new T(function(){return B(unCStr("Ratio has zero denominator"));}),_dy=new T(function(){return B(unCStr("denormal"));}),_dz=new T(function(){return B(unCStr("divide by zero"));}),_dA=new T(function(){return B(unCStr("loss of precision"));}),_dB=function(_dC){switch(E(_dC)){case 0:return E(_dw);case 1:return E(_dv);case 2:return E(_dA);case 3:return E(_dz);case 4:return E(_dy);default:return E(_dx);}},_dD=function(_dE){return new F(function(){return _J(_dv,_dE);});},_dF=function(_dE){return new F(function(){return _J(_dw,_dE);});},_dG=function(_dE){return new F(function(){return _J(_dx,_dE);});},_dH=function(_dE){return new F(function(){return _J(_dy,_dE);});},_dI=function(_dE){return new F(function(){return _J(_dz,_dE);});},_dJ=function(_dE){return new F(function(){return _J(_dA,_dE);});},_dK=function(_dL){switch(E(_dL)){case 0:return E(_dF);case 1:return E(_dD);case 2:return E(_dJ);case 3:return E(_dI);case 4:return E(_dH);default:return E(_dG);}},_dM=[0,44],_dN=[0,93],_dO=[0,91],_dP=function(_dQ,_dR,_dS){var _dT=E(_dR);return _dT[0]==0?B(unAppCStr("[]",_dS)):[1,_dO,new T(function(){return B(A(_dQ,[_dT[1],new T(function(){var _dU=function(_dV){var _dW=E(_dV);return _dW[0]==0?E([1,_dN,_dS]):[1,_dM,new T(function(){return B(A(_dQ,[_dW[1],new T(function(){return B(_dU(_dW[2]));})]));})];};return B(_dU(_dT[2]));})]));})];},_dX=function(_dY,_dZ){return new F(function(){return _dP(_dK,_dY,_dZ);});},_e0=function(_e1,_e2){switch(E(_e2)){case 0:return E(_dF);case 1:return E(_dD);case 2:return E(_dJ);case 3:return E(_dI);case 4:return E(_dH);default:return E(_dG);}},_e3=[0,_e0,_dB,_dX],_e4=new T(function(){return [0,_de,_e3,_e5,_ds];}),_e5=function(_dE){return [0,_e4,_dE];},_e6=3,_e7=new T(function(){return B(_e5(_e6));}),_e8=new T(function(){return die(_e7);}),_e9=function(_ea,_eb){var _ec=E(_ea);if(!_ec[0]){var _ed=_ec[1],_ee=E(_eb);return _ee[0]==0?_ed==_ee[1]:I_compareInt(_ee[1],_ed)==0?true:false;}else{var _ef=_ec[1],_eg=E(_eb);return _eg[0]==0?I_compareInt(_ef,_eg[1])==0?true:false:I_compare(_ef,_eg[1])==0?true:false;}},_eh=function(_ei){return E(E(_ei)[7]);},_ej=function(_ek,_el){var _em=E(_ek);if(!_em[0]){var _en=_em[1],_eo=E(_el);return _eo[0]==0?_en>=_eo[1]:I_compareInt(_eo[1],_en)<=0;}else{var _ep=_em[1],_eq=E(_el);return _eq[0]==0?I_compareInt(_ep,_eq[1])>=0:I_compare(_ep,_eq[1])>=0;}},_er=[0,0],_es=function(_et,_eu){var _ev=E(_et);if(!_ev[0]){var _ew=_ev[1],_ex=E(_eu);return _ex[0]==0?_ew>_ex[1]:I_compareInt(_ex[1],_ew)<0;}else{var _ey=_ev[1],_ez=E(_eu);return _ez[0]==0?I_compareInt(_ey,_ez[1])>0:I_compare(_ey,_ez[1])>0;}},_eA=[0,1000],_eB=function(_eC,_eD){while(1){var _eE=E(_eC);if(!_eE[0]){var _eF=_eE[1],_eG=E(_eD);if(!_eG[0]){var _eH=_eG[1],_eI=subC(_eF,_eH);if(!E(_eI[2])){return [0,_eI[1]];}else{_eC=[1,I_fromInt(_eF)];_eD=[1,I_fromInt(_eH)];continue;}}else{_eC=[1,I_fromInt(_eF)];_eD=_eG;continue;}}else{var _eJ=E(_eD);if(!_eJ[0]){_eC=_eE;_eD=[1,I_fromInt(_eJ[1])];continue;}else{return [1,I_sub(_eE[1],_eJ[1])];}}}},_eK=function(_eL,_eM){var _eN=_eL%_eM;if(_eL<=0){if(_eL>=0){return E(_eN);}else{if(_eM<=0){return E(_eN);}else{var _eO=E(_eN);return _eO==0?0:_eO+_eM|0;}}}else{if(_eM>=0){if(_eL>=0){return E(_eN);}else{if(_eM<=0){return E(_eN);}else{var _eP=E(_eN);return _eP==0?0:_eP+_eM|0;}}}else{var _eQ=E(_eN);return _eQ==0?0:_eQ+_eM|0;}}},_eR=function(_eS,_eT){while(1){var _eU=E(_eS);if(!_eU[0]){var _eV=E(_eU[1]);if(_eV==(-2147483648)){_eS=[1,I_fromInt(-2147483648)];continue;}else{var _eW=E(_eT);if(!_eW[0]){return [0,B(_eK(_eV,_eW[1]))];}else{_eS=[1,I_fromInt(_eV)];_eT=_eW;continue;}}}else{var _eX=_eU[1],_eY=E(_eT);return _eY[0]==0?[0,I_toInt(I_mod(_eX,I_fromInt(_eY[1])))]:[1,I_mod(_eX,_eY[1])];}}},_eZ=function(_f0,_f1){while(1){var _f2=E(_f0);if(!_f2[0]){var _f3=_f2[1],_f4=E(_f1);if(!_f4[0]){var _f5=_f4[1],_f6=addC(_f3,_f5);if(!E(_f6[2])){return [0,_f6[1]];}else{_f0=[1,I_fromInt(_f3)];_f1=[1,I_fromInt(_f5)];continue;}}else{_f0=[1,I_fromInt(_f3)];_f1=_f4;continue;}}else{var _f7=E(_f1);if(!_f7[0]){_f0=_f2;_f1=[1,I_fromInt(_f7[1])];continue;}else{return [1,I_add(_f2[1],_f7[1])];}}}},_f8=function(_f9){return [0,_f9];},_fa=function(_fb,_fc){while(1){var _fd=E(_fb);if(!_fd[0]){var _fe=_fd[1],_ff=E(_fc);if(!_ff[0]){var _fg=_ff[1];if(!(imul(_fe,_fg)|0)){return [0,imul(_fe,_fg)|0];}else{_fb=[1,I_fromInt(_fe)];_fc=[1,I_fromInt(_fg)];continue;}}else{_fb=[1,I_fromInt(_fe)];_fc=_ff;continue;}}else{var _fh=E(_fc);if(!_fh[0]){_fb=_fd;_fc=[1,I_fromInt(_fh[1])];continue;}else{return [1,I_mul(_fd[1],_fh[1])];}}}},_fi=function(_fj,_fk,_fl,_fm){while(1){var _fn=(function(_fo,_fp,_fq,_fr){if(!B(_es(_fp,_fq))){var _fs=B(_eZ(B(_eB(_fq,_fp)),_cL)),_ft=B((function(_fu,_fv,_fw){while(1){if(!B(_ej(_fu,B(_fa(_fs,_eA))))){var _fx=E(_fw),_fy=B(_cP(_fx[1],_fx[2])),_fz=B(_fa(_fu,_d8)),_fA=B(_eZ(B(_fa(_fv,_d8)),B(_eB(B(_f8(E(_fy[1])[1])),_cL))));_fw=_fy[2];_fu=_fz;_fv=_fA;continue;}else{return [0,_fv,_fw];}}})(_cL,_er,_fr));return [0,new T(function(){return B(A(_eh,[_fo,new T(function(){if(!B(_e9(_fs,_er))){var _fB=B(_eZ(_fp,B(_eR(_ft[1],_fs))));}else{var _fB=E(_e8);}return _fB;})]));}),_ft[2]];}else{var _fC=_fo,_fD=_fq,_fE=_fp,_fF=_fr;_fj=_fC;_fk=_fD;_fl=_fE;_fm=_fF;return null;}})(_fj,_fk,_fl,_fm);if(_fn!=null){return _fn;}}},_fG=[0,0],_fH=function(_fI){var _fJ=E(_fI);if(_fJ==(-2147483648)){return E(_cm);}else{var _fK=_fJ-1|0;if(0<=_fK){var _fL=function(_fM){return [1,[0,[0,_fM]],new T(function(){if(_fM!=_fK){var _fN=B(_fL(_fM+1|0));}else{var _fN=[0];}var _fO=_fN;return _fO;})];};return new F(function(){return _fL(0);});}else{return [0];}}},_fP=function(_fQ,_fR,_fS,_fT){return new F(function(){return A(_fR,[function(_fU){if(!E(_fQ)){return [1,_fU,new T(function(){var _fV=E(_fT);return _fV[0]==0?E(_b9):E(_fV[2]);})];}else{var _fW=E(_fT);if(!_fW[0]){return E(_b7);}else{return new F(function(){return _J(B(_b2(_fW[1],_fW[2])),[1,_fU,_10]);});}}},new T(function(){return B(A(_fS,[new T(function(){if(!E(_fQ)){var _fX=E(_fT),_fY=_fX[0]==0?E(_6h):E(_fX[1]);}else{var _fZ=E(_fT),_fY=_fZ[0]==0?E(_6M):B(_6H(_fZ[1],_fZ[2]));}return _fY;})]));})]);});},_g0=[2],_g1=function(_g2){return [1,_g2];},_g3=function(_g2){return [0,_g2];},_g4=function(_g5,_g6){while(1){var _g7=E(_g6);if(!_g7[0]){return true;}else{if(!B(A(_g5,[_g7[1]]))){return false;}else{_g6=_g7[2];continue;}}}},_g8=function(_g9){while(1){var _ga=(function(_gb){var _gc=E(_gb);if(!_gc[0]){return [0];}else{var _gd=_gc[2],_ge=E(_gc[1]);if(!_ge[0]){_g9=_gd;return null;}else{return [1,_ge[1],new T(function(){return B(_g8(_gd));})];}}})(_g9);if(_ga!=null){return _ga;}}},_gf=function(_gg,_gh){if(_gg<=_gh){var _gi=function(_gj){return [1,[0,_gj],new T(function(){if(_gj!=_gh){var _gk=B(_gi(_gj+1|0));}else{var _gk=[0];}var _gl=_gk;return _gl;})];};return new F(function(){return _gi(_gg);});}else{return [0];}},_gm=function(_gn,_go){var _gp=E(_gn);if(!_gp[0]){return E(_6h);}else{var _gq=B(_E(_gp[1],0));if(_gq==(-2147483648)){return E(_cm);}else{var _gr=_gq-1|0;if(0<=_gr){var _gs=function(_gt){return [1,new T(function(){var _gu=[0,_gt],_gv=function(_gw){var _gx=E(_gw);if(!_gx[0]){return [0];}else{var _gy=_gx[1];return [1,new T(function(){return B(A(_go,[_gy,_gu,new T(function(){return B(_2H(_gy,_9,function(_gz){return new F(function(){return _2H(_gu,_9,_2W,_gz);});},_gp));})]));}),new T(function(){return B(_gv(_gx[2]));})];}};return B(_gv(new T(function(){var _gA=B(_E(_gp,0));if(_gA==(-2147483648)){var _gB=E(_cm);}else{var _gB=B(_gf(0,_gA-1|0));}return _gB;})));}),new T(function(){if(_gt!=_gr){var _gC=B(_gs(_gt+1|0));}else{var _gC=[0];}var _gD=_gC;return _gD;})];};return new F(function(){return _gs(0);});}else{return [0];}}}},_gE=function(_gF){return E(_gF)[0]==0?false:true;},_gG=function(_gH){var _gI=E(_gH);return !E(_gI[2])?[0]:[1,_gI[1]];},_gJ=new T(function(){return B(_gf(0,2147483647));}),_gK=function(_gL){var _gM=E(_gL);return E(_gM[2])[0]==0?[1,_gM[1]]:[0];},_gN=function(_gO,_gP){while(1){var _gQ=(function(_gR,_gS){var _gT=E(_gS);if(!_gT[0]){return [0];}else{var _gU=_gT[2],_gV=B(A(_gR,[_gT[1]]));if(!_gV[0]){var _gW=_gR;_gP=_gU;_gO=_gW;return null;}else{return [1,_gV[1],new T(function(){return B(_gN(_gR,_gU));})];}}})(_gO,_gP);if(_gQ!=null){return _gQ;}}},_gX=function(_gY,_gZ){var _h0=E(_gY);if(!_h0[0]){return [0];}else{var _h1=E(_gZ);return _h1[0]==0?[0]:[1,[0,_h0[1],_h1[1]],new T(function(){return B(_gX(_h0[2],_h1[2]));})];}},_h2=function(_h3){return [0,_g0,new T(function(){var _h4=E(_h3),_h5=_h4[1],_h6=_h4[2],_h7=_h4[4],_h8=E(_h4[3]);switch(_h8[0]){case 1:var _h9=function(_ha){var _hb=E(_ha);return _hb[0]==0?E(new T(function(){if(!B(_g4(_gE,B(_fP(_h6,_9,_2W,_h7))))){var _hc=B(_1C(_g3,B(_gN(_gG,B(_gX(_gJ,new T(function(){if(!E(_h6)){var _hd=E(E(_h5)[2])[1],_he=B(_1C(function(_hf){return new F(function(){return _4N(E(_hf)[1],_hd);});},_hd));}else{var _hg=E(E(_h5)[1])[1],_he=B(_1C(function(_hh){return new F(function(){return _4N(E(_hh)[1],_hg);});},_hg));}return _he;},1)))))));}else{var _hc=[0];}var _hi=_hc;return _hi;})):[1,[1,_hb[1]],new T(function(){return B(_h9(_hb[2]));})];},_hj=B(_h9(B(_g8(B(_6i(B(_gm(_h7,function(_hk,_hl,_hm){var _hn=E(_hm);if(!_hn[0]){return [0];}else{var _ho=function(_hp){return B(_4q(_h6,_h7,_hk,_hl))[0]==0?[0]:[1,[0,_hk,_hl]];};return !E(E(_hn[1])[1])?!E(_h6)?B(_ho(_)):[0]:!E(_h6)?[0]:B(_ho(_));}}))))))));break;case 2:var _hq=E(_h8[1]),_hj=B(_1C(_g1,B(_4q(_h6,_h7,_hq[1],_hq[2]))));break;case 4:if(!E(_h6)){var _hr=B(_fH(B(_E(E(E(_h5)[2])[1],0))));}else{var _hr=B(_fH(B(_E(E(E(_h5)[1])[1],0))));}var _hj=_hr;break;case 5:var _hj=B(_1C(function(_hs){return [1,[0,new T(function(){if(!E(_h6)){var _ht=E(_fG);}else{var _hu=B(_E(_h7,0));if(_hu==(-2147483648)){var _hv=E(_cm);}else{var _hv=[0,_hu-1|0];}var _ht=_hv;}return _ht;}),_hs]];},B(_gN(_gK,B(_gX(_gJ,new T(function(){return B(_fP(_h6,_9,_2W,_h7));},1)))))));break;default:var _hj=[0];}var _hw=_hj;return _hw;})];},_hx=function(_hy){var _hz=B(_h2(_hy));return [1,_hz[1],_hz[2]];},_hA=[0,0],_hB=function(_hC){var _hD=new T(function(){var _hE=B(_E(B(_hx(_hC)),0));if(_hE==(-2147483648)){var _hF=E(_cm);}else{var _hG=B(_fi(_cK,_hA,B(_f8(_hE-1|0)),new T(function(){return E(E(_hC)[5]);}))),_hF=[0,_hG[1],_hG[2]];}var _hH=_hF;return _hH;});return [0,new T(function(){return B(_2H(new T(function(){return E(E(_hD)[1]);}),_9,_2W,new T(function(){return B(_hx(_hC));})));}),new T(function(){return E(E(_hD)[2]);})];},_hI=function(_hJ,_hK,_hL,_hM,_hN,_hO){var _hP=B(_2H(_hJ,_9,_2W,new T(function(){if(!E(_hM)){var _hQ=E(E(_hL)[1]);}else{var _hQ=E(E(_hK)[1]);}return _hQ;})))[1];if(!E(_hM)){var _hR=E(_hL);return !B(_4N(_hP,_hR[1]))?[0]:[1,[0,[0,_hK,_hR],_4T,[5,_hJ],_hN,_hO]];}else{var _hS=E(_hK);return !B(_4N(_hP,_hS[1]))?[0]:[1,[0,[0,_hS,_hL],_4U,[5,_hJ],_hN,_hO]];}},_hT=function(_hU){return [0,E(E(_hU))];},_hV=function(_hW,_hX){return E(_hW);},_hY=[0,_ac,_hV],_hZ=[0,2147483647],_i0=[0,-2147483648],_i1=[0,2147483562],_i2=[0,1],_i3=[0,_i2,_i1],_i4=function(_i5){return E(_i3);},_i6=function(_i7){var _i8=E(_i7),_i9=B(_cP(_i8[1],_i8[2]));return [0,_i9[1],_i9[2]];},_ia=function(_ib,_ic){var _id=new T(function(){return E(B(_cP(_ib,_ic))[2]);});return [0,new T(function(){var _ie=E(_ib);if(_ie==2147483562){var _if=[0,1,E(_id)[2]];}else{var _if=[0,_ie+1|0,E(_id)[2]];}return _if;}),new T(function(){var _ig=E(_id)[1],_ih=E(_ic);if(_ih==1){var _ii=[0,_ig,2147483398];}else{var _ii=[0,_ig,_ih-1|0];}var _ij=_ii;return _ij;})];},_ik=function(_il){var _im=E(_il),_in=B(_ia(_im[1],_im[2]));return [0,_in[1],_in[2]];},_io=[0,_i6,_i4,_ik],_ip=function(_iq){return E(E(_iq)[2]);},_ir=function(_is){return E(E(_is)[1]);},_it=function(_iu,_iv,_iw,_ix,_iy){while(1){var _iz=(function(_iA,_iB,_iC,_iD,_iE){if(!B(_es(_iC,_iD))){var _iF=B(_eZ(B(_eB(_iD,_iC)),_cL)),_iG=new T(function(){return B(A(_ip,[_iA,_iE]));}),_iH=new T(function(){return E(E(_iG)[1]);}),_iI=new T(function(){return B(_eZ(B(_eB(B(_f8(E(E(_iG)[2])[1])),B(_f8(E(_iH)[1])))),_cL));}),_iJ=B((function(_iK,_iL,_iM){while(1){if(!B(_ej(_iK,B(_fa(_iF,_eA))))){var _iN=B(A(new T(function(){return B(_ir(_iA));}),[_iM])),_iO=B(_fa(_iK,_iI)),_iP=B(_eZ(B(_fa(_iL,_iI)),B(_eB(B(_f8(E(_iN[1])[1])),new T(function(){return B(_f8(E(_iH)[1]));})))));_iM=_iN[2];_iK=_iO;_iL=_iP;continue;}else{return [0,_iL,_iM];}}})(_cL,_er,_iE));return [0,new T(function(){return B(A(_eh,[_iB,new T(function(){if(!B(_e9(_iF,_er))){var _iQ=B(_eZ(_iC,B(_eR(_iJ[1],_iF))));}else{var _iQ=E(_e8);}return _iQ;})]));}),_iJ[2]];}else{var _iR=_iA,_iS=_iB,_iT=_iD,_iU=_iC,_iV=_iE;_iu=_iR;_iv=_iS;_iw=_iT;_ix=_iU;_iy=_iV;return null;}})(_iu,_iv,_iw,_ix,_iy);if(_iz!=null){return _iz;}}},_iW=[0,0],_iX=function(_iY,_iZ,_j0){var _j1=E(_iZ);if(!_j1){return [0];}else{var _j2=new T(function(){var _j3=B(_it(_iY,_cK,_iW,B(_f8(_j1)),_j0));return [0,_j3[1],_j3[2]];});return [1,[0,new T(function(){return E(E(_j2)[1]);}),_j0],new T(function(){return B(_iX(_iY,_j1-1|0,new T(function(){return E(E(_j2)[2]);})));})];}},_j4=function(_j5){var _j6=E(_j5);if(!_j6[0]){return [0,_10,_10];}else{var _j7=E(_j6[1]),_j8=new T(function(){var _j9=B(_j4(_j6[2]));return [0,_j9[1],_j9[2]];});return [0,[1,_j7[1],new T(function(){return E(E(_j8)[1]);})],[1,_j7[2],new T(function(){return E(E(_j8)[2]);})]];}},_ja=new T(function(){return B(unCStr("[extractTree] impossible"));}),_jb=new T(function(){return B(err(_ja));}),_jc=function(_jd,_je){var _jf=function(_jg){var _jh=E(_je);if(!_jh[0]){return E(_jb);}else{var _ji=_jh[1],_jj=_jh[3],_jk=E(_jh[2]);if(!_jk[0]){var _jl=new T(function(){var _jm=B(_jc(_jd-1|0,_jj));return [0,_jm[1],_jm[2]];});return [0,new T(function(){return E(E(_jl)[1]);}),new T(function(){return [1,_ji-1|0,E(_jk),E(E(E(_jl)[2]))];})];}else{var _jn=_jk[1],_jo=function(_jp){if(_jd>=_jn){var _jq=new T(function(){var _jr=B(_jc(_jd-_jn|0,_jj));return [0,_jr[1],_jr[2]];});return [0,new T(function(){return E(E(_jq)[1]);}),new T(function(){return [1,_ji-1|0,E(_jk),E(E(E(_jq)[2]))];})];}else{var _js=new T(function(){var _jt=B(_jc(_jd,_jk));return [0,_jt[1],_jt[2]];});return [0,new T(function(){return E(E(_js)[1]);}),new T(function(){return [1,_ji-1|0,E(E(E(_js)[2])),E(_jj)];})];}},_ju=E(_jj);if(!_ju[0]){return (_jd+1|0)!=_ji?B(_jo(_)):[0,_ju[1],_jk];}else{return new F(function(){return _jo(_);});}}}};switch(E(_jd)){case 0:var _jv=E(_je);if(!_jv[0]){return new F(function(){return _jf(_);});}else{var _jw=E(_jv[2]);return _jw[0]==0?[0,_jw[1],_jv[3]]:B(_jf(_));}break;case 1:var _jx=E(_je);if(!_jx[0]){return new F(function(){return _jf(_);});}else{if(E(_jx[1])==2){var _jy=E(_jx[2]);if(!_jy[0]){var _jz=E(_jx[3]);return _jz[0]==0?[0,_jz[1],_jy]:B(_jf(_));}else{return new F(function(){return _jf(_);});}}else{return new F(function(){return _jf(_);});}}break;default:return new F(function(){return _jf(_);});}},_jA=new T(function(){return B(unCStr("[shuffle] called with lists of different lengths"));}),_jB=new T(function(){return B(err(_jA));}),_jC=function(_jD,_jE){var _jF=function(_jG){var _jH=E(_jE);if(!_jH[0]){return E(_jB);}else{var _jI=new T(function(){var _jJ=B(_jc(E(_jH[1])[1],_jD));return [0,_jJ[1],_jJ[2]];});return [1,new T(function(){return E(E(_jI)[1]);}),new T(function(){return B(_jC(E(_jI)[2],_jH[2]));})];}},_jK=E(_jD);return _jK[0]==0?E(_jE)[0]==0?[1,_jK[1],_10]:B(_jF(_)):B(_jF(_));},_jL=function(_jM){var _jN=E(_jM);if(!_jN[0]){return [0];}else{var _jO=_jN[1],_jP=E(_jN[2]);if(!_jP[0]){return [1,_jO,_10];}else{var _jQ=E(_jP[1]);return [1,new T(function(){var _jR=E(E(_jO));if(!_jR[0]){var _jS=E(_jQ);if(!_jS[0]){var _jT=[1,2,E(_jR),E(_jS)];}else{var _jT=[1,_jS[1]+1|0,E(_jR),E(_jS)];}var _jU=_jT;}else{var _jV=_jR[1],_jW=E(_jQ);if(!_jW[0]){var _jX=[1,_jV+1|0,E(_jR),E(_jW)];}else{var _jX=[1,_jV+_jW[1]|0,E(_jR),E(_jW)];}var _jU=_jX;}return _jU;}),new T(function(){return B(_jL(_jP[2]));})];}}},_jY=new T(function(){return B(_jL(_10));}),_jZ=new T(function(){return B(_k0(_jY));}),_k0=function(_k1){while(1){var _k2=E(_k1);if(!_k2[0]){return E(_jZ);}else{if(!E(_k2[2])[0]){return E(_k2[1]);}else{_k1=B(_jL(_k2));continue;}}}},_k3=function(_k4,_k5,_k6){var _k7=B(A(_k4,[_hY,function(_k8){var _k9=E(_k8),_ka=_k9[2];return [0,_k9[1],[1,_k5,new T(function(){return B(_jC(B(_k0(B(_1C(_hT,_ka)))),B(_j4(B(_iX(_io,B(_E(_ka,0))-1|0,new T(function(){return E(E(_k6)[5]);})))))[1]));})],_k9[3],_k9[4],_k9[5]];},_k6]));return [0,_k7[1],_k7[2],_k7[3],_k7[4],new T(function(){return E(B(_fi(_cK,_i0,_hZ,_k7[5]))[2]);})];},_kb=function(_kc,_kd,_ke){return new F(function(){return A(_kc,[function(_kf){var _kg=E(_ke),_kh=_kg[1],_ki=_kg[2];return [0,new T(function(){if(!E(_ki)){var _kj=[0,E(_kh)[1],_kf];}else{var _kj=[0,_kf,E(_kh)[2]];}return _kj;}),_ki,_kg[3],_kg[4],_kg[5]];},new T(function(){return B(A(_kd,[new T(function(){var _kk=E(_ke),_kl=_kk[1];if(!E(_kk[2])){var _km=E(E(_kl)[2]);}else{var _km=E(E(_kl)[1]);}var _kn=_km;return _kn;})]));})]);});},_ko=function(_kp,_kq,_kr){return new F(function(){return _kb(E(_kp)[1],_kq,_kr);});},_ks=function(_kt,_ku,_kv){var _kw=function(_kx){if(_kt<=_kx){var _ky=B(_k3(_ko,new T(function(){return B(_2H(_ku,_9,_2W,new T(function(){var _kz=E(_kv),_kA=_kz[1];if(!E(_kz[2])){var _kB=E(E(E(_kA)[2])[1]);}else{var _kB=E(E(E(_kA)[1])[1]);}var _kC=_kB;return _kC;})));}),_kv)),_kD=_ky[1],_kE=_ky[2];return [0,new T(function(){if(!E(_kE)){var _kF=E(_kD),_kG=[0,_kF[1],new T(function(){var _kH=E(_kF[2]);return [0,new T(function(){return B(_aZ(_ku,_kH[1]));}),_kH[2],_kH[3],_kH[4],_kH[5]];})];}else{var _kI=E(_kD),_kG=[0,new T(function(){var _kJ=E(_kI[1]);return [0,new T(function(){return B(_aZ(_ku,_kJ[1]));}),_kJ[2],_kJ[3],_kJ[4],_kJ[5]];}),_kI[2]];}return _kG;}),_kE,_af,_ky[4],_ky[5]];}else{var _kK=B(_k3(_ko,new T(function(){return B(_2H(_ku,_9,_2W,new T(function(){var _kL=E(_kv),_kM=_kL[1];if(!E(_kL[2])){var _kN=E(E(E(_kM)[2])[1]);}else{var _kN=E(E(E(_kM)[1])[1]);}var _kO=_kN;return _kO;})));}),new T(function(){var _kP=E(_kv),_kQ=_kP[1],_kR=_kP[2];return [0,_kQ,_kR,[4,new T(function(){if(E(B(_2H(_ku,_9,_2W,new T(function(){if(!E(_kR)){var _kS=E(E(E(_kQ)[2])[1]);}else{var _kS=E(E(E(_kQ)[1])[1]);}return _kS;})))[1])==1){var _kT=[0,_kt-2|0];}else{var _kT=[0,_kt-1|0];}var _kU=_kT;return _kU;})],_kP[4],_kP[5]];}))),_kV=_kK[1],_kW=_kK[2];return [0,new T(function(){if(!E(_kW)){var _kX=E(_kV),_kY=[0,_kX[1],new T(function(){var _kZ=E(_kX[2]);return [0,new T(function(){return B(_aZ(_ku,_kZ[1]));}),_kZ[2],_kZ[3],_kZ[4],_kZ[5]];})];}else{var _l0=E(_kV),_kY=[0,new T(function(){var _l1=E(_l0[1]);return [0,new T(function(){return B(_aZ(_ku,_l1[1]));}),_l1[2],_l1[3],_l1[4],_l1[5]];}),_l0[2]];}return _kY;}),_kW,_kK[3],_kK[4],_kK[5]];}};return E(B(_2H(_ku,_9,_2W,new T(function(){var _l2=E(_kv),_l3=_l2[1];if(!E(_l2[2])){var _l4=E(E(E(_l3)[2])[1]);}else{var _l4=E(E(E(_l3)[1])[1]);}var _l5=_l4;return _l5;})))[1])==1?B(_kw(2)):B(_kw(1));},_l6=[3],_l7=function(_){return _4S;},_l8=function(_){var _l9=B(A(_16,["(function(){return document.body;})",_])),_la=_l9;return [0,_la];},_lb=[0],_lc=function(_ld,_le,_){var _lf=B(A(_ld,[_])),_lg=_lf,_lh=E(_le),_li=jsSetTimeout(1000,_lh);return _4S;},_lj=function(_lk,_ll,_lm,_ln,_lo){var _lp=function(_lq){var _lr=new T(function(){return !E(_lm)?[0,_lk,new T(function(){var _ls=E(_ll);return [0,[1,_lq,_ls[1]],_ls[2],_ls[3],_ls[4],_ls[5]];})]:[0,new T(function(){var _lt=E(_lk);return [0,[1,_lq,_lt[1]],_lt[2],_lt[3],_lt[4],_lt[5]];}),_ll];},1);return [0,new T(function(){if(!E(_lm)){var _lu=E(_lr),_lv=[0,_lu[1],new T(function(){var _lw=E(_lu[2]);return [0,_lw[1],new T(function(){var _lx=E(_lw[2]);return _lx[0]==0?E(_b9):E(_lx[2]);}),_lw[3],_lw[4],_lw[5]];})];}else{var _ly=E(_lr),_lv=[0,new T(function(){var _lz=E(_ly[1]);return [0,_lz[1],new T(function(){var _lA=E(_lz[2]);return _lA[0]==0?E(_b9):E(_lA[2]);}),_lz[3],_lz[4],_lz[5]];}),_ly[2]];}return _lv;}),_lm,_ab,_ln,_lo];};if(!E(_lm)){var _lB=E(_ll),_lC=E(_lB[2]);return _lC[0]==0?[0,[0,_lk,_lB],_4T,_ah,_ln,_lo]:B(_lp(_lC[1]));}else{var _lD=E(_lk),_lE=E(_lD[2]);return _lE[0]==0?[0,[0,_lD,_ll],_4U,_ag,_ln,_lo]:B(_lp(_lE[1]));}},_lF=function(_lG){var _lH=E(_lG),_lI=E(_lH[1]),_lJ=B(_lj(_lI[1],_lI[2],_lH[2],_lH[4],_lH[5]));return [0,_lJ[1],_lJ[2],_lJ[3],_lJ[4],_lJ[5]];},_lK=new T(function(){return B(unCStr("foldr1"));}),_lL=new T(function(){return B(_6e(_lK));}),_lM=function(_lN,_lO){var _lP=E(_lO);if(!_lP[0]){return E(_lL);}else{var _lQ=_lP[1],_lR=E(_lP[2]);if(!_lR[0]){return E(_lQ);}else{return new F(function(){return A(_lN,[_lQ,new T(function(){return B(_lM(_lN,_lR));})]);});}}},_lS=function(_lT,_lU){return new F(function(){return _lM(_lc,[1,_l7,[1,function(_){var _lV=E(_lT)[1],_lW=rMV(_lV),_lX=_lW,_=wMV(_lV,new T(function(){var _lY=E(_lX);return [0,_lY[1],new T(function(){return !E(_lY[2])?true:false;}),_lb,_lY[4],_lY[5]];})),_lZ=rMV(_lV),_m0=_lZ,_m1=B(_l8(_)),_m2=_m1,_m3=B(A(_7f,[_m0,_m2,_])),_m4=_m3;return _4S;},[1,function(_){var _m5=E(_lT)[1],_m6=rMV(_m5),_m7=_m6,_=wMV(_m5,new T(function(){return B(_lF(_m7));})),_m8=rMV(_m5),_m9=_m8,_ma=B(_l8(_)),_mb=_ma,_mc=B(A(_7f,[_m9,_mb,_])),_md=_mc;return _4S;},[1,_lU,_10]]]]);});},_me=function(_mf,_){var _mg=function(_){return new F(function(){return _me(_mf,_);});},_mh=rMV(_mf),_mi=_mh,_mj=E(_mi);switch(E(_mj[3])[0]){case 3:var _mk=rMV(_mf),_ml=_mk,_=wMV(_mf,new T(function(){var _mm=E(_ml);return [0,_mm[1],_mm[2],_af,_mm[4],_mm[5]];})),_mn=rMV(_mf),_mo=_mn,_mp=B(_l8(_)),_mq=_mp,_mr=B(A(_7f,[_mo,_mq,_])),_ms=_mr,_mt=jsSetTimeout(1000,_mg);return _4S;case 6:return new F(function(){return A(_lS,[[0,_mf],_l7,_]);});break;default:var _mu=rMV(_mf),_mv=_mu,_=wMV(_mf,new T(function(){var _mw=B(_hB(_mj)),_mx=_mw[2],_my=E(_mv),_mz=_my[1],_mA=_my[2],_mB=_my[3],_mC=_my[4],_mD=_my[5],_mE=E(_mw[1]);switch(_mE[0]){case 0:var _mF=_mE[1],_mG=E(_mB);switch(_mG[0]){case 1:var _mH=E(_mz),_mI=B(_hI(_mF,_mH[1],_mH[2],_mA,_mC,_mD));if(!_mI[0]){var _mJ=[0,_mH,_mA,_ab,_mC,_mx];}else{var _mK=E(_mI[1]),_mJ=[0,_mK[1],_mK[2],_mK[3],_mK[4],_mx];}var _mL=_mJ,_mM=_mL;break;case 4:var _mN=B(_ks(E(_mG[1])[1],_mF,_my)),_mM=[0,_mN[1],_mN[2],_mN[3],_mN[4],_mx];break;case 5:var _mO=E(_mF);if(_mO[1]!=E(_mG[1])[1]){var _mP=E(_mz),_mQ=B(_hI(_mO,_mP[1],_mP[2],_mA,_mC,_mD));if(!_mQ[0]){var _mR=[0,_mP,_mA,_mG,_mC,_mx];}else{var _mS=E(_mQ[1]),_mR=[0,_mS[1],_mS[2],_mS[3],_mS[4],_mx];}var _mT=_mR,_mU=_mT;}else{var _mU=[0,_mz,_mA,_ab,_mC,_mx];}var _mV=_mU,_mW=_mV,_mX=_mW,_mM=_mX;break;default:var _mM=[0,_mz,_mA,_mG,_mC,_mx];}var _mY=_mM;break;case 1:var _mZ=E(_mE[1]),_n0=B(_bR(_mZ[1],_mZ[2],_mz,_mA,_mB,_mC,_mD)),_mY=[0,_n0[1],_n0[2],_n0[3],_n0[4],_mx];break;default:var _mY=[0,_mz,_mA,_l6,_mC,_mx];}var _n1=_mY,_n2=_n1;return _n2;})),_n3=rMV(_mf),_n4=_n3,_n5=B(_l8(_)),_n6=_n5,_n7=B(A(_7f,[_n4,_n6,_])),_n8=_n7,_n9=jsSetTimeout(1000,_mg);return _4S;}},_na=function(_nb){return new F(function(){return _53(E(_nb)[1]);});},_nc=function(_nd){return [0,[0,_nd],new T(function(){var _ne=E(_nd);if(_ne==13){var _nf=[0];}else{var _ng=B(_nc(_ne+1|0)),_nf=[1,_ng[1],_ng[2]];}return _nf;})];},_nh=new T(function(){var _ni=B(_nc(1));return [1,_ni[1],_ni[2]];}),_nj=function(_nk){return _nk>1?[0,_nh,new T(function(){var _nl=B(_nj(_nk-1|0));return [1,_nl[1],_nl[2]];})]:[0,_nh,_10];},_nm=new T(function(){var _nn=B(_nj(2));return B(_6i([1,_nn[1],_nn[2]]));}),_no=new T(function(){return [0,B(_E(_nm,0))];}),_np=function(_nq,_nr,_ns){return new F(function(){return _iX(_nq,E(_nr)[1],_ns);});},_nt=function(_nu,_nv,_nw){return function(_nx){return new F(function(){return _jC(new T(function(){return B(_k0(B(_1C(_hT,_nv))));}),B(_j4(B(_np(_nu,new T(function(){return [0,E(_nw)[1]-1|0];}),_nx))))[1]);});};},_ny=new T(function(){return B(_nt(_io,_nm,_no));}),_nz=function(_nA){return _nA>1?[0,_27,new T(function(){var _nB=B(_nz(_nA-1|0));return [1,_nB[1],_nB[2]];})]:[0,_27,_10];},_nC=new T(function(){var _nD=B(_nz(3));return [1,_nD[1],_nD[2]];}),_nE=function(_nF){return _nF>1?[0,_nC,new T(function(){var _nG=B(_nE(_nF-1|0));return [1,_nG[1],_nG[2]];})]:[0,_nC,_10];},_nH=new T(function(){var _nI=B(_nE(5));return [1,_nI[1],_nI[2]];}),_nJ=[0,63],_nK=[1,_nJ,_10],_nL=function(_nM){return E(_nK);},_nN=new T(function(){return B(unCStr("computers"));}),_nO=new T(function(){return B(unCStr("\u30b3\u30f3\u30d4\u30e5\u30fc\u30bf"));}),_nP=new T(function(){return B(unCStr("yours"));}),_nQ=new T(function(){return B(unCStr("\u3042\u306a\u305f"));}),_nR=function(_nS,_nT){var _nU=E(_nS);if(!_nU){return [0];}else{var _nV=E(_nT);return _nV[0]==0?[0]:[1,_nV[1],new T(function(){return B(_nR(_nU-1|0,_nV[2]));})];}},_nW=function(_nX,_nY,_nZ){var _o0=new T(function(){return B(A(_ny,[_nY]));}),_o1=new T(function(){return B(A(_ny,[_nX]));});return [0,[0,[0,new T(function(){return B(_nR(3,_o1));}),new T(function(){return B(_2C(3,_o1));}),_nQ,_nP,_na],[0,new T(function(){return B(_nR(3,_o0));}),new T(function(){return B(_2C(3,_o0));}),_nO,_nN,_nL]],_4U,_lb,_nH,_nZ];},_o2=[0,0],_o3=0,_o4=new T(function(){return B(_e5(_o3));}),_o5=new T(function(){return die(_o4);}),_o6=function(_o7,_o8){var _o9=E(_o8);if(!_o9){return E(_e8);}else{var _oa=function(_ob){if(_o7<=0){if(_o7>=0){var _oc=quotRemI(_o7,_o9);return [0,[0,_oc[1]],[0,_oc[2]]];}else{if(_o9<=0){var _od=quotRemI(_o7,_o9);return [0,[0,_od[1]],[0,_od[2]]];}else{var _oe=quotRemI(_o7+1|0,_o9);return [0,[0,_oe[1]-1|0],[0,(_oe[2]+_o9|0)-1|0]];}}}else{if(_o9>=0){if(_o7>=0){var _of=quotRemI(_o7,_o9);return [0,[0,_of[1]],[0,_of[2]]];}else{if(_o9<=0){var _og=quotRemI(_o7,_o9);return [0,[0,_og[1]],[0,_og[2]]];}else{var _oh=quotRemI(_o7+1|0,_o9);return [0,[0,_oh[1]-1|0],[0,(_oh[2]+_o9|0)-1|0]];}}}else{var _oi=quotRemI(_o7-1|0,_o9);return [0,[0,_oi[1]-1|0],[0,(_oi[2]+_o9|0)+1|0]];}}};return E(_o9)==(-1)?E(_o7)==(-2147483648)?[0,_o5,_o2]:B(_oa(_)):B(_oa(_));}},_oj=function(_ok){var _ol=B(_o6((_ok>>>0&2147483647>>>0)>>>0&4.294967295e9,2147483562));return [0,E(_ol[2])[1]+1|0,B(_eK(E(_ol[1])[1],2147483398))+1|0];},_om=function(_){var _on=B(A(_16,["(function(){ return Math.floor(Math.random() * Math.pow(2, 53)); })",_])),_oo=_on;return new T(function(){var _op=jsTrunc(_oo),_oq=_op,_or=B(_oj(_oq));return [0,_or[1],_or[2]];});},_os=new T(function(){return [0,"click"];}),_ot=[8,_],_ou=function(_ov){var _ow=String(_ov),_ox=_ow;return new F(function(){return fromJSStr(_ox);});},_oy=function(_oz,_oA){while(1){var _oB=E(_oz);if(!_oB[0]){return E(_oA)[0]==0?true:false;}else{var _oC=E(_oA);if(!_oC[0]){return false;}else{if(E(_oB[1])[1]!=E(_oC[1])[1]){return false;}else{_oz=_oB[2];_oA=_oC[2];continue;}}}}},_oD=new T(function(){return B(unCStr("LI"));}),_oE=new T(function(){return B(_16("(function(e){ return e.tagName })"));}),_oF=new T(function(){return B(unCStr("wheel"));}),_oG=new T(function(){return B(unCStr("mouseout"));}),_oH=new T(function(){return B(unCStr("mouseover"));}),_oI=new T(function(){return B(unCStr("mousemove"));}),_oJ=new T(function(){return B(unCStr("blur"));}),_oK=new T(function(){return B(unCStr("focus"));}),_oL=new T(function(){return B(unCStr("change"));}),_oM=new T(function(){return B(unCStr("unload"));}),_oN=new T(function(){return B(unCStr("load"));}),_oO=new T(function(){return B(unCStr("submit"));}),_oP=new T(function(){return B(unCStr("keydown"));}),_oQ=new T(function(){return B(unCStr("keyup"));}),_oR=new T(function(){return B(unCStr("keypress"));}),_oS=new T(function(){return B(unCStr("mouseup"));}),_oT=new T(function(){return B(unCStr("mousedown"));}),_oU=new T(function(){return B(unCStr("dblclick"));}),_oV=new T(function(){return B(unCStr("click"));}),_oW=function(_oX){switch(E(_oX)[0]){case 0:return E(_oN);case 1:return E(_oM);case 2:return E(_oL);case 3:return E(_oK);case 4:return E(_oJ);case 5:return E(_oI);case 6:return E(_oH);case 7:return E(_oG);case 8:return E(_oV);case 9:return E(_oU);case 10:return E(_oT);case 11:return E(_oS);case 12:return E(_oR);case 13:return E(_oQ);case 14:return E(_oP);case 15:return E(_oO);default:return E(_oF);}},_oY=new T(function(){return E(0);}),_oZ=new T(function(){return B(_16("(function(f) {  return (function() {      return (function(){        var args=Array.prototype.slice.call(arguments,0);        args.push(0);        return E(B(A(f, args)));    });  });})"));}),_p0=function(_p1,_){return new F(function(){return A(_oZ,[E(_p1),_]);});},_p2=function(_p3,_){return new F(function(){return _p0(_p3,_);});},_p4=new T(function(){return B(_16("(function(node, evtName, f){ node.addEventListener(evtName, (function(e){ f(e.target) }))})"));}),_p5=function(_p6,_p7){return function(_p8,_){var _p9=E(_p8),_pa=B(A(_p4,[E(_p9[1]),E(toJSStr(E(new T(function(){return B(_oW(_p6));})))),E(new T(function(){return B(_12(function(_){var _=0;return new F(function(){return _p2(function(_pb){return new F(function(){return _12(function(_){var _=0,_pc=B(A(_p7,[[0,_pb],_])),_pd=_pc;return E(_oY);});});},_);});}));})),_])),_pe=_pa;return _p9;};},_pf=function(_pg){return new F(function(){return _p5(_ot,function(_ph,_){var _pi=E(_ph)[1],_pj=B(A(_oE,[E(_pi),_])),_pk=_pj;if(!B(_oy(B(_ou(_pk)),_oD))){return _4S;}else{var _pl=B(_2t(_6E,_pi,_)),_pm=_pl;return new F(function(){return A(_pg,[_pm,_]);});}});});},_pn=new T(function(){return B(unCStr("TD"));}),_po=function(_pp){return new F(function(){return _p5(_ot,function(_pq,_){var _pr=E(_pq)[1],_ps=E(_pr),_pt=B(A(_oE,[_ps,_])),_pu=_pt;if(!B(_oy(B(_ou(_pu)),_pn))){return _4S;}else{var _pv=B(A(_6l,[_ps,_])),_pw=_pv,_px=B(_2t(_6E,_pw,_)),_py=_px,_pz=B(_2t(_6E,_pr,_)),_pA=_pz;return new F(function(){return A(_pp,[_py,_pA,_]);});}});});},_pB=new T(function(){return B(unCStr("#field"));}),_pC=new T(function(){return B(unCStr("#yours ol.hand"));}),_pD=new T(function(){return B(unCStr("button#pass"));}),_pE=function(_pF,_pG,_pH){var _pI=E(_pH);return new F(function(){return _bR(_pF,_pG,_pI[1],_pI[2],_pI[3],_pI[4],_pI[5]);});},_pJ=function(_pK,_pL,_pM,_pN,_pO,_pP){var _pQ=E(_pN);switch(_pQ[0]){case 1:var _pR=E(_pL),_pS=B(_hI(_pK,_pR[1],_pR[2],_pM,_pO,_pP));return _pS[0]==0?[0,_pR,_pM,_ab,_pO,_pP]:E(_pS[1]);case 4:var _pT=B(_ks(E(_pQ[1])[1],_pK,[0,_pL,_pM,_pQ,_pO,_pP]));return [0,_pT[1],_pT[2],_pT[3],_pT[4],_pT[5]];case 5:var _pU=E(_pK);if(_pU[1]!=E(_pQ[1])[1]){var _pV=E(_pL),_pW=B(_hI(_pU,_pV[1],_pV[2],_pM,_pO,_pP));return _pW[0]==0?[0,_pV,_pM,_pQ,_pO,_pP]:E(_pW[1]);}else{return [0,_pL,_pM,_ab,_pO,_pP];}break;default:return [0,_pL,_pM,_pQ,_pO,_pP];}},_pX=function(_pY,_pZ){var _q0=E(_pZ);return new F(function(){return _pJ(_pY,_q0[1],_q0[2],_q0[3],_q0[4],_q0[5]);});},_q1=function(_){var _q2=B(_om(_)),_q3=_q2,_q4=B(_om(_)),_q5=_q4,_q6=B(_om(_)),_q7=_q6,_q8=new T(function(){var _q9=B(_nW(_q3,_q5,_q7));return [0,_q9[1],_q9[2],_q9[3],_q9[4],_q9[5]];}),_qa=nMV(_q8),_qb=_qa,_qc=function(_){return new F(function(){return _me(_qb,_);});},_qd=B(_l8(_)),_qe=_qd,_qf=[0,_qb],_qg=B(_1c(_pD,function(_qh,_){var _qi=E(_qh),_qj=jsSetCB(_qi[1],E(_os)[1],function(_qk,_ql,_){var _qm=rMV(_qb),_qn=_qm;return !E(E(_qn)[2])?_4S:B(A(_lS,[_qf,_qc,_]));}),_qo=_qj;return _qi;},_qe,_)),_qp=_qg,_qq=B(_1c(_pC,new T(function(){return B(_pf(function(_qr,_){var _qs=rMV(_qb),_qt=_qs;if(!E(E(_qt)[2])){return _4S;}else{var _qu=rMV(_qb),_qv=_qu,_=wMV(_qb,new T(function(){return B(_pX(_qr,_qv));})),_qw=rMV(_qb),_qx=_qw,_qy=B(_l8(_)),_qz=_qy,_qA=B(A(_7f,[_qx,_qz,_])),_qB=_qA,_qC=rMV(_qb),_qD=_qC;if(E(E(_qD)[3])[0]==6){var _qE=B(A(_lS,[_qf,_qc,_])),_qF=_qE;return _4S;}else{return _4S;}}}));}),_qe,_)),_qG=_qq,_qH=B(_1c(_pB,new T(function(){return B(_po(function(_qI,_qJ,_){var _qK=rMV(_qb),_qL=_qK,_=wMV(_qb,new T(function(){return B(_pE(_qI,_qJ,_qL));})),_qM=rMV(_qb),_qN=_qM,_qO=B(_l8(_)),_qP=_qO,_qQ=B(A(_7f,[_qN,_qP,_])),_qR=_qQ,_qS=rMV(_qb),_qT=_qS;if(E(E(_qT)[3])[0]==6){var _qU=B(A(_lS,[_qf,_qc,_])),_qV=_qU;return _4S;}else{return _4S;}}));}),_qe,_)),_qW=_qH,_qX=B(A(_7f,[_q8,_qe,_])),_qY=_qX,_qZ=jsSetTimeout(1000,function(_){var _r0=rMV(_qb),_r1=_r0,_=wMV(_qb,new T(function(){return B(_lF(_r1));})),_r2=rMV(_qb),_r3=_r2,_r4=B(_l8(_)),_r5=_r4,_r6=B(A(_7f,[_r3,_r5,_])),_r7=_r6;return _4S;});return _4S;},_r8=function(_){return new F(function(){return _q1(_);});};
var hasteMain = function() {B(A(_r8, [0]));};window.onload = hasteMain;