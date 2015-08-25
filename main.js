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

var _0=new T(function(){return B(unCStr("Prelude.(!!): negative index\n"));}),_1=new T(function(){return B(err(_0));}),_2=new T(function(){return B(unCStr("Prelude.(!!): index too large\n"));}),_3=new T(function(){return B(err(_2));}),_4=function(_5,_6){while(1){var _7=E(_5);if(!_7[0]){return E(_3);}else{var _8=E(_6);if(!_8){return E(_7[1]);}else{_5=_7[2];_6=_8-1|0;continue;}}}},_9=function(_a,_b){return E(_b);},_c=function(_d,_e,_){var _f=jsCreateTextNode(toJSStr(E(_d))),_g=_f,_h=jsAppendChild(_g,E(_e)[1]);return [0,_g];},_i=function(_j,_k,_){var _l=E(_j);if(!_l[0]){return _k;}else{var _m=B(A(_l[1],[_k,_])),_n=_m,_o=B(_i(_l[2],_k,_)),_p=_o;return _k;}},_q=new T(function(){return B(unCStr("\u884c\u52d5\u3092\u9078\u629e"));}),_r=new T(function(){return B(unCStr("\u30c9\u30ed\u30fc"));}),_s=new T(function(){return B(unCStr("\u3042\u306a\u305f\u306e\u52dd\u3061\u3067\u3059!"));}),_t=new T(function(){return B(unCStr("\u30b3\u30f3\u30d4\u30e5\u30fc\u30bf\u304c\u52dd\u3061!"));}),_u=new T(function(){return B(unCStr("\u624b\u756a\u3092\u4ea4\u4ee3"));}),_v=new T(function(){return B(unCStr("\u53ec\u559a\u3059\u308b\u4f4d\u7f6e\u3092\u9078\u629e"));}),_w=new T(function(){return B(unCStr("\u751f\u8d04\u3092\u9078\u629e: \u30a8\u30cd\u30eb\u30ae\u30fc\u304c\u3042\u3068"));}),_x=new T(function(){return B(unCStr("\u5fc5\u8981"));}),_y=new T(function(){return B(unCStr("\u30d1\u30b9\u3057\u307e\u3059"));}),_z=new T(function(){return B(unCStr("\u79fb\u52d5\u3059\u308b\u4f4d\u7f6e\u3092\u9078\u629e"));}),_A=new T(function(){return B(unCStr(" .deck"));}),_B=new T(function(){return B(unCStr("\u306e\u6b8b\u308a\u5c71\u672d: "));}),_C=[0,35],_D=new T(function(){return B(unCStr(" .hand"));}),_E=function(_F,_G){while(1){var _H=E(_F);if(!_H[0]){return E(_G);}else{_F=_H[2];var _I=_G+1|0;_G=_I;continue;}}},_J=function(_K,_L){var _M=E(_K);return _M[0]==0?E(_L):[1,_M[1],new T(function(){return B(_J(_M[2],_L));})];},_N=function(_O,_P){var _Q=jsShowI(_O),_R=_Q;return new F(function(){return _J(fromJSStr(_R),_P);});},_S=[0,41],_T=[0,40],_U=function(_V,_W,_X){if(_W>=0){return new F(function(){return _N(_W,_X);});}else{return _V<=6?B(_N(_W,_X)):[1,_T,new T(function(){var _Y=jsShowI(_W),_Z=_Y;return B(_J(fromJSStr(_Z),[1,_S,_X]));})];}},_10=[0],_11=new T(function(){return [0,"arr2lst"];}),_12=function(_13){var _14=B(A(_13,[_])),_15=_14;return E(_15);},_16=function(_17){return new F(function(){return _12(function(_){var _=0;return new F(function(){return eval(_17);});});});},_18=function(_19,_1a){return new F(function(){return _12(function(_){var _=0;return new F(function(){return A(_16,[E(_11)[1],E(_19),E(_1a),_]);});});});},_1b=new T(function(){return B(_16("(function(sel){return document.querySelectorAll(sel);})"));}),_1c=function(_1d,_1e,_1f,_){var _1g=B(A(_1b,[E(toJSStr(E(_1d))),_])),_1h=_1g,_1i=function(_1j,_){var _1k=E(_1j);if(!_1k[0]){return _10;}else{var _1l=B(A(_1e,[[0,_1k[1]],_])),_1m=_1l,_1n=B(_1i(_1k[2],_)),_1o=_1n;return [1,_1m,_1o];}},_1p=B(_1i(B(_18(_1h,0)),_)),_1q=_1p;return _1f;},_1r=new T(function(){return B(unCStr("li"));}),_1s=function(_1t,_1u,_1v,_){var _1w=jsCreateElem(toJSStr(E(_1r))),_1x=_1w,_1y=jsAppendChild(_1x,E(_1v)[1]),_1z=[0,_1x],_1A=B(A(_1t,[_1u,_1z,_])),_1B=_1A;return _1z;},_1C=function(_1D,_1E){var _1F=E(_1E);return _1F[0]==0?[0]:[1,new T(function(){return B(A(_1D,[_1F[1]]));}),new T(function(){return B(_1C(_1D,_1F[2]));})];},_1G=function(_1H){return function(_1I,_){var _1J=B(_1c([1,_C,new T(function(){return B(_J(E(_1H)[4],_A));})],function(_1K,_){var _1L=E(_1K),_1M=jsClearChildren(_1L[1]),_1N=B(_c(new T(function(){var _1O=E(_1H);return B(_J(_1O[3],new T(function(){return B(_J(_B,new T(function(){return B(_U(0,B(_E(_1O[2],0)),_10));},1)));},1)));}),_1L,_)),_1P=_1N;return _1L;},_1I,_)),_1Q=_1J,_1R=B(_1c([1,_C,new T(function(){return B(_J(E(_1H)[4],_D));})],function(_1S,_){var _1T=E(_1S),_1U=jsClearChildren(_1T[1]),_1V=B(_i(new T(function(){var _1W=E(_1H);return B(_1C(function(_1X){return function(_1Y,_1Z){return new F(function(){return _1s(_c,new T(function(){return B(A(_1W[5],[_1X]));}),_1Y,_1Z);});};},_1W[1]));}),_1T,_)),_20=_1V;return _1T;},_1I,_)),_21=_1R;return _1I;};},_22=new T(function(){return B(unCStr("Prelude.Enum.succ{Int}: tried to take `succ\' of maxBound"));}),_23=new T(function(){return B(err(_22));}),_24=function(_25){var _26=E(E(_25)[1]);return _26==2147483647?E(_23):[0,_26+1|0];},_27=[0],_28=new T(function(){return [0,"(function(e){ return e.previousSibling })"];}),_29=new T(function(){return B(_16("(function(x) {return x === null})"));}),_2a=new T(function(){return B(_16("(function(node) {return node.nodeType === 1})"));}),_2b=function(_2c,_){var _2d=E(_28)[1],_2e=B(A(_16,[_2d,E(_2c),_])),_2f=_2e,_2g=E(_2f),_2h=B(A(_29,[_2g,_])),_2i=_2h;if(_2i<=0){var _2j=B(A(_2a,[_2g,_])),_2k=_2j;if(_2k<=0){return new F(function(){return (function(_2l,_){while(1){var _2m=B(A(_16,[_2d,E(_2l),_])),_2n=_2m,_2o=E(_2n),_2p=B(A(_29,[_2o,_])),_2q=_2p;if(_2q<=0){var _2r=B(A(_2a,[_2o,_])),_2s=_2r;if(_2s<=0){_2l=_2n;continue;}else{return [1,[0,_2o]];}}else{return _27;}}})(_2f,_);});}else{return [1,[0,_2g]];}}else{return _27;}},_2t=function(_2u,_2v,_){while(1){var _2w=(function(_2x,_2y,_){var _2z=B(_2b(_2y,_)),_2A=_2z,_2B=E(_2A);if(!_2B[0]){return _2x;}else{_2u=new T(function(){return B(_24(_2x));});_2v=E(_2B[1])[1];return null;}})(_2u,_2v,_);if(_2w!=null){return _2w;}}},_2C=function(_2D,_2E){while(1){var _2F=E(_2D);if(!_2F){return E(_2E);}else{var _2G=E(_2E);if(!_2G[0]){return [0];}else{_2D=_2F-1|0;_2E=_2G[2];continue;}}}},_2H=function(_2I,_2J,_2K,_2L){return new F(function(){return A(_2J,[function(_2M){var _2N=E(_2I)[1],_2O=[1,_2M,new T(function(){var _2P=E(_2I)[1]+1|0;return _2P>=0?B(_2C(_2P,_2L)):E(_2L);})];if(_2N>0){var _2Q=function(_2R,_2S){var _2T=E(_2R);if(!_2T[0]){return E(_2O);}else{var _2U=_2T[1];return _2S>1?[1,_2U,new T(function(){return B(_2Q(_2T[2],_2S-1|0));})]:[1,_2U,_2O];}};return new F(function(){return _2Q(_2L,_2N);});}else{return E(_2O);}},new T(function(){return B(A(_2K,[new T(function(){var _2V=E(_2I)[1];return _2V>=0?B(_4(_2L,_2V)):E(_1);})]));})]);});},_2W=[0,0],_2X=[0,-1],_2Y=[0,_2X,_2W],_2Z=[1,_2Y,_10],_30=function(_31,_32){return [0,E(_31)[1]+E(_32)[1]|0];},_33=function(_34,_35){var _36=E(_34),_37=E(_35);return [0,new T(function(){return [0, -E(_36[1])[1]+E(_37[1])[1]|0];}),new T(function(){return B(_30(_36[2],_37[2]));})];},_38=function(_39,_3a){var _3b=E(_39),_3c=E(_3a);return [0,new T(function(){return B(_30(_3b[1],_3c[1]));}),new T(function(){return B(_30(_3b[2],_3c[2]));})];},_3d=[0,1],_3e=[0,_2X,_3d],_3f=[1,_3e,_10],_3g=[1,_2Y,_3f],_3h=[0,_2X,_2X],_3i=[1,_3h,_3g],_3j=function(_3k,_3l){return _3l>=10?!E(_3k)?B(_1C(_33,_3i)):B(_1C(_38,_3i)):!E(_3k)?B(_1C(_33,_2Z)):B(_1C(_38,_2Z));},_3m=function(_3n){return E(_3n);},_3o=new T(function(){return B(unCStr(": empty list"));}),_3p=new T(function(){return B(unCStr("Prelude."));}),_3q=function(_3r){return new F(function(){return err(B(_J(_3p,new T(function(){return B(_J(_3r,_3o));},1))));});},_3s=new T(function(){return B(unCStr("head"));}),_3t=new T(function(){return B(_3q(_3s));}),_3u=function(_3v){return E(E(_3v)[1]);},_3w=function(_3x,_3y,_3z){while(1){var _3A=E(_3z);if(!_3A[0]){return false;}else{if(!B(A(_3u,[_3x,_3y,_3A[1]]))){_3z=_3A[2];continue;}else{return true;}}}},_3B=function(_3C,_3D,_3E,_3F,_3G,_3H){return !B(A(_3C,[_3E,_3G]))?true:!B(A(_3u,[_3D,_3F,_3H]))?true:false;},_3I=function(_3J,_3K,_3L,_3M){var _3N=E(_3L),_3O=E(_3M);return new F(function(){return _3B(E(_3J)[1],_3K,_3N[1],_3N[2],_3O[1],_3O[2]);});},_3P=function(_3Q,_3R,_3S,_3T,_3U,_3V){return !B(A(_3Q,[_3S,_3U]))?false:B(A(_3u,[_3R,_3T,_3V]));},_3W=function(_3X,_3Y,_3Z,_40){var _41=E(_3Z),_42=E(_40);return new F(function(){return _3P(E(_3X)[1],_3Y,_41[1],_41[2],_42[1],_42[2]);});},_43=function(_44,_45){return [0,function(_46,_47){return new F(function(){return _3W(_44,_45,_46,_47);});},function(_46,_47){return new F(function(){return _3I(_44,_45,_46,_47);});}];},_48=function(_49,_4a){return E(_49)[1]==E(_4a)[1];},_4b=function(_4c,_4d){return E(_4c)[1]!=E(_4d)[1];},_4e=[0,_48,_4b],_4f=new T(function(){return B(_43(_4e,_4e));}),_4g=function(_4h,_4i,_4j,_4k,_4l){var _4m=B(_2H(_4i,_9,function(_4n){return new F(function(){return _2H(_4j,_9,_3m,_4n);});},new T(function(){return E(E(_4h)[4]);})));if(!_4m[0]){return false;}else{var _4o=E(_4k),_4p=_4o[1];if(_4p<0){return false;}else{var _4q=E(_4l),_4r=_4q[1];if(_4r<0){return false;}else{var _4s=E(_4h),_4t=_4s[4];if(_4p>B(_E(_4t,0))){return false;}else{var _4u=E(_4t);if(!_4u[0]){return E(_3t);}else{if(_4r>B(_E(_4u[1],0))){return false;}else{var _4v=E(_4m[1]),_4w=_4v[1],_4x=E(_4v[2])[1];if(!B(_3w(_4f,[0,_4o,_4q],B(_1C(function(_4y){return new F(function(){return A(_4y,[[0,_4i,_4j]]);});},B(_3j(_4s[2],_4x))))))){return false;}else{var _4z=B(_2H(_4o,_9,function(_4A){return new F(function(){return _2H(_4q,_9,_3m,_4A);});},_4u));if(!_4z[0]){return true;}else{var _4B=E(_4z[1]);return _4x<=E(_4B[2])[1]?false:!E(_4B[1])?E(_4w):!E(_4w)?true:false;}}}}}}}}},_4C=function(_4D){return new F(function(){return err(B(unAppCStr("Oops!  Entered absent arg ",new T(function(){return B(unCStr(_4D));}))));});},_4E=new T(function(){return B(_4C("ww_sadc{v} [lid] random-1.1:System.Random.StdGen{tc r15E}"));}),_4F=new T(function(){return B(_4C("ww_sada{v} [lid] main:Game.BoardTrump.GameState.Phase{tc r1WQ}"));}),_4G=new T(function(){return B(_4C("ww_sad8{v} [lid] (main:Game.BoardTrump.Player.Player{tc r1IJ},\n                  main:Game.BoardTrump.Player.Player{tc r1IJ})"));}),_4H=function(_4I,_4J,_4K,_4L,_4M){var _4N=function(_4O){while(1){var _4P=(function(_4Q){var _4R=E(_4Q);if(!_4R[0]){return [0];}else{var _4S=_4R[2],_4T=new T(function(){return B(A(_4R[1],[[0,_4L,_4M]]));});if(!B(_4g([0,_4G,_4J,_4F,_4K,_4E],_4L,_4M,new T(function(){return E(E(_4T)[1]);},1),new T(function(){return E(E(_4T)[2]);},1)))){_4O=_4S;return null;}else{return [1,_4T,new T(function(){return B(_4N(_4S));})];}}})(_4O);if(_4P!=null){return _4P;}}};return new F(function(){return _4N(B(_3j(_4J,_4I)));});},_4U=function(_4V,_4W){while(1){var _4X=E(_4W);if(!_4X[0]){return E(_4V);}else{var _4Y=_4V+E(_4X[1])[1]|0;_4W=_4X[2];_4V=_4Y;continue;}}},_4Z=[0,2],_50=function(_51){return E(E(_51)[1])==1?E(_4Z):E(_3d);},_52=function(_53,_54){var _55=function(_56){return E(_53)==1?_56<=B(_4U(-2,B(_1C(_50,_54)))):_56<=B(_4U(-1,B(_1C(_50,_54))));};return _53<=10?E(_53)==1?B(_55(114514)):B(_55(0)):B(_55(2));},_57=0,_58=false,_59=true,_5a=[0,75],_5b=[1,_5a,_10],_5c=[0,81],_5d=[1,_5c,_10],_5e=[0,74],_5f=[1,_5e,_10],_5g=[0,65],_5h=[1,_5g,_10],_5i=function(_5j){var _5k=E(_5j);switch(_5k){case 1:return E(_5h);case 11:return E(_5f);case 12:return E(_5d);case 13:return E(_5b);default:return new F(function(){return _U(0,_5k,_10);});}},_5l=function(_5m,_5n,_5o,_5p){return new F(function(){return A(_5m,[function(_){var _5q=jsSetAttr(E(_5n)[1],toJSStr(E(_5o)),toJSStr(E(_5p)));return _57;}]);});},_5r=new T(function(){return B(unCStr("td"));}),_5s=function(_5t,_5u,_5v,_){var _5w=jsCreateElem(toJSStr(E(_5r))),_5x=_5w,_5y=jsAppendChild(_5x,E(_5v)[1]),_5z=[0,_5x],_5A=B(A(_5t,[_5u,_5z,_])),_5B=_5A;return _5z;},_5C=function(_5D,_){return new F(function(){return _5s(_c,_10,_5D,_);});},_5E=function(_5F){return E(_5F);},_5G=new T(function(){return B(unCStr("class"));}),_5H=new T(function(){return B(unCStr("computers-card"));}),_5I=new T(function(){return B(unCStr("your-card"));}),_5J=function(_5K){var _5L=E(_5K);if(!_5L[0]){return E(_5C);}else{var _5M=E(_5L[1]);return function(_5N,_){var _5O=B(_5s(_c,new T(function(){return B(_5i(E(_5M[2])[1]));}),_5N,_)),_5P=_5O,_5Q=B(A(_5l,[_5E,_5P,_5G,new T(function(){return !E(_5M[1])?E(_5H):E(_5I);}),_])),_5R=_5Q;return _5P;};}},_5S=new T(function(){return B(unCStr("tr"));}),_5T=function(_5U,_5V,_5W,_){var _5X=jsCreateElem(toJSStr(E(_5S))),_5Y=_5X,_5Z=jsAppendChild(_5Y,E(_5W)[1]),_60=[0,_5Y],_61=B(A(_5U,[_5V,_60,_])),_62=_61;return _60;},_63=function(_64){return E(_64);},_65=function(_66){return function(_1Y,_1Z){return new F(function(){return _5T(_63,function(_67,_){return new F(function(){return _i(new T(function(){return B(_1C(_5J,_66));}),_67,_);});},_1Y,_1Z);});};},_68=function(_69,_){return _57;},_6a=new T(function(){return B(unCStr("selectable-hand"));}),_6b=new T(function(){return B(_16("(function(e,c,x){x?e.classList.add(c):e.classList.remove(c);})"));}),_6c=function(_){var _=0;return new F(function(){return A(_16,["false",_]);});},_6d=new T(function(){return B(_12(_6c));}),_6e=function(_){var _=0;return new F(function(){return A(_16,["true",_]);});},_6f=new T(function(){return B(_12(_6e));}),_6g=function(_6h){return function(_6i){return function(_6j,_){var _6k=B(A(new T(function(){return B(A(new T(function(){return B(A(_6b,[E(E(_6h)[1])]));}),[E(toJSStr(E(_6i)))]));}),[!E(_6j)?E(_6d):E(_6f),_])),_6l=_6k;return _57;};};},_6m=function(_6n,_){while(1){var _6o=E(_6n);if(!_6o[0]){return _57;}else{var _6p=B(A(_6g,[_6o[1],_6a,_59,_])),_6q=_6p;_6n=_6o[2];continue;}}},_6r=function(_6s){var _6t=E(_6s);if(!_6t[0]){return [0];}else{return new F(function(){return _J(_6t[1],new T(function(){return B(_6r(_6t[2]));},1));});}},_6u=new T(function(){return B(_16("(function(e){return e.parentNode;})"));}),_6v=new T(function(){return B(unCStr("td"));}),_6w=function(_6x,_6y){while(1){var _6z=(function(_6A,_6B){var _6C=E(_6B);if(!_6C[0]){return [0];}else{var _6D=_6C[1],_6E=_6C[2];if(!B(A(_6A,[_6D]))){var _6F=_6A;_6y=_6E;_6x=_6F;return null;}else{return [1,_6D,new T(function(){return B(_6w(_6A,_6E));})];}}})(_6x,_6y);if(_6z!=null){return _6z;}}},_6G=function(_6H,_6I,_6J,_6K){var _6L=E(_6J);if(!_6L[0]){return E(_6I);}else{var _6M=E(_6K);if(!_6M[0]){return E(_6I);}else{return new F(function(){return A(_6H,[_6L[1],_6M[1],new T(function(){return B(_6G(_6H,_6I,_6L[2],_6M[2]));})]);});}}},_6N=[0,0],_6O=new T(function(){return B(unCStr("Maybe.fromJust: Nothing"));}),_6P=new T(function(){return B(err(_6O));}),_6Q=function(_6R){return E(_6R)[0]==0?true:false;},_6S=function(_6T,_6U){while(1){var _6V=E(_6U);if(!_6V[0]){return E(_6T);}else{_6T=_6V[1];_6U=_6V[2];continue;}}},_6W=new T(function(){return B(unCStr("last"));}),_6X=new T(function(){return B(_3q(_6W));}),_6Y=new T(function(){return B(unCStr("table#field"));}),_6Z=new T(function(){return B(unCStr("movable-card"));}),_70=new T(function(){return B(unCStr("id"));}),_71=new T(function(){return B(unCStr("moving-subject"));}),_72=new T(function(){return B(unCStr("motion-scope"));}),_73=new T(function(){return B(unCStr("obj-of-summon"));}),_74=new T(function(){return B(unCStr("\u306e\u756a\u3067\u3059\u3001"));}),_75=new T(function(){return B(unCStr(" ol.hand li"));}),_76=function(_77,_78,_79,_){if(!E(_78)){return new F(function(){return A(_79,[_]);});}else{var _7a=B(A(_6g,[_77,_6a,_59,_])),_7b=_7a;return new F(function(){return A(_79,[_]);});}},_7c=function(_){return _57;},_7d=new T(function(){return B(unCStr("summonable-zone"));}),_7e=function(_7f,_7g,_7h,_){if(!E(_7g)){return new F(function(){return A(_7h,[_]);});}else{var _7i=B(A(_5l,[_5E,_7f,_5G,_7d,_])),_7j=_7i;return new F(function(){return A(_7h,[_]);});}},_7k=new T(function(){return B(unCStr("#status"));}),_7l=[0,35],_7m=new T(function(){return B(unCStr("#field tr"));}),_7n=[0,0],_7o=function(_7p,_){return _7p;},_7q=function(_7r){return function(_1Y,_1Z){return new F(function(){return _i([1,function(_5D,_){return new F(function(){return _1c(_6Y,function(_7s,_){var _7t=E(_7s),_7u=jsClearChildren(_7t[1]),_7v=B(_i(new T(function(){return B(_1C(_65,E(_7r)[4]));}),_7t,_)),_7w=_7v;return _7t;},_5D,_);});},[1,function(_5D,_){return new F(function(){return _1c(_7k,function(_7x,_){var _7y=E(_7x),_7z=jsClearChildren(_7y[1]),_7A=B(A(new T(function(){var _7B=E(_7r),_7C=_7B[1],_7D=E(_7B[3]);if(_7D[0]==7){var _7E=function(_1Y,_1Z){return new F(function(){return _c(new T(function(){return !E(_7D[1])?E(_t):E(_s);}),_1Y,_1Z);});};}else{var _7E=function(_1Y,_1Z){return new F(function(){return _c(new T(function(){return B(unAppCStr("-- ",new T(function(){var _7F=new T(function(){return B(_J(_74,new T(function(){var _7G=E(_7D);switch(_7G[0]){case 0:var _7H=E(_r);break;case 1:var _7H=E(_q);break;case 2:var _7H=E(_z);break;case 3:var _7H=E(_y);break;case 4:var _7I=function(_7J){var _7K=E(_7J);return _7K[0]==0?E(new T(function(){return B(_J(B(_U(0,E(_7G[1])[1],_10)),_x));})):[1,_7K[1],new T(function(){return B(_7I(_7K[2]));})];},_7H=B(_7I(_w));break;case 5:var _7H=E(_v);break;default:var _7H=E(_u);}return _7H;},1)));},1);if(!E(_7B[2])){var _7L=B(_J(E(E(_7C)[2])[3],_7F));}else{var _7L=B(_J(E(E(_7C)[1])[3],_7F));}return _7L;})));}),_1Y,_1Z);});};}var _7M=_7E;return _7M;}),[_7y,_])),_7N=_7A;return _7y;},_5D,_);});},[1,function(_7O,_){var _7P=E(new T(function(){var _7Q=E(E(_7r)[1]);return [0,new T(function(){return B(_1G(_7Q[1]));}),new T(function(){return B(_1G(_7Q[2]));})];})),_7R=B(A(_7P[1],[_7O,_])),_7S=_7R,_7T=B(A(_7P[2],[_7O,_])),_7U=_7T;return _7O;},[1,new T(function(){var _7V=E(_7r),_7W=_7V[1],_7X=_7V[2],_7Y=_7V[4],_7Z=E(_7V[3]);switch(_7Z[0]){case 1:var _80=function(_81){var _82=E(_81);if(!_82[0]){return E(_68);}else{var _83=function(_84){var _85=E(_84);if(!_85[0]){return E(new T(function(){return B(_80(_82[2]));}));}else{var _86=new T(function(){return B(_83(_85[2]));});return function(_87,_){var _88=E(_87);if(!_88[0]){return _57;}else{var _89=_88[2],_8a=E(_88[1]);if(!_8a[0]){return new F(function(){return A(_86,[_89,_]);});}else{var _8b=E(_85[1]),_8c=_8b[1],_8d=B(A(_6u,[E(_8c),_])),_8e=_8d,_8f=B(_2t(_6N,_8e,_)),_8g=_8f,_8h=B(_2t(_7n,_8c,_)),_8i=_8h,_8j=E(_8a[1]),_8k=_8j[2];if(!E(_8j[1])){if(!E(_7X)){if(!B(_4H(E(_8k)[1],_58,_7Y,_8g,_8i))[0]){return new F(function(){return A(_86,[_89,_]);});}else{var _8l=B(A(_6g,[_8b,_6Z,_59,_])),_8m=_8l;return new F(function(){return A(_86,[_89,_]);});}}else{return new F(function(){return A(_86,[_89,_]);});}}else{if(!E(_7X)){return new F(function(){return A(_86,[_89,_]);});}else{if(!B(_4H(E(_8k)[1],_59,_7Y,_8g,_8i))[0]){return new F(function(){return A(_86,[_89,_]);});}else{var _8n=B(A(_6g,[_8b,_6Z,_59,_])),_8o=_8n;return new F(function(){return A(_86,[_89,_]);});}}}}}};}};return new F(function(){return _83(_82[1]);});}},_8p=function(_8q,_){var _8r=E(_8q),_8s=_8r[1],_8t=jsQuerySelectorAll(_8s,toJSStr([1,_7l,new T(function(){if(!E(_7X)){var _8u=B(_J(E(E(_7W)[2])[4],_75));}else{var _8u=B(_J(E(E(_7W)[1])[4],_75));}return _8u;})])),_8v=_8t,_8w=B(A(_6G,[_76,_7c,_8v,new T(function(){var _8x=function(_8y){return new F(function(){return (function(_8z){if(!E(_7X)){return new F(function(){return _52(_8z,E(E(_7W)[2])[1]);});}else{return new F(function(){return _52(_8z,E(E(_7W)[1])[1]);});}})(E(_8y)[1]);});};if(!E(_7X)){var _8A=B(_1C(_8x,E(E(_7W)[2])[1]));}else{var _8A=B(_1C(_8x,E(E(_7W)[1])[1]));}return _8A;}),_])),_8B=_8w,_8C=jsQuerySelectorAll(_8s,toJSStr(E(_7m))),_8D=_8C,_8E=E(_8D);if(!_8E[0]){return _8r;}else{var _8F=E(_6v),_8G=jsQuerySelectorAll(E(_8E[1])[1],toJSStr(_8F)),_8H=_8G,_8I=function(_8J,_){var _8K=E(_8J);if(!_8K[0]){return _10;}else{var _8L=jsQuerySelectorAll(E(_8K[1])[1],toJSStr(_8F)),_8M=_8L,_8N=B(_8I(_8K[2],_)),_8O=_8N;return [1,_8M,_8O];}},_8P=B(_8I(_8E[2],_)),_8Q=_8P,_8R=B(A(function(_8S,_8T){var _8U=function(_8V){var _8W=E(_8V);if(!_8W[0]){return E(new T(function(){return B(_80(_8T));}));}else{var _8X=new T(function(){return B(_8U(_8W[2]));});return function(_8Y,_){var _8Z=E(_8Y);if(!_8Z[0]){return _57;}else{var _90=_8Z[2],_91=E(_8Z[1]);if(!_91[0]){return new F(function(){return A(_8X,[_90,_]);});}else{var _92=E(_8W[1]),_93=_92[1],_94=B(A(_6u,[E(_93),_])),_95=_94,_96=B(_2t(_6N,_95,_)),_97=_96,_98=B(_2t(_7n,_93,_)),_99=_98,_9a=E(_91[1]),_9b=_9a[2];if(!E(_9a[1])){if(!E(_7X)){if(!B(_4H(E(_9b)[1],_58,_7Y,_97,_99))[0]){return new F(function(){return A(_8X,[_90,_]);});}else{var _9c=B(A(_6g,[_92,_6Z,_59,_])),_9d=_9c;return new F(function(){return A(_8X,[_90,_]);});}}else{return new F(function(){return A(_8X,[_90,_]);});}}else{if(!E(_7X)){return new F(function(){return A(_8X,[_90,_]);});}else{if(!B(_4H(E(_9b)[1],_59,_7Y,_97,_99))[0]){return new F(function(){return A(_8X,[_90,_]);});}else{var _9e=B(A(_6g,[_92,_6Z,_59,_])),_9f=_9e;return new F(function(){return A(_8X,[_90,_]);});}}}}}};}};return new F(function(){return _8U(_8S);});},[_8H,_8Q,new T(function(){return B(_6r(_7Y));}),_])),_9g=_8R;return _8r;}};break;case 2:var _9h=_7Z[1],_8p=function(_9i,_){var _9j=E(_9i),_9k=jsQuerySelectorAll(_9j[1],toJSStr(E(_7m))),_9l=_9k,_9m=_9l,_9n=E(_9h),_9o=E(_9n[1])[1];if(_9o>=0){var _9p=E(_6v),_9q=jsQuerySelectorAll(B(_4(_9m,_9o))[1],toJSStr(_9p)),_9r=_9q,_9s=E(_9n[2])[1];if(_9s>=0){var _9t=jsSetAttr(B(_4(_9r,_9s))[1],toJSStr(E(_70)),toJSStr(E(_71))),_9u=function(_,_9v){var _9w=B((function(_9x,_){while(1){var _9y=(function(_9z,_){var _9A=E(_9z);if(!_9A[0]){return _57;}else{var _9B=_9A[1],_9C=B(A(_6g,[new T(function(){return B(_2H(new T(function(){return E(B(A(_9B,[_9n]))[1]);}),_9,function(_9D){return new F(function(){return _2H(new T(function(){return E(B(A(_9B,[_9n]))[2]);}),_9,_3m,_9D);});},_9v));},1),_72,_59,_])),_9E=_9C;_9x=_9A[2];return null;}})(_9x,_);if(_9y!=null){return _9y;}}})(new T(function(){var _9F=B(_2H(new T(function(){return E(E(_9h)[1]);}),_9,function(_9G){return new F(function(){return _2H(new T(function(){return E(E(_9h)[2]);}),_9,_3m,_9G);});},_7Y));if(!_9F[0]){var _9H=E(_6P);}else{var _9H=B(_6w(function(_9I){var _9J=new T(function(){return B(A(_9I,[_9h]));});return new F(function(){return _4g(_7V,new T(function(){return E(E(_9h)[1]);}),new T(function(){return E(E(_9h)[2]);}),new T(function(){return E(E(_9J)[1]);},1),new T(function(){return E(E(_9J)[2]);},1));});},B(_3j(_7X,E(E(_9F[1])[2])[1]))));}return _9H;}),_)),_9K=_9w;return _9j;},_9L=E(_9m);if(!_9L[0]){return new F(function(){return _9u(_,_10);});}else{var _9M=jsQuerySelectorAll(E(_9L[1])[1],toJSStr(_9p)),_9N=_9M,_9O=function(_9P,_){var _9Q=E(_9P);if(!_9Q[0]){return _10;}else{var _9R=jsQuerySelectorAll(E(_9Q[1])[1],toJSStr(_9p)),_9S=_9R,_9T=B(_9O(_9Q[2],_)),_9U=_9T;return [1,_9S,_9U];}},_9V=B(_9O(_9L[2],_)),_9W=_9V;return new F(function(){return _9u(_,[1,_9N,_9W]);});}}else{return E(_1);}}else{return E(_1);}};break;case 4:var _8p=function(_9X,_){var _9Y=E(_9X),_9Z=jsQuerySelectorAll(_9Y[1],toJSStr([1,_7l,new T(function(){if(!E(_7X)){var _a0=B(_J(E(E(_7W)[2])[4],_75));}else{var _a0=B(_J(E(E(_7W)[1])[4],_75));}return _a0;})])),_a1=_9Z,_a2=B(_6m(_a1,_)),_a3=_a2;return _9Y;};break;case 5:var _8p=function(_a4,_){var _a5=E(_a4),_a6=_a5[1],_a7=jsQuerySelectorAll(_a6,toJSStr([1,_7l,new T(function(){if(!E(_7X)){var _a8=B(_J(E(E(_7W)[2])[4],_75));}else{var _a8=B(_J(E(E(_7W)[1])[4],_75));}return _a8;})])),_a9=_a7,_aa=E(_7Z[1])[1];if(_aa>=0){var _ab=jsSetAttr(B(_4(_a9,_aa))[1],toJSStr(E(_70)),toJSStr(E(_73))),_ac=jsQuerySelectorAll(_a6,toJSStr(E(_7m))),_ad=_ac,_ae=_ad,_af=function(_ag){var _ah=jsQuerySelectorAll(_ag,toJSStr(E(_6v))),_ai=_ah,_aj=B(A(_6G,[_7e,_7c,_ai,new T(function(){if(!E(_7X)){var _ak=E(_7Y),_al=_ak[0]==0?E(_3t):B(_1C(_6Q,_ak[1]));}else{var _am=E(_7Y);if(!_am[0]){var _an=E(_6X);}else{var _an=B(_1C(_6Q,B(_6S(_am[1],_am[2]))));}var _al=_an;}return _al;}),_])),_ao=_aj;return _a5;};if(!E(_7X)){var _ap=E(_ae);if(!_ap[0]){return E(_3t);}else{return new F(function(){return _af(E(_ap[1])[1]);});}}else{var _aq=E(_ae);if(!_aq[0]){return E(_6X);}else{return new F(function(){return _af(B(_6S(_aq[1],_aq[2]))[1]);});}}}else{return E(_1);}};break;default:var _8p=E(_7o);}var _ar=_8p;return _ar;}),_10]]]],_1Y,_1Z);});};},_as=[0],_at=function(_au,_av,_){var _aw=B(A(_au,[_])),_ax=_aw,_ay=E(_av),_az=jsSetTimeout(1000,_ay);return _57;},_aA=[1],_aB=[7,_58],_aC=[7,_59],_aD=function(_aE,_aF,_aG,_aH,_aI){var _aJ=function(_aK,_aL){var _aM=new T(function(){return !E(_aG)?[0,_aE,new T(function(){var _aN=E(_aF);return [0,[1,_aK,_aN[1]],_aN[2],_aN[3],_aN[4],_aN[5]];})]:[0,new T(function(){var _aO=E(_aE);return [0,[1,_aK,_aO[1]],_aO[2],_aO[3],_aO[4],_aO[5]];}),_aF];}),_aP=new T(function(){if(!E(_aG)){var _aQ=E(E(_aM)[2]),_aR=[0,_aQ[1],_aL,_aQ[3],_aQ[4],_aQ[5]];}else{var _aS=E(E(_aM)[1]),_aR=[0,_aS[1],_aL,_aS[3],_aS[4],_aS[5]];}return _aR;});return [0,new T(function(){if(!E(_aG)){var _aT=[0,E(_aM)[1],_aP];}else{var _aT=[0,_aP,E(_aM)[2]];}return _aT;}),_aG,_aA,_aH,_aI];};if(!E(_aG)){var _aU=E(_aF),_aV=E(_aU[2]);return _aV[0]==0?[0,[0,_aE,_aU],_58,_aC,_aH,_aI]:B(_aJ(_aV[1],_aV[2]));}else{var _aW=E(_aE),_aX=E(_aW[2]);return _aX[0]==0?[0,[0,_aW,_aF],_59,_aB,_aH,_aI]:B(_aJ(_aX[1],_aX[2]));}},_aY=function(_aZ){var _b0=E(_aZ),_b1=E(_b0[1]),_b2=B(_aD(_b1[1],_b1[2],_b0[2],_b0[4],_b0[5]));return [0,_b2[1],_b2[2],_b2[3],_b2[4],_b2[5]];},_b3=new T(function(){return B(unCStr("foldr1"));}),_b4=new T(function(){return B(_3q(_b3));}),_b5=function(_b6,_b7){var _b8=E(_b7);if(!_b8[0]){return E(_b4);}else{var _b9=_b8[1],_ba=E(_b8[2]);if(!_ba[0]){return E(_b9);}else{return new F(function(){return A(_b6,[_b9,new T(function(){return B(_b5(_b6,_ba));})]);});}}},_bb=function(_){var _bc=B(A(_16,["(function(){return document.body;})",_])),_bd=_bc;return [0,_bd];},_be=function(_bf,_bg,_bh,_){var _bi=rMV(_bf),_bj=_bi;if(!E(_bj)){return new F(function(){return A(_b5,[_at,[1,function(_){var _=wMV(_bf,_59);return _57;},[1,function(_){var _bk=E(_bg)[1],_bl=rMV(_bk),_bm=_bl,_=wMV(_bk,new T(function(){var _bn=E(_bm);return [0,_bn[1],new T(function(){return !E(_bn[2])?true:false;}),_as,_bn[4],_bn[5]];})),_bo=rMV(_bk),_bp=_bo,_bq=B(_bb(_)),_br=_bq,_bs=B(A(_7q,[_bp,_br,_])),_bt=_bs;return _57;},[1,function(_){var _bu=E(_bg)[1],_bv=rMV(_bu),_bw=_bv,_=wMV(_bu,new T(function(){return B(_aY(_bw));})),_bx=rMV(_bu),_by=_bx,_bz=B(_bb(_)),_bA=_bz,_bB=B(A(_7q,[_by,_bA,_])),_bC=_bB;return _57;},[1,function(_){var _bD=B(A(_bh,[_])),_bE=_bD,_=wMV(_bf,_58);return _57;},_10]]]],_]);});}else{return _57;}},_bF=function(_bG,_bH){return new F(function(){return A(_bG,[_bH]);});},_bI=[6],_bJ=function(_bK){return [0];},_bL=function(_bM,_bN,_bO,_bP,_bQ){if(!B(_4g(_bQ,_bM,_bN,_bO,_bP))){return [0];}else{var _bR=B(_2H(_bM,_9,function(_bS){return new F(function(){return _2H(_bN,_9,_3m,_bS);});},new T(function(){return E(E(_bQ)[4]);})));return _bR[0]==0?[0]:[1,new T(function(){var _bT=E(_bQ),_bU=_bT[1],_bV=_bT[4],_bW=_bT[5],_bX=new T(function(){return B(_2H(_bM,_bF,function(_bY){return new F(function(){return _2H(_bN,_bF,_bJ,_bY);});},new T(function(){return B(_2H(_bO,_bF,function(_bZ){return new F(function(){return _2H(_bP,_bF,function(_c0){return [1,_bR[1]];},_bZ);});},_bV));})));});if(!E(_bT[2])){var _c1=(E(_bO)[1]+1|0)!=B(_E(_bV,0))?[0,_bU,_58,_bI,_bX,_bW]:[0,_bU,_58,_aB,_bX,_bW];}else{var _c1=E(E(_bO)[1])==0?[0,_bU,_59,_aC,_bX,_bW]:[0,_bU,_59,_bI,_bX,_bW];}var _c2=_c1;return _c2;})];}},_c3=function(_c4,_c5,_c6){var _c7=B(_2H(_c4,_9,function(_c8){return new F(function(){return _2H(_c5,_9,_3m,_c8);});},new T(function(){return E(E(_c6)[4]);})));if(!_c7[0]){return [0];}else{var _c9=_c7[1],_ca=E(_c6),_cb=_ca[2],_cc=_ca[4],_cd=function(_ce){return B(_4H(E(E(_c9)[2])[1],_cb,_cc,_c4,_c5))[0]==0?[0]:[1,[0,_ca[1],_cb,[2,[0,_c4,_c5]],_cc,_ca[5]]];};return !E(_cb)?!E(E(_c9)[1])?B(_cd(_)):[0]:!E(E(_c9)[1])?[0]:B(_cd(_));}},_cf=[0,114514],_cg=function(_ch,_ci){var _cj=new T(function(){var _ck=_ch+1|0;return _ck>=0?B(_2C(_ck,_ci)):E(_ci);});if(_ch>0){var _cl=function(_cm,_cn){var _co=E(_cm);if(!_co[0]){return E(_cj);}else{var _cp=_co[1];return _cn>1?[1,_cp,new T(function(){return B(_cl(_co[2],_cn-1|0));})]:[1,_cp,_cj];}};return new F(function(){return _cl(_ci,_ch);});}else{return E(_cj);}},_cq=function(_cr,_cs){return new F(function(){return _cg(E(_cr)[1],_cs);});},_ct=function(_cu,_cv){var _cw=E(_cv);return _cw[0]==0?[0]:[1,_cu,new T(function(){return B(_ct(_cw[1],_cw[2]));})];},_cx=new T(function(){return B(unCStr("init"));}),_cy=new T(function(){return B(_3q(_cx));}),_cz=new T(function(){return B(unCStr("tail"));}),_cA=new T(function(){return B(_3q(_cz));}),_cB=function(_cC,_cD,_cE,_cF){return B(_2H(_cD,_9,_3m,new T(function(){var _cG=E(_cF),_cH=_cG[4];if(!E(_cG[2])){var _cI=E(_cH),_cJ=_cI[0]==0?E(_3t):E(_cI[1]);}else{var _cK=E(_cH),_cJ=_cK[0]==0?E(_6X):B(_6S(_cK[1],_cK[2]));}var _cL=_cJ;return _cL;})))[0]==0?[1,new T(function(){var _cM=E(_cE),_cN=_cM[1],_cO=function(_cP){var _cQ=E(_cF),_cR=_cQ[1],_cS=_cQ[2],_cT=_cQ[4],_cU=_cQ[5],_cV=new T(function(){var _cW=new T(function(){return B(_2H(_cD,_bF,function(_cX){return E([1,[0,_cS,_cM]]);},new T(function(){if(!E(_cS)){var _cY=E(_cT),_cZ=_cY[0]==0?E(_3t):E(_cY[1]);}else{var _d0=E(_cT),_cZ=_d0[0]==0?E(_6X):B(_6S(_d0[1],_d0[2]));}return _cZ;})));});if(!E(_cS)){var _d1=[1,_cW,new T(function(){var _d2=E(_cT);return _d2[0]==0?E(_cA):E(_d2[2]);})];}else{var _d3=E(_cT);if(!_d3[0]){var _d4=E(_cy);}else{var _d4=B(_J(B(_ct(_d3[1],_d3[2])),[1,_cW,_10]));}var _d1=_d4;}return _d1;}),_d5=new T(function(){if(!E(_cS)){var _d6=E(_cR),_d7=[0,_d6[1],new T(function(){var _d8=E(_d6[2]);return [0,new T(function(){return B(_cq(_cC,_d8[1]));}),_d8[2],_d8[3],_d8[4],_d8[5]];})];}else{var _d9=E(_cR),_d7=[0,new T(function(){var _da=E(_d9[1]);return [0,new T(function(){return B(_cq(_cC,_da[1]));}),_da[2],_da[3],_da[4],_da[5]];}),_d9[2]];}return _d7;});return E(_cP)==0?[0,_d5,_cS,_bI,_cV,_cU]:[0,_d5,_cS,[4,new T(function(){return _cN<=10?E(_cN)==1?E(_cf):E(_2W):E(_4Z);})],_cV,_cU];};if(_cN<=10){if(E(_cN)==1){var _db=B(_cO(114514)),_dc=[0,_db[1],_db[2],_db[3],_db[4],_db[5]];}else{var _dd=B(_cO(0)),_dc=[0,_dd[1],_dd[2],_dd[3],_dd[4],_dd[5]];}var _de=_dc;}else{var _df=B(_cO(2)),_de=[0,_df[1],_df[2],_df[3],_df[4],_df[5]];}var _dg=_de,_dh=_dg;return _dh;})]:[0];},_di=function(_dj,_dk,_dl,_dm,_dn,_do,_dp){var _dq=[0,_dl,_dm,_dn,_do,_dp],_dr=E(_dn);switch(_dr[0]){case 1:var _ds=B(_c3(_dj,_dk,_dq));return _ds[0]==0?E(_dq):E(_ds[1]);case 2:var _dt=E(_dr[1]),_du=_dt[2],_dv=E(_dt[1]),_dw=E(_dj);if(_dv[1]!=_dw[1]){var _dx=B(_bL(_dv,_du,_dw,_dk,_dq));return _dx[0]==0?E(_dq):E(_dx[1]);}else{var _dy=E(_du),_dz=E(_dk);if(_dy[1]!=_dz[1]){var _dA=B(_bL(_dv,_dy,_dw,_dz,_dq));return _dA[0]==0?E(_dq):E(_dA[1]);}else{return [0,_dl,_dm,_aA,_do,_dp];}}break;case 5:var _dB=_dr[1],_dC=function(_dD){var _dE=function(_dF){var _dG=B(_cB(_dB,_dk,new T(function(){var _dH=E(_dB)[1];if(_dH>=0){if(!E(_dm)){var _dI=B(_4(E(E(_dl)[2])[1],_dH));}else{var _dI=B(_4(E(E(_dl)[1])[1],_dH));}var _dJ=_dI;}else{var _dJ=E(_1);}var _dK=_dJ,_dL=_dK;return _dL;},1),_dq));return _dG[0]==0?E(_dq):E(_dG[1]);};if(!E(_dm)){return E(E(_dj)[1])==0?B(_dE(_)):E(_dq);}else{return new F(function(){return _dE(_);});}};if(!E(_dm)){return new F(function(){return _dC(_);});}else{return E(_dj)[1]!=(B(_E(_do,0))-1|0)?E(_dq):B(_dC(_));}break;default:return E(_dq);}},_dM=new T(function(){return B(unCStr("Prelude.Enum.pred{Int}: tried to take `pred\' of minBound"));}),_dN=new T(function(){return B(err(_dM));}),_dO=function(_dP,_dQ){return [0,imul(E(_dP)[1],E(_dQ)[1])|0];},_dR=function(_dS,_dT){return [0,E(_dS)[1]-E(_dT)[1]|0];},_dU=function(_dV){var _dW=E(_dV),_dX=_dW[1];return _dX<0?[0, -_dX]:E(_dW);},_dY=function(_dZ){var _e0=E(_dZ);return _e0[0]==0?E(_e0[1]):I_toInt(_e0[1]);},_e1=function(_e2){return [0,B(_dY(_e2))];},_e3=function(_e4){return [0, -E(_e4)[1]];},_e5=[0,-1],_e6=[0,0],_e7=[0,1],_e8=function(_e9){var _ea=E(_e9)[1];return _ea>=0?E(_ea)==0?E(_e6):E(_e7):E(_e5);},_eb=[0,_30,_dO,_dR,_e3,_dU,_e8,_e1],_ec=[0,1],_ed=function(_ee,_ef){return [0,E(_ee)[1],E(_ef)[1]];},_eg=function(_eh,_ei){var _ej=quot(_ei,52774),_ek=(imul(40692,_ei-(imul(_ej,52774)|0)|0)|0)-(imul(_ej,3791)|0)|0,_el=new T(function(){if(_ek>=0){var _em=[0,_ek];}else{var _em=[0,_ek+2147483399|0];}var _en=_em;return _en;}),_eo=quot(_eh,53668),_ep=(imul(40014,_eh-(imul(_eo,53668)|0)|0)|0)-(imul(_eo,12211)|0)|0,_eq=new T(function(){if(_ep>=0){var _er=[0,_ep];}else{var _er=[0,_ep+2147483563|0];}var _es=_er;return _es;});return [0,new T(function(){var _et=E(_eq)[1]-E(_el)[1]|0;if(_et>=1){var _eu=[0,_et];}else{var _eu=[0,_et+2147483562|0];}var _ev=_eu,_ew=_ev,_ex=_ew,_ey=_ex;return _ey;}),new T(function(){return B(_ed(_eq,_el));})];},_ez=[0,2147483562],_eA=new T(function(){return B(unCStr("base"));}),_eB=new T(function(){return B(unCStr("GHC.Exception"));}),_eC=new T(function(){return B(unCStr("ArithException"));}),_eD=[0,I_fromBits([4194982440,719304104]),I_fromBits([3110813675,1843557400]),_eA,_eB,_eC],_eE=[0,I_fromBits([4194982440,719304104]),I_fromBits([3110813675,1843557400]),_eD,_10],_eF=function(_eG){return E(_eE);},_eH=function(_eI){return E(E(_eI)[1]);},_eJ=function(_eK,_eL,_eM){var _eN=B(A(_eK,[_])),_eO=B(A(_eL,[_])),_eP=hs_eqWord64(_eN[1],_eO[1]),_eQ=_eP;if(!E(_eQ)){return [0];}else{var _eR=hs_eqWord64(_eN[2],_eO[2]),_eS=_eR;return E(_eS)==0?[0]:[1,_eM];}},_eT=function(_eU){var _eV=E(_eU);return new F(function(){return _eJ(B(_eH(_eV[1])),_eF,_eV[2]);});},_eW=new T(function(){return B(unCStr("arithmetic underflow"));}),_eX=new T(function(){return B(unCStr("arithmetic overflow"));}),_eY=new T(function(){return B(unCStr("Ratio has zero denominator"));}),_eZ=new T(function(){return B(unCStr("denormal"));}),_f0=new T(function(){return B(unCStr("divide by zero"));}),_f1=new T(function(){return B(unCStr("loss of precision"));}),_f2=function(_f3){switch(E(_f3)){case 0:return E(_eX);case 1:return E(_eW);case 2:return E(_f1);case 3:return E(_f0);case 4:return E(_eZ);default:return E(_eY);}},_f4=function(_f5){return new F(function(){return _J(_eW,_f5);});},_f6=function(_f5){return new F(function(){return _J(_eX,_f5);});},_f7=function(_f5){return new F(function(){return _J(_eY,_f5);});},_f8=function(_f5){return new F(function(){return _J(_eZ,_f5);});},_f9=function(_f5){return new F(function(){return _J(_f0,_f5);});},_fa=function(_f5){return new F(function(){return _J(_f1,_f5);});},_fb=function(_fc){switch(E(_fc)){case 0:return E(_f6);case 1:return E(_f4);case 2:return E(_fa);case 3:return E(_f9);case 4:return E(_f8);default:return E(_f7);}},_fd=[0,44],_fe=[0,93],_ff=[0,91],_fg=function(_fh,_fi,_fj){var _fk=E(_fi);return _fk[0]==0?B(unAppCStr("[]",_fj)):[1,_ff,new T(function(){return B(A(_fh,[_fk[1],new T(function(){var _fl=function(_fm){var _fn=E(_fm);return _fn[0]==0?E([1,_fe,_fj]):[1,_fd,new T(function(){return B(A(_fh,[_fn[1],new T(function(){return B(_fl(_fn[2]));})]));})];};return B(_fl(_fk[2]));})]));})];},_fo=function(_fp,_fq){return new F(function(){return _fg(_fb,_fp,_fq);});},_fr=function(_fs,_ft){switch(E(_ft)){case 0:return E(_f6);case 1:return E(_f4);case 2:return E(_fa);case 3:return E(_f9);case 4:return E(_f8);default:return E(_f7);}},_fu=[0,_fr,_f2,_fo],_fv=new T(function(){return [0,_eF,_fu,_fw,_eT];}),_fw=function(_f5){return [0,_fv,_f5];},_fx=3,_fy=new T(function(){return B(_fw(_fx));}),_fz=new T(function(){return die(_fy);}),_fA=function(_fB,_fC){var _fD=E(_fB);if(!_fD[0]){var _fE=_fD[1],_fF=E(_fC);return _fF[0]==0?_fE==_fF[1]:I_compareInt(_fF[1],_fE)==0?true:false;}else{var _fG=_fD[1],_fH=E(_fC);return _fH[0]==0?I_compareInt(_fG,_fH[1])==0?true:false:I_compare(_fG,_fH[1])==0?true:false;}},_fI=function(_fJ){return E(E(_fJ)[7]);},_fK=function(_fL,_fM){var _fN=E(_fL);if(!_fN[0]){var _fO=_fN[1],_fP=E(_fM);return _fP[0]==0?_fO>=_fP[1]:I_compareInt(_fP[1],_fO)<=0;}else{var _fQ=_fN[1],_fR=E(_fM);return _fR[0]==0?I_compareInt(_fQ,_fR[1])>=0:I_compare(_fQ,_fR[1])>=0;}},_fS=[0,0],_fT=function(_fU,_fV){var _fW=E(_fU);if(!_fW[0]){var _fX=_fW[1],_fY=E(_fV);return _fY[0]==0?_fX>_fY[1]:I_compareInt(_fY[1],_fX)<0;}else{var _fZ=_fW[1],_g0=E(_fV);return _g0[0]==0?I_compareInt(_fZ,_g0[1])>0:I_compare(_fZ,_g0[1])>0;}},_g1=[0,1000],_g2=function(_g3,_g4){while(1){var _g5=E(_g3);if(!_g5[0]){var _g6=_g5[1],_g7=E(_g4);if(!_g7[0]){var _g8=_g7[1],_g9=subC(_g6,_g8);if(!E(_g9[2])){return [0,_g9[1]];}else{_g3=[1,I_fromInt(_g6)];_g4=[1,I_fromInt(_g8)];continue;}}else{_g3=[1,I_fromInt(_g6)];_g4=_g7;continue;}}else{var _ga=E(_g4);if(!_ga[0]){_g3=_g5;_g4=[1,I_fromInt(_ga[1])];continue;}else{return [1,I_sub(_g5[1],_ga[1])];}}}},_gb=function(_gc,_gd){var _ge=_gc%_gd;if(_gc<=0){if(_gc>=0){return E(_ge);}else{if(_gd<=0){return E(_ge);}else{var _gf=E(_ge);return _gf==0?0:_gf+_gd|0;}}}else{if(_gd>=0){if(_gc>=0){return E(_ge);}else{if(_gd<=0){return E(_ge);}else{var _gg=E(_ge);return _gg==0?0:_gg+_gd|0;}}}else{var _gh=E(_ge);return _gh==0?0:_gh+_gd|0;}}},_gi=function(_gj,_gk){while(1){var _gl=E(_gj);if(!_gl[0]){var _gm=E(_gl[1]);if(_gm==(-2147483648)){_gj=[1,I_fromInt(-2147483648)];continue;}else{var _gn=E(_gk);if(!_gn[0]){return [0,B(_gb(_gm,_gn[1]))];}else{_gj=[1,I_fromInt(_gm)];_gk=_gn;continue;}}}else{var _go=_gl[1],_gp=E(_gk);return _gp[0]==0?[0,I_toInt(I_mod(_go,I_fromInt(_gp[1])))]:[1,I_mod(_go,_gp[1])];}}},_gq=function(_gr,_gs){while(1){var _gt=E(_gr);if(!_gt[0]){var _gu=_gt[1],_gv=E(_gs);if(!_gv[0]){var _gw=_gv[1],_gx=addC(_gu,_gw);if(!E(_gx[2])){return [0,_gx[1]];}else{_gr=[1,I_fromInt(_gu)];_gs=[1,I_fromInt(_gw)];continue;}}else{_gr=[1,I_fromInt(_gu)];_gs=_gv;continue;}}else{var _gy=E(_gs);if(!_gy[0]){_gr=_gt;_gs=[1,I_fromInt(_gy[1])];continue;}else{return [1,I_add(_gt[1],_gy[1])];}}}},_gz=function(_gA){return [0,_gA];},_gB=function(_gC,_gD){while(1){var _gE=E(_gC);if(!_gE[0]){var _gF=_gE[1],_gG=E(_gD);if(!_gG[0]){var _gH=_gG[1];if(!(imul(_gF,_gH)|0)){return [0,imul(_gF,_gH)|0];}else{_gC=[1,I_fromInt(_gF)];_gD=[1,I_fromInt(_gH)];continue;}}else{_gC=[1,I_fromInt(_gF)];_gD=_gG;continue;}}else{var _gI=E(_gD);if(!_gI[0]){_gC=_gE;_gD=[1,I_fromInt(_gI[1])];continue;}else{return [1,I_mul(_gE[1],_gI[1])];}}}},_gJ=function(_gK,_gL,_gM,_gN){while(1){var _gO=(function(_gP,_gQ,_gR,_gS){if(!B(_fT(_gQ,_gR))){var _gT=B(_gq(B(_g2(_gR,_gQ)),_ec)),_gU=B((function(_gV,_gW,_gX){while(1){if(!B(_fK(_gV,B(_gB(_gT,_g1))))){var _gY=E(_gX),_gZ=B(_eg(_gY[1],_gY[2])),_h0=B(_gB(_gV,_ez)),_h1=B(_gq(B(_gB(_gW,_ez)),B(_g2(B(_gz(E(_gZ[1])[1])),_ec))));_gX=_gZ[2];_gV=_h0;_gW=_h1;continue;}else{return [0,_gW,_gX];}}})(_ec,_fS,_gS));return [0,new T(function(){return B(A(_fI,[_gP,new T(function(){if(!B(_fA(_gT,_fS))){var _h2=B(_gq(_gQ,B(_gi(_gU[1],_gT))));}else{var _h2=E(_fz);}return _h2;})]));}),_gU[2]];}else{var _h3=_gP,_h4=_gR,_h5=_gQ,_h6=_gS;_gK=_h3;_gL=_h4;_gM=_h5;_gN=_h6;return null;}})(_gK,_gL,_gM,_gN);if(_gO!=null){return _gO;}}},_h7=[0,0],_h8=function(_h9){var _ha=E(_h9);if(_ha==(-2147483648)){return E(_dN);}else{var _hb=_ha-1|0;if(0<=_hb){var _hc=function(_hd){return [1,[0,[0,_hd]],new T(function(){if(_hd!=_hb){var _he=B(_hc(_hd+1|0));}else{var _he=[0];}var _hf=_he;return _hf;})];};return new F(function(){return _hc(0);});}else{return [0];}}},_hg=function(_hh,_hi,_hj,_hk){return new F(function(){return A(_hi,[function(_hl){if(!E(_hh)){return [1,_hl,new T(function(){var _hm=E(_hk);return _hm[0]==0?E(_cA):E(_hm[2]);})];}else{var _hn=E(_hk);if(!_hn[0]){return E(_cy);}else{return new F(function(){return _J(B(_ct(_hn[1],_hn[2])),[1,_hl,_10]);});}}},new T(function(){return B(A(_hj,[new T(function(){if(!E(_hh)){var _ho=E(_hk),_hp=_ho[0]==0?E(_3t):E(_ho[1]);}else{var _hq=E(_hk),_hp=_hq[0]==0?E(_6X):B(_6S(_hq[1],_hq[2]));}return _hp;})]));})]);});},_hr=[2],_hs=function(_ht){return [1,_ht];},_hu=function(_ht){return [0,_ht];},_hv=function(_hw,_hx){while(1){var _hy=E(_hx);if(!_hy[0]){return true;}else{if(!B(A(_hw,[_hy[1]]))){return false;}else{_hx=_hy[2];continue;}}}},_hz=function(_hA){while(1){var _hB=(function(_hC){var _hD=E(_hC);if(!_hD[0]){return [0];}else{var _hE=_hD[2],_hF=E(_hD[1]);if(!_hF[0]){_hA=_hE;return null;}else{return [1,_hF[1],new T(function(){return B(_hz(_hE));})];}}})(_hA);if(_hB!=null){return _hB;}}},_hG=function(_hH,_hI){if(_hH<=_hI){var _hJ=function(_hK){return [1,[0,_hK],new T(function(){if(_hK!=_hI){var _hL=B(_hJ(_hK+1|0));}else{var _hL=[0];}var _hM=_hL;return _hM;})];};return new F(function(){return _hJ(_hH);});}else{return [0];}},_hN=function(_hO,_hP){var _hQ=E(_hO);if(!_hQ[0]){return E(_3t);}else{var _hR=B(_E(_hQ[1],0));if(_hR==(-2147483648)){return E(_dN);}else{var _hS=_hR-1|0;if(0<=_hS){var _hT=function(_hU){return [1,new T(function(){var _hV=[0,_hU],_hW=function(_hX){var _hY=E(_hX);if(!_hY[0]){return [0];}else{var _hZ=_hY[1];return [1,new T(function(){return B(A(_hP,[_hZ,_hV,new T(function(){return B(_2H(_hZ,_9,function(_i0){return new F(function(){return _2H(_hV,_9,_3m,_i0);});},_hQ));})]));}),new T(function(){return B(_hW(_hY[2]));})];}};return B(_hW(new T(function(){var _i1=B(_E(_hQ,0));if(_i1==(-2147483648)){var _i2=E(_dN);}else{var _i2=B(_hG(0,_i1-1|0));}return _i2;})));}),new T(function(){if(_hU!=_hS){var _i3=B(_hT(_hU+1|0));}else{var _i3=[0];}var _i4=_i3;return _i4;})];};return new F(function(){return _hT(0);});}else{return [0];}}}},_i5=function(_i6){return E(_i6)[0]==0?false:true;},_i7=function(_i8){var _i9=E(_i8);return !E(_i9[2])?[0]:[1,_i9[1]];},_ia=new T(function(){return B(_hG(0,2147483647));}),_ib=function(_ic){var _id=E(_ic);return E(_id[2])[0]==0?[1,_id[1]]:[0];},_ie=function(_if,_ig){while(1){var _ih=(function(_ii,_ij){var _ik=E(_ij);if(!_ik[0]){return [0];}else{var _il=_ik[2],_im=B(A(_ii,[_ik[1]]));if(!_im[0]){var _in=_ii;_ig=_il;_if=_in;return null;}else{return [1,_im[1],new T(function(){return B(_ie(_ii,_il));})];}}})(_if,_ig);if(_ih!=null){return _ih;}}},_io=function(_ip,_iq){var _ir=E(_ip);if(!_ir[0]){return [0];}else{var _is=E(_iq);return _is[0]==0?[0]:[1,[0,_ir[1],_is[1]],new T(function(){return B(_io(_ir[2],_is[2]));})];}},_it=function(_iu){return [0,_hr,new T(function(){var _iv=E(_iu),_iw=_iv[1],_ix=_iv[2],_iy=_iv[4],_iz=E(_iv[3]);switch(_iz[0]){case 1:var _iA=function(_iB){var _iC=E(_iB);return _iC[0]==0?E(new T(function(){if(!B(_hv(_i5,B(_hg(_ix,_9,_3m,_iy))))){var _iD=B(_1C(_hu,B(_ie(_i7,B(_io(_ia,new T(function(){if(!E(_ix)){var _iE=E(E(_iw)[2])[1],_iF=B(_1C(function(_iG){return new F(function(){return _52(E(_iG)[1],_iE);});},_iE));}else{var _iH=E(E(_iw)[1])[1],_iF=B(_1C(function(_iI){return new F(function(){return _52(E(_iI)[1],_iH);});},_iH));}return _iF;},1)))))));}else{var _iD=[0];}var _iJ=_iD;return _iJ;})):[1,[1,_iC[1]],new T(function(){return B(_iA(_iC[2]));})];},_iK=B(_iA(B(_hz(B(_6r(B(_hN(_iy,function(_iL,_iM,_iN){var _iO=E(_iN);if(!_iO[0]){return [0];}else{var _iP=E(_iO[1]),_iQ=function(_iR){return B(_4H(E(_iP[2])[1],_ix,_iy,_iL,_iM))[0]==0?[0]:[1,[0,_iL,_iM]];};return !E(_iP[1])?!E(_ix)?B(_iQ(_)):[0]:!E(_ix)?[0]:B(_iQ(_));}}))))))));break;case 2:var _iS=E(_iz[1]),_iT=_iS[1],_iU=_iS[2],_iV=B(_2H(_iT,_9,function(_iW){return new F(function(){return _2H(_iU,_9,_3m,_iW);});},_iy));if(!_iV[0]){var _iX=E(_6P);}else{var _iX=B(_1C(_hs,B(_4H(E(E(_iV[1])[2])[1],_ix,_iy,_iT,_iU))));}var _iY=_iX,_iK=_iY;break;case 4:if(!E(_ix)){var _iZ=B(_h8(B(_E(E(E(_iw)[2])[1],0))));}else{var _iZ=B(_h8(B(_E(E(E(_iw)[1])[1],0))));}var _iK=_iZ;break;case 5:var _iK=B(_1C(function(_j0){return [1,[0,new T(function(){if(!E(_ix)){var _j1=E(_h7);}else{var _j2=B(_E(_iy,0));if(_j2==(-2147483648)){var _j3=E(_dN);}else{var _j3=[0,_j2-1|0];}var _j1=_j3;}return _j1;}),_j0]];},B(_ie(_ib,B(_io(_ia,new T(function(){return B(_hg(_ix,_9,_3m,_iy));},1)))))));break;default:var _iK=[0];}var _j4=_iK;return _j4;})];},_j5=function(_j6){var _j7=B(_it(_j6));return [1,_j7[1],_j7[2]];},_j8=[0,0],_j9=function(_ja){var _jb=new T(function(){var _jc=B(_E(B(_j5(_ja)),0));if(_jc==(-2147483648)){var _jd=E(_dN);}else{var _je=B(_gJ(_eb,_j8,B(_gz(_jc-1|0)),new T(function(){return E(E(_ja)[5]);}))),_jd=[0,_je[1],_je[2]];}var _jf=_jd;return _jf;});return [0,new T(function(){return B(_2H(new T(function(){return E(E(_jb)[1]);}),_9,_3m,new T(function(){return B(_j5(_ja));})));}),new T(function(){return E(E(_jb)[2]);})];},_jg=function(_jh,_ji,_jj,_jk,_jl,_jm){var _jn=B(_2H(_jh,_9,_3m,new T(function(){if(!E(_jk)){var _jo=E(E(_jj)[1]);}else{var _jo=E(E(_ji)[1]);}return _jo;})))[1];if(!E(_jk)){var _jp=E(_jj);return !B(_52(_jn,_jp[1]))?[0]:[1,[0,[0,_ji,_jp],_58,[5,_jh],_jl,_jm]];}else{var _jq=E(_ji);return !B(_52(_jn,_jq[1]))?[0]:[1,[0,[0,_jq,_jj],_59,[5,_jh],_jl,_jm]];}},_jr=function(_js){return [0,E(E(_js))];},_jt=function(_ju,_jv){return E(_ju);},_jw=[0,_bF,_jt],_jx=[0,2147483647],_jy=[0,-2147483648],_jz=[0,2147483562],_jA=[0,1],_jB=[0,_jA,_jz],_jC=function(_jD){return E(_jB);},_jE=function(_jF){var _jG=E(_jF),_jH=B(_eg(_jG[1],_jG[2]));return [0,_jH[1],_jH[2]];},_jI=function(_jJ,_jK){var _jL=new T(function(){return E(B(_eg(_jJ,_jK))[2]);});return [0,new T(function(){var _jM=E(_jJ);if(_jM==2147483562){var _jN=[0,1,E(_jL)[2]];}else{var _jN=[0,_jM+1|0,E(_jL)[2]];}return _jN;}),new T(function(){var _jO=E(_jL)[1],_jP=E(_jK);if(_jP==1){var _jQ=[0,_jO,2147483398];}else{var _jQ=[0,_jO,_jP-1|0];}var _jR=_jQ;return _jR;})];},_jS=function(_jT){var _jU=E(_jT),_jV=B(_jI(_jU[1],_jU[2]));return [0,_jV[1],_jV[2]];},_jW=[0,_jE,_jC,_jS],_jX=function(_jY){return E(E(_jY)[2]);},_jZ=function(_k0){return E(E(_k0)[1]);},_k1=function(_k2,_k3,_k4,_k5,_k6){while(1){var _k7=(function(_k8,_k9,_ka,_kb,_kc){if(!B(_fT(_ka,_kb))){var _kd=B(_gq(B(_g2(_kb,_ka)),_ec)),_ke=new T(function(){return B(A(_jX,[_k8,_kc]));}),_kf=new T(function(){return E(E(_ke)[1]);}),_kg=new T(function(){return B(_gq(B(_g2(B(_gz(E(E(_ke)[2])[1])),B(_gz(E(_kf)[1])))),_ec));}),_kh=B((function(_ki,_kj,_kk){while(1){if(!B(_fK(_ki,B(_gB(_kd,_g1))))){var _kl=B(A(new T(function(){return B(_jZ(_k8));}),[_kk])),_km=B(_gB(_ki,_kg)),_kn=B(_gq(B(_gB(_kj,_kg)),B(_g2(B(_gz(E(_kl[1])[1])),new T(function(){return B(_gz(E(_kf)[1]));})))));_kk=_kl[2];_ki=_km;_kj=_kn;continue;}else{return [0,_kj,_kk];}}})(_ec,_fS,_kc));return [0,new T(function(){return B(A(_fI,[_k9,new T(function(){if(!B(_fA(_kd,_fS))){var _ko=B(_gq(_ka,B(_gi(_kh[1],_kd))));}else{var _ko=E(_fz);}return _ko;})]));}),_kh[2]];}else{var _kp=_k8,_kq=_k9,_kr=_kb,_ks=_ka,_kt=_kc;_k2=_kp;_k3=_kq;_k4=_kr;_k5=_ks;_k6=_kt;return null;}})(_k2,_k3,_k4,_k5,_k6);if(_k7!=null){return _k7;}}},_ku=[0,0],_kv=function(_kw,_kx,_ky){var _kz=E(_kx);if(!_kz){return [0];}else{var _kA=new T(function(){var _kB=B(_k1(_kw,_eb,_ku,B(_gz(_kz)),_ky));return [0,_kB[1],_kB[2]];});return [1,[0,new T(function(){return E(E(_kA)[1]);}),_ky],new T(function(){return B(_kv(_kw,_kz-1|0,new T(function(){return E(E(_kA)[2]);})));})];}},_kC=function(_kD){var _kE=E(_kD);if(!_kE[0]){return [0,_10,_10];}else{var _kF=E(_kE[1]),_kG=new T(function(){var _kH=B(_kC(_kE[2]));return [0,_kH[1],_kH[2]];});return [0,[1,_kF[1],new T(function(){return E(E(_kG)[1]);})],[1,_kF[2],new T(function(){return E(E(_kG)[2]);})]];}},_kI=new T(function(){return B(unCStr("[extractTree] impossible"));}),_kJ=new T(function(){return B(err(_kI));}),_kK=function(_kL,_kM){var _kN=function(_kO){var _kP=E(_kM);if(!_kP[0]){return E(_kJ);}else{var _kQ=_kP[1],_kR=_kP[3],_kS=E(_kP[2]);if(!_kS[0]){var _kT=new T(function(){var _kU=B(_kK(_kL-1|0,_kR));return [0,_kU[1],_kU[2]];});return [0,new T(function(){return E(E(_kT)[1]);}),new T(function(){return [1,_kQ-1|0,E(_kS),E(E(E(_kT)[2]))];})];}else{var _kV=_kS[1],_kW=function(_kX){if(_kL>=_kV){var _kY=new T(function(){var _kZ=B(_kK(_kL-_kV|0,_kR));return [0,_kZ[1],_kZ[2]];});return [0,new T(function(){return E(E(_kY)[1]);}),new T(function(){return [1,_kQ-1|0,E(_kS),E(E(E(_kY)[2]))];})];}else{var _l0=new T(function(){var _l1=B(_kK(_kL,_kS));return [0,_l1[1],_l1[2]];});return [0,new T(function(){return E(E(_l0)[1]);}),new T(function(){return [1,_kQ-1|0,E(E(E(_l0)[2])),E(_kR)];})];}},_l2=E(_kR);if(!_l2[0]){return (_kL+1|0)!=_kQ?B(_kW(_)):[0,_l2[1],_kS];}else{return new F(function(){return _kW(_);});}}}};switch(E(_kL)){case 0:var _l3=E(_kM);if(!_l3[0]){return new F(function(){return _kN(_);});}else{var _l4=E(_l3[2]);return _l4[0]==0?[0,_l4[1],_l3[3]]:B(_kN(_));}break;case 1:var _l5=E(_kM);if(!_l5[0]){return new F(function(){return _kN(_);});}else{if(E(_l5[1])==2){var _l6=E(_l5[2]);if(!_l6[0]){var _l7=E(_l5[3]);return _l7[0]==0?[0,_l7[1],_l6]:B(_kN(_));}else{return new F(function(){return _kN(_);});}}else{return new F(function(){return _kN(_);});}}break;default:return new F(function(){return _kN(_);});}},_l8=new T(function(){return B(unCStr("[shuffle] called with lists of different lengths"));}),_l9=new T(function(){return B(err(_l8));}),_la=function(_lb,_lc){var _ld=function(_le){var _lf=E(_lc);if(!_lf[0]){return E(_l9);}else{var _lg=new T(function(){var _lh=B(_kK(E(_lf[1])[1],_lb));return [0,_lh[1],_lh[2]];});return [1,new T(function(){return E(E(_lg)[1]);}),new T(function(){return B(_la(E(_lg)[2],_lf[2]));})];}},_li=E(_lb);return _li[0]==0?E(_lc)[0]==0?[1,_li[1],_10]:B(_ld(_)):B(_ld(_));},_lj=function(_lk){var _ll=E(_lk);if(!_ll[0]){return [0];}else{var _lm=_ll[1],_ln=E(_ll[2]);if(!_ln[0]){return [1,_lm,_10];}else{var _lo=E(_ln[1]);return [1,new T(function(){var _lp=E(E(_lm));if(!_lp[0]){var _lq=E(_lo);if(!_lq[0]){var _lr=[1,2,E(_lp),E(_lq)];}else{var _lr=[1,_lq[1]+1|0,E(_lp),E(_lq)];}var _ls=_lr;}else{var _lt=_lp[1],_lu=E(_lo);if(!_lu[0]){var _lv=[1,_lt+1|0,E(_lp),E(_lu)];}else{var _lv=[1,_lt+_lu[1]|0,E(_lp),E(_lu)];}var _ls=_lv;}return _ls;}),new T(function(){return B(_lj(_ln[2]));})];}}},_lw=new T(function(){return B(_lj(_10));}),_lx=new T(function(){return B(_ly(_lw));}),_ly=function(_lz){while(1){var _lA=E(_lz);if(!_lA[0]){return E(_lx);}else{if(!E(_lA[2])[0]){return E(_lA[1]);}else{_lz=B(_lj(_lA));continue;}}}},_lB=function(_lC,_lD,_lE){var _lF=B(A(_lC,[_jw,function(_lG){var _lH=E(_lG),_lI=_lH[2];return [0,_lH[1],[1,_lD,new T(function(){return B(_la(B(_ly(B(_1C(_jr,_lI)))),B(_kC(B(_kv(_jW,B(_E(_lI,0))-1|0,new T(function(){return E(E(_lE)[5]);})))))[1]));})],_lH[3],_lH[4],_lH[5]];},_lE]));return [0,_lF[1],_lF[2],_lF[3],_lF[4],new T(function(){return E(B(_gJ(_eb,_jy,_jx,_lF[5]))[2]);})];},_lJ=function(_lK,_lL,_lM){return new F(function(){return A(_lK,[function(_lN){var _lO=E(_lM),_lP=_lO[1],_lQ=_lO[2];return [0,new T(function(){if(!E(_lQ)){var _lR=[0,E(_lP)[1],_lN];}else{var _lR=[0,_lN,E(_lP)[2]];}return _lR;}),_lQ,_lO[3],_lO[4],_lO[5]];},new T(function(){return B(A(_lL,[new T(function(){var _lS=E(_lM),_lT=_lS[1];if(!E(_lS[2])){var _lU=E(E(_lT)[2]);}else{var _lU=E(E(_lT)[1]);}var _lV=_lU;return _lV;})]));})]);});},_lW=function(_lX,_lY,_lZ){return new F(function(){return _lJ(E(_lX)[1],_lY,_lZ);});},_m0=function(_m1,_m2,_m3){var _m4=function(_m5){if(_m1<=_m5){var _m6=B(_lB(_lW,new T(function(){return B(_2H(_m2,_9,_3m,new T(function(){var _m7=E(_m3),_m8=_m7[1];if(!E(_m7[2])){var _m9=E(E(E(_m8)[2])[1]);}else{var _m9=E(E(E(_m8)[1])[1]);}var _ma=_m9;return _ma;})));}),_m3)),_mb=_m6[1],_mc=_m6[2];return [0,new T(function(){if(!E(_mc)){var _md=E(_mb),_me=[0,_md[1],new T(function(){var _mf=E(_md[2]);return [0,new T(function(){return B(_cq(_m2,_mf[1]));}),_mf[2],_mf[3],_mf[4],_mf[5]];})];}else{var _mg=E(_mb),_me=[0,new T(function(){var _mh=E(_mg[1]);return [0,new T(function(){return B(_cq(_m2,_mh[1]));}),_mh[2],_mh[3],_mh[4],_mh[5]];}),_mg[2]];}return _me;}),_mc,_bI,_m6[4],_m6[5]];}else{var _mi=B(_lB(_lW,new T(function(){return B(_2H(_m2,_9,_3m,new T(function(){var _mj=E(_m3),_mk=_mj[1];if(!E(_mj[2])){var _ml=E(E(E(_mk)[2])[1]);}else{var _ml=E(E(E(_mk)[1])[1]);}var _mm=_ml;return _mm;})));}),new T(function(){var _mn=E(_m3),_mo=_mn[1],_mp=_mn[2];return [0,_mo,_mp,[4,new T(function(){if(E(B(_2H(_m2,_9,_3m,new T(function(){if(!E(_mp)){var _mq=E(E(E(_mo)[2])[1]);}else{var _mq=E(E(E(_mo)[1])[1]);}return _mq;})))[1])==1){var _mr=[0,_m1-2|0];}else{var _mr=[0,_m1-1|0];}var _ms=_mr;return _ms;})],_mn[4],_mn[5]];}))),_mt=_mi[1],_mu=_mi[2];return [0,new T(function(){if(!E(_mu)){var _mv=E(_mt),_mw=[0,_mv[1],new T(function(){var _mx=E(_mv[2]);return [0,new T(function(){return B(_cq(_m2,_mx[1]));}),_mx[2],_mx[3],_mx[4],_mx[5]];})];}else{var _my=E(_mt),_mw=[0,new T(function(){var _mz=E(_my[1]);return [0,new T(function(){return B(_cq(_m2,_mz[1]));}),_mz[2],_mz[3],_mz[4],_mz[5]];}),_my[2]];}return _mw;}),_mu,_mi[3],_mi[4],_mi[5]];}};return E(B(_2H(_m2,_9,_3m,new T(function(){var _mA=E(_m3),_mB=_mA[1];if(!E(_mA[2])){var _mC=E(E(E(_mB)[2])[1]);}else{var _mC=E(E(E(_mB)[1])[1]);}var _mD=_mC;return _mD;})))[1])==1?B(_m4(2)):B(_m4(1));},_mE=[3],_mF=function(_){return _57;},_mG=function(_mH,_mI,_mJ,_){var _mK=function(_){return new F(function(){return _mG(_mH,_,_mJ,_);});},_mL=rMV(_mJ),_mM=_mL,_mN=E(_mM);switch(E(_mN[3])[0]){case 3:var _mO=rMV(_mJ),_mP=_mO,_=wMV(_mJ,new T(function(){var _mQ=E(_mP);return [0,_mQ[1],_mQ[2],_bI,_mQ[4],_mQ[5]];})),_mR=rMV(_mJ),_mS=_mR,_mT=B(_bb(_)),_mU=_mT,_mV=B(A(_7q,[_mS,_mU,_])),_mW=_mV,_mX=jsSetTimeout(1000,_mK);return _57;case 6:return new F(function(){return _be(_mH,[0,_mJ],_mF,_);});break;default:var _mY=rMV(_mJ),_mZ=_mY,_=wMV(_mJ,new T(function(){var _n0=B(_j9(_mN)),_n1=_n0[2],_n2=E(_mZ),_n3=_n2[1],_n4=_n2[2],_n5=_n2[3],_n6=_n2[4],_n7=_n2[5],_n8=E(_n0[1]);switch(_n8[0]){case 0:var _n9=_n8[1],_na=E(_n5);switch(_na[0]){case 1:var _nb=E(_n3),_nc=B(_jg(_n9,_nb[1],_nb[2],_n4,_n6,_n7));if(!_nc[0]){var _nd=[0,_nb,_n4,_aA,_n6,_n1];}else{var _ne=E(_nc[1]),_nd=[0,_ne[1],_ne[2],_ne[3],_ne[4],_n1];}var _nf=_nd,_ng=_nf;break;case 4:var _nh=B(_m0(E(_na[1])[1],_n9,_n2)),_ng=[0,_nh[1],_nh[2],_nh[3],_nh[4],_n1];break;case 5:var _ni=E(_n9);if(_ni[1]!=E(_na[1])[1]){var _nj=E(_n3),_nk=B(_jg(_ni,_nj[1],_nj[2],_n4,_n6,_n7));if(!_nk[0]){var _nl=[0,_nj,_n4,_na,_n6,_n1];}else{var _nm=E(_nk[1]),_nl=[0,_nm[1],_nm[2],_nm[3],_nm[4],_n1];}var _nn=_nl,_no=_nn;}else{var _no=[0,_n3,_n4,_aA,_n6,_n1];}var _np=_no,_nq=_np,_nr=_nq,_ng=_nr;break;default:var _ng=[0,_n3,_n4,_na,_n6,_n1];}var _ns=_ng;break;case 1:var _nt=E(_n8[1]),_nu=B(_di(_nt[1],_nt[2],_n3,_n4,_n5,_n6,_n7)),_ns=[0,_nu[1],_nu[2],_nu[3],_nu[4],_n1];break;default:var _ns=[0,_n3,_n4,_mE,_n6,_n1];}var _nv=_ns,_nw=_nv;return _nw;})),_nx=rMV(_mJ),_ny=_nx,_nz=B(_bb(_)),_nA=_nz,_nB=B(A(_7q,[_ny,_nA,_])),_nC=_nB,_nD=jsSetTimeout(1000,_mK);return _57;}},_nE=function(_nF){return new F(function(){return _5i(E(_nF)[1]);});},_nG=function(_nH){return [0,[0,_nH],new T(function(){var _nI=E(_nH);if(_nI==13){var _nJ=[0];}else{var _nK=B(_nG(_nI+1|0)),_nJ=[1,_nK[1],_nK[2]];}return _nJ;})];},_nL=new T(function(){var _nM=B(_nG(1));return [1,_nM[1],_nM[2]];}),_nN=function(_nO){return _nO>1?[0,_nL,new T(function(){var _nP=B(_nN(_nO-1|0));return [1,_nP[1],_nP[2]];})]:[0,_nL,_10];},_nQ=new T(function(){var _nR=B(_nN(2));return B(_6r([1,_nR[1],_nR[2]]));}),_nS=new T(function(){return [0,B(_E(_nQ,0))];}),_nT=function(_nU,_nV,_nW){return new F(function(){return _kv(_nU,E(_nV)[1],_nW);});},_nX=function(_nY,_nZ,_o0){return function(_o1){return new F(function(){return _la(new T(function(){return B(_ly(B(_1C(_jr,_nZ))));}),B(_kC(B(_nT(_nY,new T(function(){return [0,E(_o0)[1]-1|0];}),_o1))))[1]);});};},_o2=new T(function(){return B(_nX(_jW,_nQ,_nS));}),_o3=function(_o4){return _o4>1?[0,_27,new T(function(){var _o5=B(_o3(_o4-1|0));return [1,_o5[1],_o5[2]];})]:[0,_27,_10];},_o6=new T(function(){var _o7=B(_o3(3));return [1,_o7[1],_o7[2]];}),_o8=function(_o9){return _o9>1?[0,_o6,new T(function(){var _oa=B(_o8(_o9-1|0));return [1,_oa[1],_oa[2]];})]:[0,_o6,_10];},_ob=new T(function(){var _oc=B(_o8(5));return [1,_oc[1],_oc[2]];}),_od=[0,63],_oe=[1,_od,_10],_of=function(_og){return E(_oe);},_oh=new T(function(){return B(unCStr("computers"));}),_oi=new T(function(){return B(unCStr("\u30b3\u30f3\u30d4\u30e5\u30fc\u30bf"));}),_oj=new T(function(){return B(unCStr("yours"));}),_ok=new T(function(){return B(unCStr("\u3042\u306a\u305f"));}),_ol=function(_om,_on){var _oo=E(_om);if(!_oo){return [0];}else{var _op=E(_on);return _op[0]==0?[0]:[1,_op[1],new T(function(){return B(_ol(_oo-1|0,_op[2]));})];}},_oq=function(_or,_os,_ot){var _ou=new T(function(){return B(A(_o2,[_os]));}),_ov=new T(function(){return B(A(_o2,[_or]));});return [0,[0,[0,new T(function(){return B(_ol(3,_ov));}),new T(function(){return B(_2C(3,_ov));}),_ok,_oj,_nE],[0,new T(function(){return B(_ol(3,_ou));}),new T(function(){return B(_2C(3,_ou));}),_oi,_oh,_of]],_59,_as,_ob,_ot];},_ow=[0,0],_ox=0,_oy=new T(function(){return B(_fw(_ox));}),_oz=new T(function(){return die(_oy);}),_oA=function(_oB,_oC){var _oD=E(_oC);if(!_oD){return E(_fz);}else{var _oE=function(_oF){if(_oB<=0){if(_oB>=0){var _oG=quotRemI(_oB,_oD);return [0,[0,_oG[1]],[0,_oG[2]]];}else{if(_oD<=0){var _oH=quotRemI(_oB,_oD);return [0,[0,_oH[1]],[0,_oH[2]]];}else{var _oI=quotRemI(_oB+1|0,_oD);return [0,[0,_oI[1]-1|0],[0,(_oI[2]+_oD|0)-1|0]];}}}else{if(_oD>=0){if(_oB>=0){var _oJ=quotRemI(_oB,_oD);return [0,[0,_oJ[1]],[0,_oJ[2]]];}else{if(_oD<=0){var _oK=quotRemI(_oB,_oD);return [0,[0,_oK[1]],[0,_oK[2]]];}else{var _oL=quotRemI(_oB+1|0,_oD);return [0,[0,_oL[1]-1|0],[0,(_oL[2]+_oD|0)-1|0]];}}}else{var _oM=quotRemI(_oB-1|0,_oD);return [0,[0,_oM[1]-1|0],[0,(_oM[2]+_oD|0)+1|0]];}}};return E(_oD)==(-1)?E(_oB)==(-2147483648)?[0,_oz,_ow]:B(_oE(_)):B(_oE(_));}},_oN=function(_oO){var _oP=B(_oA((_oO>>>0&2147483647>>>0)>>>0&4.294967295e9,2147483562));return [0,E(_oP[2])[1]+1|0,B(_gb(E(_oP[1])[1],2147483398))+1|0];},_oQ=function(_){var _oR=B(A(_16,["(function(){ return Math.floor(Math.random() * Math.pow(2, 53)); })",_])),_oS=_oR;return new T(function(){var _oT=jsTrunc(_oS),_oU=_oT,_oV=B(_oN(_oU));return [0,_oV[1],_oV[2]];});},_oW=new T(function(){return [0,"click"];}),_oX=[8,_],_oY=function(_oZ){var _p0=String(_oZ),_p1=_p0;return new F(function(){return fromJSStr(_p1);});},_p2=function(_p3,_p4){while(1){var _p5=E(_p3);if(!_p5[0]){return E(_p4)[0]==0?true:false;}else{var _p6=E(_p4);if(!_p6[0]){return false;}else{if(E(_p5[1])[1]!=E(_p6[1])[1]){return false;}else{_p3=_p5[2];_p4=_p6[2];continue;}}}}},_p7=new T(function(){return B(unCStr("LI"));}),_p8=new T(function(){return B(_16("(function(e){ return e.tagName })"));}),_p9=new T(function(){return B(unCStr("wheel"));}),_pa=new T(function(){return B(unCStr("mouseout"));}),_pb=new T(function(){return B(unCStr("mouseover"));}),_pc=new T(function(){return B(unCStr("mousemove"));}),_pd=new T(function(){return B(unCStr("blur"));}),_pe=new T(function(){return B(unCStr("focus"));}),_pf=new T(function(){return B(unCStr("change"));}),_pg=new T(function(){return B(unCStr("unload"));}),_ph=new T(function(){return B(unCStr("load"));}),_pi=new T(function(){return B(unCStr("submit"));}),_pj=new T(function(){return B(unCStr("keydown"));}),_pk=new T(function(){return B(unCStr("keyup"));}),_pl=new T(function(){return B(unCStr("keypress"));}),_pm=new T(function(){return B(unCStr("mouseup"));}),_pn=new T(function(){return B(unCStr("mousedown"));}),_po=new T(function(){return B(unCStr("dblclick"));}),_pp=new T(function(){return B(unCStr("click"));}),_pq=function(_pr){switch(E(_pr)[0]){case 0:return E(_ph);case 1:return E(_pg);case 2:return E(_pf);case 3:return E(_pe);case 4:return E(_pd);case 5:return E(_pc);case 6:return E(_pb);case 7:return E(_pa);case 8:return E(_pp);case 9:return E(_po);case 10:return E(_pn);case 11:return E(_pm);case 12:return E(_pl);case 13:return E(_pk);case 14:return E(_pj);case 15:return E(_pi);default:return E(_p9);}},_ps=new T(function(){return E(0);}),_pt=new T(function(){return B(_16("(function(f) {  return (function() {      return (function(){        var args=Array.prototype.slice.call(arguments,0);        args.push(0);        return E(B(A(f, args)));    });  });})"));}),_pu=function(_pv,_){return new F(function(){return A(_pt,[E(_pv),_]);});},_pw=function(_px,_){return new F(function(){return _pu(_px,_);});},_py=new T(function(){return B(_16("(function(node, evtName, f){ node.addEventListener(evtName, (function(e){ f(e.target) }))})"));}),_pz=function(_pA,_pB){return function(_pC,_){var _pD=E(_pC),_pE=B(A(_py,[E(_pD[1]),E(toJSStr(E(new T(function(){return B(_pq(_pA));})))),E(new T(function(){return B(_12(function(_){var _=0;return new F(function(){return _pw(function(_pF){return new F(function(){return _12(function(_){var _=0,_pG=B(A(_pB,[[0,_pF],_])),_pH=_pG;return E(_ps);});});},_);});}));})),_])),_pI=_pE;return _pD;};},_pJ=function(_pK){return new F(function(){return _pz(_oX,function(_pL,_){var _pM=E(_pL)[1],_pN=B(A(_p8,[E(_pM),_])),_pO=_pN;if(!B(_p2(B(_oY(_pO)),_p7))){return _57;}else{var _pP=B(_2t(_6N,_pM,_)),_pQ=_pP;return new F(function(){return A(_pK,[_pQ,_]);});}});});},_pR=new T(function(){return B(unCStr("TD"));}),_pS=function(_pT){return new F(function(){return _pz(_oX,function(_pU,_){var _pV=E(_pU)[1],_pW=E(_pV),_pX=B(A(_p8,[_pW,_])),_pY=_pX;if(!B(_p2(B(_oY(_pY)),_pR))){return _57;}else{var _pZ=B(A(_6u,[_pW,_])),_q0=_pZ,_q1=B(_2t(_6N,_q0,_)),_q2=_q1,_q3=B(_2t(_6N,_pV,_)),_q4=_q3;return new F(function(){return A(_pT,[_q2,_q4,_]);});}});});},_q5=new T(function(){return B(unCStr("#field"));}),_q6=new T(function(){return B(unCStr("#yours ol.hand"));}),_q7=new T(function(){return B(unCStr("button#pass"));}),_q8=function(_q9,_qa,_qb){var _qc=E(_qb);return new F(function(){return _di(_q9,_qa,_qc[1],_qc[2],_qc[3],_qc[4],_qc[5]);});},_qd=function(_qe,_qf,_qg,_qh,_qi,_qj){var _qk=E(_qh);switch(_qk[0]){case 1:var _ql=E(_qf),_qm=B(_jg(_qe,_ql[1],_ql[2],_qg,_qi,_qj));return _qm[0]==0?[0,_ql,_qg,_aA,_qi,_qj]:E(_qm[1]);case 4:var _qn=B(_m0(E(_qk[1])[1],_qe,[0,_qf,_qg,_qk,_qi,_qj]));return [0,_qn[1],_qn[2],_qn[3],_qn[4],_qn[5]];case 5:var _qo=E(_qe);if(_qo[1]!=E(_qk[1])[1]){var _qp=E(_qf),_qq=B(_jg(_qo,_qp[1],_qp[2],_qg,_qi,_qj));return _qq[0]==0?[0,_qp,_qg,_qk,_qi,_qj]:E(_qq[1]);}else{return [0,_qf,_qg,_aA,_qi,_qj];}break;default:return [0,_qf,_qg,_qk,_qi,_qj];}},_qr=function(_qs,_qt){var _qu=E(_qt);return new F(function(){return _qd(_qs,_qu[1],_qu[2],_qu[3],_qu[4],_qu[5]);});},_qv=function(_){var _qw=B(_oQ(_)),_qx=_qw,_qy=B(_oQ(_)),_qz=_qy,_qA=B(_oQ(_)),_qB=_qA,_qC=new T(function(){var _qD=B(_oq(_qx,_qz,_qB));return [0,_qD[1],_qD[2],_qD[3],_qD[4],_qD[5]];}),_qE=nMV(_qC),_qF=_qE,_qG=nMV(_58),_qH=_qG,_qI=function(_){return new F(function(){return _mG(_qH,_,_qF,_);});},_qJ=B(_bb(_)),_qK=_qJ,_qL=[0,_qF],_qM=B(_1c(_q7,function(_qN,_){var _qO=E(_qN),_qP=jsSetCB(_qO[1],E(_oW)[1],function(_qQ,_qR,_){var _qS=rMV(_qF),_qT=_qS,_qU=rMV(_qH),_qV=_qU;return new F(function(){return _be(_qH,_qL,_qI,_);});}),_qW=_qP;return _qO;},_qK,_)),_qX=_qM,_qY=B(_1c(_q6,new T(function(){return B(_pJ(function(_qZ,_){var _r0=rMV(_qF),_r1=_r0,_r2=E(_r1);if(!E(_r2[2])){return _57;}else{var _r3=rMV(_qF),_r4=_r3,_=wMV(_qF,new T(function(){return B(_qr(_qZ,_r4));})),_r5=rMV(_qF),_r6=_r5,_r7=B(_bb(_)),_r8=_r7,_r9=B(A(_7q,[_r6,_r8,_])),_ra=_r9;if(E(_r2[3])[0]==6){var _rb=B(_be(_qH,_qL,_qI,_)),_rc=_rb;return _57;}else{return _57;}}}));}),_qK,_)),_rd=_qY,_re=B(_1c(_q5,new T(function(){return B(_pS(function(_rf,_rg,_){var _rh=rMV(_qF),_ri=_rh,_=wMV(_qF,new T(function(){return B(_q8(_rf,_rg,_ri));})),_rj=rMV(_qF),_rk=_rj,_rl=B(_bb(_)),_rm=_rl,_rn=B(A(_7q,[_rk,_rm,_])),_ro=_rn,_rp=rMV(_qF),_rq=_rp;if(E(E(_rq)[3])[0]==6){var _rr=B(_be(_qH,_qL,_qI,_)),_rs=_rr;return _57;}else{return _57;}}));}),_qK,_)),_rt=_re,_ru=B(A(_7q,[_qC,_qK,_])),_rv=_ru,_rw=jsSetTimeout(1000,function(_){var _rx=rMV(_qF),_ry=_rx,_=wMV(_qF,new T(function(){return B(_aY(_ry));})),_rz=rMV(_qF),_rA=_rz,_rB=B(_bb(_)),_rC=_rB,_rD=B(A(_7q,[_rA,_rC,_])),_rE=_rD;return _57;});return _57;},_rF=function(_){return new F(function(){return _qv(_);});};
var hasteMain = function() {B(A(_rF, [0]));};window.onload = hasteMain;