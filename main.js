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

var _0=new T(function(){return B(unCStr("Prelude.(!!): negative index\n"));}),_1=new T(function(){return B(err(_0));}),_2=new T(function(){return B(unCStr("Prelude.(!!): index too large\n"));}),_3=new T(function(){return B(err(_2));}),_4=function(_5,_6){while(1){var _7=E(_5);if(!_7[0]){return E(_3);}else{var _8=E(_6);if(!_8){return E(_7[1]);}else{_5=_7[2];_6=_8-1|0;continue;}}}},_9=function(_a,_b){return E(_b);},_c=function(_d,_e,_){var _f=jsCreateTextNode(toJSStr(E(_d))),_g=_f,_h=jsAppendChild(_g,E(_e)[1]);return [0,_g];},_i=function(_j,_k,_){var _l=E(_j);if(!_l[0]){return _k;}else{var _m=B(A(_l[1],[_k,_])),_n=_m,_o=B(_i(_l[2],_k,_)),_p=_o;return _k;}},_q=new T(function(){return B(unCStr("\u884c\u52d5\u3092\u9078\u629e"));}),_r=new T(function(){return B(unCStr("\u30c9\u30ed\u30fc"));}),_s=new T(function(){return B(unCStr("\u3042\u306a\u305f\u306e\u52dd\u3061\u3067\u3059!"));}),_t=new T(function(){return B(unCStr("\u30b3\u30f3\u30d4\u30e5\u30fc\u30bf\u304c\u52dd\u3061!"));}),_u=new T(function(){return B(unCStr("\u624b\u756a\u3092\u4ea4\u4ee3"));}),_v=new T(function(){return B(unCStr("\u53ec\u559a\u3059\u308b\u4f4d\u7f6e\u3092\u9078\u629e"));}),_w=new T(function(){return B(unCStr("\u751f\u8d04\u3092\u9078\u629e: \u30a8\u30cd\u30eb\u30ae\u30fc\u304c\u3042\u3068"));}),_x=new T(function(){return B(unCStr("\u5fc5\u8981"));}),_y=new T(function(){return B(unCStr("\u30d1\u30b9\u3057\u307e\u3059"));}),_z=new T(function(){return B(unCStr("\u79fb\u52d5\u3059\u308b\u4f4d\u7f6e\u3092\u9078\u629e"));}),_A=[0,0],_B=new T(function(){return B(unCStr(" .deck"));}),_C=new T(function(){return B(unCStr("\u306e\u6b8b\u308a\u5c71\u672d: "));}),_D=[0,35],_E=new T(function(){return B(unCStr(" .hand"));}),_F=function(_G,_H){while(1){var _I=E(_G);if(!_I[0]){return E(_H);}else{_G=_I[2];var _J=_H+1|0;_H=_J;continue;}}},_K=function(_L,_M){var _N=E(_L);return _N[0]==0?E(_M):[1,_N[1],new T(function(){return B(_K(_N[2],_M));})];},_O=function(_P,_Q){var _R=jsShowI(_P),_S=_R;return new F(function(){return _K(fromJSStr(_S),_Q);});},_T=[0,41],_U=[0,40],_V=function(_W,_X,_Y){if(_X>=0){return new F(function(){return _O(_X,_Y);});}else{return _W<=6?B(_O(_X,_Y)):[1,_U,new T(function(){var _Z=jsShowI(_X),_10=_Z;return B(_K(fromJSStr(_10),[1,_T,_Y]));})];}},_11=[0],_12=new T(function(){return [0,"arr2lst"];}),_13=function(_14){var _15=B(A(_14,[_])),_16=_15;return E(_16);},_17=function(_18){return new F(function(){return _13(function(_){var _=0;return new F(function(){return eval(_18);});});});},_19=function(_1a,_1b){return new F(function(){return _13(function(_){var _=0;return new F(function(){return A(_17,[E(_12)[1],E(_1a),E(_1b),_]);});});});},_1c=new T(function(){return B(_17("(function(sel){return document.querySelectorAll(sel);})"));}),_1d=function(_1e,_1f,_1g,_){var _1h=B(A(_1c,[E(toJSStr(E(_1e))),_])),_1i=_1h,_1j=function(_1k,_){var _1l=E(_1k);if(!_1l[0]){return _11;}else{var _1m=B(A(_1f,[[0,_1l[1]],_])),_1n=_1m,_1o=B(_1j(_1l[2],_)),_1p=_1o;return [1,_1n,_1p];}},_1q=B(_1j(B(_19(_1i,0)),_)),_1r=_1q;return _1g;},_1s=new T(function(){return B(unCStr("li"));}),_1t=function(_1u,_1v,_1w,_){var _1x=jsCreateElem(toJSStr(E(_1s))),_1y=_1x,_1z=jsAppendChild(_1y,E(_1w)[1]),_1A=[0,_1y],_1B=B(A(_1u,[_1v,_1A,_])),_1C=_1B;return _1A;},_1D=function(_1E,_1F){var _1G=E(_1F);return _1G[0]==0?[0]:[1,new T(function(){return B(A(_1E,[_1G[1]]));}),new T(function(){return B(_1D(_1E,_1G[2]));})];},_1H=function(_1I){return function(_1J,_){var _1K=B(_1d([1,_D,new T(function(){return B(_K(E(_1I)[4],_B));})],function(_1L,_){var _1M=E(_1L),_1N=jsClearChildren(_1M[1]),_1O=B(_c(new T(function(){var _1P=E(_1I);return B(_K(_1P[3],new T(function(){return B(_K(_C,new T(function(){return B(_V(0,B(_F(_1P[2],0)),_11));},1)));},1)));}),_1M,_)),_1Q=_1O;return _1M;},_1J,_)),_1R=_1K,_1S=B(_1d([1,_D,new T(function(){return B(_K(E(_1I)[4],_E));})],function(_1T,_){var _1U=E(_1T),_1V=jsClearChildren(_1U[1]),_1W=B(_i(new T(function(){var _1X=E(_1I);return B(_1D(function(_1Y){return function(_1Z,_20){return new F(function(){return _1t(_c,new T(function(){return B(A(_1X[5],[_1Y]));}),_1Z,_20);});};},_1X[1]));}),_1U,_)),_21=_1W;return _1U;},_1J,_)),_22=_1S;return _1J;};},_23=new T(function(){return B(unCStr("Prelude.Enum.succ{Int}: tried to take `succ\' of maxBound"));}),_24=new T(function(){return B(err(_23));}),_25=function(_26){var _27=E(E(_26)[1]);return _27==2147483647?E(_24):[0,_27+1|0];},_28=[0],_29=new T(function(){return [0,"(function(e){ return e.previousSibling })"];}),_2a=new T(function(){return B(_17("(function(x) {return x === null})"));}),_2b=new T(function(){return B(_17("(function(node) {return node.nodeType === 1})"));}),_2c=function(_2d,_){var _2e=E(_29)[1],_2f=B(A(_17,[_2e,E(_2d),_])),_2g=_2f,_2h=E(_2g),_2i=B(A(_2a,[_2h,_])),_2j=_2i;if(_2j<=0){var _2k=B(A(_2b,[_2h,_])),_2l=_2k;if(_2l<=0){return new F(function(){return (function(_2m,_){while(1){var _2n=B(A(_17,[_2e,E(_2m),_])),_2o=_2n,_2p=E(_2o),_2q=B(A(_2a,[_2p,_])),_2r=_2q;if(_2r<=0){var _2s=B(A(_2b,[_2p,_])),_2t=_2s;if(_2t<=0){_2m=_2o;continue;}else{return [1,[0,_2p]];}}else{return _28;}}})(_2g,_);});}else{return [1,[0,_2h]];}}else{return _28;}},_2u=function(_2v,_2w,_){while(1){var _2x=(function(_2y,_2z,_){var _2A=B(_2c(_2z,_)),_2B=_2A,_2C=E(_2B);if(!_2C[0]){return _2y;}else{_2v=new T(function(){return B(_25(_2y));});_2w=E(_2C[1])[1];return null;}})(_2v,_2w,_);if(_2x!=null){return _2x;}}},_2D=function(_2E,_2F){while(1){var _2G=E(_2E);if(!_2G){return E(_2F);}else{var _2H=E(_2F);if(!_2H[0]){return [0];}else{_2E=_2G-1|0;_2F=_2H[2];continue;}}}},_2I=function(_2J,_2K,_2L,_2M){return new F(function(){return A(_2K,[function(_2N){var _2O=E(_2J)[1],_2P=[1,_2N,new T(function(){var _2Q=E(_2J)[1]+1|0;return _2Q>=0?B(_2D(_2Q,_2M)):E(_2M);})];if(_2O>0){var _2R=function(_2S,_2T){var _2U=E(_2S);if(!_2U[0]){return E(_2P);}else{var _2V=_2U[1];return _2T>1?[1,_2V,new T(function(){return B(_2R(_2U[2],_2T-1|0));})]:[1,_2V,_2P];}};return new F(function(){return _2R(_2M,_2O);});}else{return E(_2P);}},new T(function(){return B(A(_2L,[new T(function(){var _2W=E(_2J)[1];return _2W>=0?B(_4(_2M,_2W)):E(_1);})]));})]);});},_2X=[0,0],_2Y=[0,-1],_2Z=[0,_2X,_2Y],_30=[1,_2Z,_11],_31=[0,_2Y,_2X],_32=[1,_31,_30],_33=[0,1],_34=[0,_2X,_33],_35=[1,_34,_32],_36=[0,-2],_37=[0,_36,_33],_38=[1,_37,_11],_39=[0,_36,_2Y],_3a=[1,_39,_38],_3b=function(_3c,_3d){return [0,E(_3c)[1]+E(_3d)[1]|0];},_3e=function(_3f,_3g){var _3h=E(_3f),_3i=E(_3g);return [0,new T(function(){return B(_3b(_3h[1],_3i[1]));}),new T(function(){return B(_3b(_3h[2],_3i[2]));})];},_3j=[1,_31,_11],_3k=function(_3l,_3m){var _3n=E(_3l),_3o=E(_3m);return [0,new T(function(){return [0, -E(_3n[1])[1]+E(_3o[1])[1]|0];}),new T(function(){return B(_3b(_3n[2],_3o[2]));})];},_3p=[0,_33,_2X],_3q=[1,_3p,_11],_3r=[0,_2X,_2X],_3s=[1,_3r,_3q],_3t=[1,_31,_3s],_3u=function(_3v,_3w){switch(E(_3w)){case 2:return !E(_3v)?B(_1D(_3k,_3a)):B(_1D(_3e,_3a));case 12:return !E(_3v)?B(_1D(_3k,_3t)):B(_1D(_3e,_3t));case 13:return !E(_3v)?B(_1D(_3k,_35)):B(_1D(_3e,_35));default:return !E(_3v)?B(_1D(_3k,_3j)):B(_1D(_3e,_3j));}},_3x=function(_3y){return E(_3y);},_3z=new T(function(){return B(unCStr(": empty list"));}),_3A=new T(function(){return B(unCStr("Prelude."));}),_3B=function(_3C){return new F(function(){return err(B(_K(_3A,new T(function(){return B(_K(_3C,_3z));},1))));});},_3D=new T(function(){return B(unCStr("head"));}),_3E=new T(function(){return B(_3B(_3D));}),_3F=function(_3G){return E(E(_3G)[1]);},_3H=function(_3I,_3J,_3K){while(1){var _3L=E(_3K);if(!_3L[0]){return false;}else{if(!B(A(_3F,[_3I,_3J,_3L[1]]))){_3K=_3L[2];continue;}else{return true;}}}},_3M=function(_3N,_3O,_3P,_3Q,_3R,_3S){return !B(A(_3N,[_3P,_3R]))?true:!B(A(_3F,[_3O,_3Q,_3S]))?true:false;},_3T=function(_3U,_3V,_3W,_3X){var _3Y=E(_3W),_3Z=E(_3X);return new F(function(){return _3M(E(_3U)[1],_3V,_3Y[1],_3Y[2],_3Z[1],_3Z[2]);});},_40=function(_41,_42,_43,_44,_45,_46){return !B(A(_41,[_43,_45]))?false:B(A(_3F,[_42,_44,_46]));},_47=function(_48,_49,_4a,_4b){var _4c=E(_4a),_4d=E(_4b);return new F(function(){return _40(E(_48)[1],_49,_4c[1],_4c[2],_4d[1],_4d[2]);});},_4e=function(_4f,_4g){return [0,function(_4h,_4i){return new F(function(){return _47(_4f,_4g,_4h,_4i);});},function(_4h,_4i){return new F(function(){return _3T(_4f,_4g,_4h,_4i);});}];},_4j=function(_4k,_4l){return E(_4k)[1]==E(_4l)[1];},_4m=function(_4n,_4o){return E(_4n)[1]!=E(_4o)[1];},_4p=[0,_4j,_4m],_4q=new T(function(){return B(_4e(_4p,_4p));}),_4r=function(_4s,_4t,_4u,_4v,_4w){var _4x=B(_2I(_4t,_9,function(_4y){return new F(function(){return _2I(_4u,_9,_3x,_4y);});},new T(function(){return E(E(_4s)[4]);})));if(!_4x[0]){return false;}else{var _4z=E(_4v),_4A=_4z[1];if(_4A<0){return false;}else{var _4B=E(_4w),_4C=_4B[1];if(_4C<0){return false;}else{var _4D=E(_4s),_4E=_4D[4];if(_4A>=B(_F(_4E,0))){return false;}else{var _4F=E(_4E);if(!_4F[0]){return E(_3E);}else{if(_4C>=B(_F(_4F[1],0))){return false;}else{var _4G=E(_4x[1]),_4H=_4G[1],_4I=E(_4G[2])[1];if(!B(_3H(_4q,[0,_4z,_4B],B(_1D(function(_4J){return new F(function(){return A(_4J,[[0,_4t,_4u]]);});},B(_3u(_4D[2],_4I))))))){return false;}else{var _4K=B(_2I(_4z,_9,function(_4L){return new F(function(){return _2I(_4B,_9,_3x,_4L);});},_4F));if(!_4K[0]){return true;}else{var _4M=E(_4K[1]);return _4I<=E(_4M[2])[1]?false:!E(_4M[1])?E(_4H):!E(_4H)?true:false;}}}}}}}}},_4N=function(_4O){return new F(function(){return err(B(unAppCStr("Oops!  Entered absent arg ",new T(function(){return B(unCStr(_4O));}))));});},_4P=new T(function(){return B(_4N("ww_shEm{v} [lid] random-1.1:System.Random.StdGen{tc ru}"));}),_4Q=new T(function(){return B(_4N("ww_shEk{v} [lid] main:Game.BoardTrump.Core.Phase{tc r1Tq}"));}),_4R=new T(function(){return B(_4N("ww_shEi{v} [lid] (main:Game.BoardTrump.Types.Player{tc r1v},\n                  main:Game.BoardTrump.Types.Player{tc r1v})"));}),_4S=function(_4T,_4U,_4V,_4W,_4X){var _4Y=function(_4Z){while(1){var _50=(function(_51){var _52=E(_51);if(!_52[0]){return [0];}else{var _53=_52[2],_54=new T(function(){return B(A(_52[1],[[0,_4W,_4X]]));});if(!B(_4r([0,_4R,_4U,_4Q,_4V,_4P],_4W,_4X,new T(function(){return E(E(_54)[1]);},1),new T(function(){return E(E(_54)[2]);},1)))){_4Z=_53;return null;}else{return [1,_54,new T(function(){return B(_4Y(_53));})];}}})(_4Z);if(_50!=null){return _50;}}};return new F(function(){return _4Y(B(_3u(_4U,_4T)));});},_55=function(_56,_57){while(1){var _58=E(_57);if(!_58[0]){return E(_56);}else{var _59=_56+E(_58[1])[1]|0;_57=_58[2];_56=_59;continue;}}},_5a=[0,2],_5b=function(_5c){return E(E(_5c)[1])==1?E(_5a):E(_33);},_5d=function(_5e,_5f){var _5g=function(_5h){return E(_5e)==1?_5h<=B(_55(-2,B(_1D(_5b,_5f)))):_5h<=B(_55(-1,B(_1D(_5b,_5f))));};return _5e<=10?E(_5e)==1?B(_5g(114514)):B(_5g(0)):B(_5g(2));},_5i=0,_5j=false,_5k=true,_5l=[0,75],_5m=[1,_5l,_11],_5n=[0,81],_5o=[1,_5n,_11],_5p=[0,74],_5q=[1,_5p,_11],_5r=[0,65],_5s=[1,_5r,_11],_5t=function(_5u){var _5v=E(_5u);switch(_5v){case 1:return E(_5s);case 11:return E(_5q);case 12:return E(_5o);case 13:return E(_5m);default:return new F(function(){return _V(0,_5v,_11);});}},_5w=function(_5x,_5y,_5z,_5A){return new F(function(){return A(_5x,[function(_){var _5B=jsSetAttr(E(_5y)[1],toJSStr(E(_5z)),toJSStr(E(_5A)));return _5i;}]);});},_5C=new T(function(){return B(unCStr("td"));}),_5D=function(_5E,_5F,_5G,_){var _5H=jsCreateElem(toJSStr(E(_5C))),_5I=_5H,_5J=jsAppendChild(_5I,E(_5G)[1]),_5K=[0,_5I],_5L=B(A(_5E,[_5F,_5K,_])),_5M=_5L;return _5K;},_5N=function(_5O,_){return new F(function(){return _5D(_c,_11,_5O,_);});},_5P=function(_5Q){return E(_5Q);},_5R=new T(function(){return B(unCStr("class"));}),_5S=new T(function(){return B(unCStr("computers-card"));}),_5T=new T(function(){return B(unCStr("your-card"));}),_5U=function(_5V){var _5W=E(_5V);if(!_5W[0]){return E(_5N);}else{var _5X=E(_5W[1]);return function(_5Y,_){var _5Z=B(_5D(_c,new T(function(){return B(_5t(E(_5X[2])[1]));}),_5Y,_)),_60=_5Z,_61=B(A(_5w,[_5P,_60,_5R,new T(function(){return !E(_5X[1])?E(_5S):E(_5T);}),_])),_62=_61;return _60;};}},_63=new T(function(){return B(unCStr("tr"));}),_64=function(_65,_66,_67,_){var _68=jsCreateElem(toJSStr(E(_63))),_69=_68,_6a=jsAppendChild(_69,E(_67)[1]),_6b=[0,_69],_6c=B(A(_65,[_66,_6b,_])),_6d=_6c;return _6b;},_6e=function(_6f){return E(_6f);},_6g=function(_6h){return function(_1Z,_20){return new F(function(){return _64(_6e,function(_6i,_){return new F(function(){return _i(new T(function(){return B(_1D(_5U,_6h));}),_6i,_);});},_1Z,_20);});};},_6j=function(_6k,_){return _5i;},_6l=new T(function(){return B(unCStr("selectable-hand"));}),_6m=new T(function(){return B(_17("(function(e,c,x){x?e.classList.add(c):e.classList.remove(c);})"));}),_6n=function(_){var _=0;return new F(function(){return A(_17,["false",_]);});},_6o=new T(function(){return B(_13(_6n));}),_6p=function(_){var _=0;return new F(function(){return A(_17,["true",_]);});},_6q=new T(function(){return B(_13(_6p));}),_6r=function(_6s){return function(_6t){return function(_6u,_){var _6v=B(A(new T(function(){return B(A(new T(function(){return B(A(_6m,[E(E(_6s)[1])]));}),[E(toJSStr(E(_6t)))]));}),[!E(_6u)?E(_6o):E(_6q),_])),_6w=_6v;return _5i;};};},_6x=function(_6y,_){while(1){var _6z=E(_6y);if(!_6z[0]){return _5i;}else{var _6A=B(A(_6r,[_6z[1],_6l,_5k,_])),_6B=_6A;_6y=_6z[2];continue;}}},_6C=function(_6D){var _6E=E(_6D);if(!_6E[0]){return [0];}else{return new F(function(){return _K(_6E[1],new T(function(){return B(_6C(_6E[2]));},1));});}},_6F=new T(function(){return B(_17("(function(e){return e.parentNode;})"));}),_6G=new T(function(){return B(unCStr("td"));}),_6H=function(_6I,_6J){while(1){var _6K=(function(_6L,_6M){var _6N=E(_6M);if(!_6N[0]){return [0];}else{var _6O=_6N[1],_6P=_6N[2];if(!B(A(_6L,[_6O]))){var _6Q=_6L;_6J=_6P;_6I=_6Q;return null;}else{return [1,_6O,new T(function(){return B(_6H(_6L,_6P));})];}}})(_6I,_6J);if(_6K!=null){return _6K;}}},_6R=function(_6S,_6T,_6U,_6V){var _6W=E(_6U);if(!_6W[0]){return E(_6T);}else{var _6X=E(_6V);if(!_6X[0]){return E(_6T);}else{return new F(function(){return A(_6S,[_6W[1],_6X[1],new T(function(){return B(_6R(_6S,_6T,_6W[2],_6X[2]));})]);});}}},_6Y=[0,0],_6Z=new T(function(){return B(unCStr("Maybe.fromJust: Nothing"));}),_70=new T(function(){return B(err(_6Z));}),_71=function(_72){return E(_72)[0]==0?true:false;},_73=function(_74,_75){while(1){var _76=E(_75);if(!_76[0]){return E(_74);}else{_74=_76[1];_75=_76[2];continue;}}},_77=new T(function(){return B(unCStr("last"));}),_78=new T(function(){return B(_3B(_77));}),_79=new T(function(){return B(unCStr("table#field"));}),_7a=new T(function(){return B(unCStr("movable-card"));}),_7b=new T(function(){return B(unCStr("id"));}),_7c=new T(function(){return B(unCStr("subject-of-moving"));}),_7d=new T(function(){return B(unCStr("motion-scope"));}),_7e=new T(function(){return B(unCStr("obj-of-summon"));}),_7f=new T(function(){return B(unCStr("\u306e\u756a\u3067\u3059\u3001"));}),_7g=new T(function(){return B(unCStr(" ol.hand li"));}),_7h=function(_7i,_7j,_7k,_){if(!E(_7j)){return new F(function(){return A(_7k,[_]);});}else{var _7l=B(A(_6r,[_7i,_6l,_5k,_])),_7m=_7l;return new F(function(){return A(_7k,[_]);});}},_7n=function(_){return _5i;},_7o=new T(function(){return B(unCStr("summonable-zone"));}),_7p=function(_7q,_7r,_7s,_){if(!E(_7r)){return new F(function(){return A(_7s,[_]);});}else{var _7t=B(A(_5w,[_5P,_7q,_5R,_7o,_])),_7u=_7t;return new F(function(){return A(_7s,[_]);});}},_7v=new T(function(){return B(unCStr("#status"));}),_7w=[0,35],_7x=new T(function(){return B(unCStr("#field tr"));}),_7y=function(_7z,_){return _7z;},_7A=function(_7B){return function(_1Z,_20){return new F(function(){return _i([1,function(_5O,_){return new F(function(){return _1d(_79,function(_7C,_){var _7D=E(_7C),_7E=jsClearChildren(_7D[1]),_7F=B(_i(new T(function(){return B(_1D(_6g,E(_7B)[4]));}),_7D,_)),_7G=_7F;return _7D;},_5O,_);});},[1,function(_5O,_){return new F(function(){return _1d(_7v,function(_7H,_){var _7I=E(_7H),_7J=jsClearChildren(_7I[1]),_7K=B(A(new T(function(){var _7L=E(_7B),_7M=_7L[1],_7N=E(_7L[3]);if(_7N[0]==7){var _7O=function(_1Z,_20){return new F(function(){return _c(new T(function(){return !E(_7N[1])?E(_t):E(_s);}),_1Z,_20);});};}else{var _7O=function(_1Z,_20){return new F(function(){return _c(new T(function(){return B(unAppCStr("-- ",new T(function(){var _7P=new T(function(){return B(_K(_7f,new T(function(){var _7Q=E(_7N);switch(_7Q[0]){case 0:var _7R=E(_r);break;case 1:var _7R=E(_q);break;case 2:var _7R=E(_z);break;case 3:var _7R=E(_y);break;case 4:var _7S=function(_7T){var _7U=E(_7T);return _7U[0]==0?E(new T(function(){return B(_K(B(_V(0,E(_7Q[1])[1],_11)),_x));})):[1,_7U[1],new T(function(){return B(_7S(_7U[2]));})];},_7R=B(_7S(_w));break;case 5:var _7R=E(_v);break;default:var _7R=E(_u);}return _7R;},1)));},1);if(!E(_7L[2])){var _7V=B(_K(E(E(_7M)[2])[3],_7P));}else{var _7V=B(_K(E(E(_7M)[1])[3],_7P));}return _7V;})));}),_1Z,_20);});};}var _7W=_7O;return _7W;}),[_7I,_])),_7X=_7K;return _7I;},_5O,_);});},[1,function(_7Y,_){var _7Z=E(new T(function(){var _80=E(E(_7B)[1]);return [0,new T(function(){return B(_1H(_80[1]));}),new T(function(){return B(_1H(_80[2]));})];})),_81=B(A(_7Z[1],[_7Y,_])),_82=_81,_83=B(A(_7Z[2],[_7Y,_])),_84=_83;return _7Y;},[1,new T(function(){var _85=E(_7B),_86=_85[1],_87=_85[2],_88=_85[4],_89=E(_85[3]);switch(_89[0]){case 1:var _8a=function(_8b){var _8c=E(_8b);if(!_8c[0]){return E(_6j);}else{var _8d=function(_8e){var _8f=E(_8e);if(!_8f[0]){return E(new T(function(){return B(_8a(_8c[2]));}));}else{var _8g=new T(function(){return B(_8d(_8f[2]));});return function(_8h,_){var _8i=E(_8h);if(!_8i[0]){return _5i;}else{var _8j=_8i[2],_8k=E(_8i[1]);if(!_8k[0]){return new F(function(){return A(_8g,[_8j,_]);});}else{var _8l=E(_8f[1]),_8m=_8l[1],_8n=B(A(_6F,[E(_8m),_])),_8o=_8n,_8p=B(_2u(_6Y,_8o,_)),_8q=_8p,_8r=B(_2u(_A,_8m,_)),_8s=_8r,_8t=E(_8k[1]),_8u=_8t[2];if(!E(_8t[1])){if(!E(_87)){if(!B(_4S(E(_8u)[1],_5j,_88,_8q,_8s))[0]){return new F(function(){return A(_8g,[_8j,_]);});}else{var _8v=B(A(_6r,[_8l,_7a,_5k,_])),_8w=_8v;return new F(function(){return A(_8g,[_8j,_]);});}}else{return new F(function(){return A(_8g,[_8j,_]);});}}else{if(!E(_87)){return new F(function(){return A(_8g,[_8j,_]);});}else{if(!B(_4S(E(_8u)[1],_5k,_88,_8q,_8s))[0]){return new F(function(){return A(_8g,[_8j,_]);});}else{var _8x=B(A(_6r,[_8l,_7a,_5k,_])),_8y=_8x;return new F(function(){return A(_8g,[_8j,_]);});}}}}}};}};return new F(function(){return _8d(_8c[1]);});}},_8z=function(_8A,_){var _8B=E(_8A),_8C=_8B[1],_8D=jsQuerySelectorAll(_8C,toJSStr([1,_7w,new T(function(){if(!E(_87)){var _8E=B(_K(E(E(_86)[2])[4],_7g));}else{var _8E=B(_K(E(E(_86)[1])[4],_7g));}return _8E;})])),_8F=_8D,_8G=B(A(_6R,[_7h,_7n,_8F,new T(function(){var _8H=function(_8I){return new F(function(){return (function(_8J){if(!E(_87)){return new F(function(){return _5d(_8J,E(E(_86)[2])[1]);});}else{return new F(function(){return _5d(_8J,E(E(_86)[1])[1]);});}})(E(_8I)[1]);});};if(!E(_87)){var _8K=B(_1D(_8H,E(E(_86)[2])[1]));}else{var _8K=B(_1D(_8H,E(E(_86)[1])[1]));}return _8K;}),_])),_8L=_8G,_8M=jsQuerySelectorAll(_8C,toJSStr(E(_7x))),_8N=_8M,_8O=E(_8N);if(!_8O[0]){return _8B;}else{var _8P=E(_6G),_8Q=jsQuerySelectorAll(E(_8O[1])[1],toJSStr(_8P)),_8R=_8Q,_8S=function(_8T,_){var _8U=E(_8T);if(!_8U[0]){return _11;}else{var _8V=jsQuerySelectorAll(E(_8U[1])[1],toJSStr(_8P)),_8W=_8V,_8X=B(_8S(_8U[2],_)),_8Y=_8X;return [1,_8W,_8Y];}},_8Z=B(_8S(_8O[2],_)),_90=_8Z,_91=B(A(function(_92,_93){var _94=function(_95){var _96=E(_95);if(!_96[0]){return E(new T(function(){return B(_8a(_93));}));}else{var _97=new T(function(){return B(_94(_96[2]));});return function(_98,_){var _99=E(_98);if(!_99[0]){return _5i;}else{var _9a=_99[2],_9b=E(_99[1]);if(!_9b[0]){return new F(function(){return A(_97,[_9a,_]);});}else{var _9c=E(_96[1]),_9d=_9c[1],_9e=B(A(_6F,[E(_9d),_])),_9f=_9e,_9g=B(_2u(_6Y,_9f,_)),_9h=_9g,_9i=B(_2u(_A,_9d,_)),_9j=_9i,_9k=E(_9b[1]),_9l=_9k[2];if(!E(_9k[1])){if(!E(_87)){if(!B(_4S(E(_9l)[1],_5j,_88,_9h,_9j))[0]){return new F(function(){return A(_97,[_9a,_]);});}else{var _9m=B(A(_6r,[_9c,_7a,_5k,_])),_9n=_9m;return new F(function(){return A(_97,[_9a,_]);});}}else{return new F(function(){return A(_97,[_9a,_]);});}}else{if(!E(_87)){return new F(function(){return A(_97,[_9a,_]);});}else{if(!B(_4S(E(_9l)[1],_5k,_88,_9h,_9j))[0]){return new F(function(){return A(_97,[_9a,_]);});}else{var _9o=B(A(_6r,[_9c,_7a,_5k,_])),_9p=_9o;return new F(function(){return A(_97,[_9a,_]);});}}}}}};}};return new F(function(){return _94(_92);});},[_8R,_90,new T(function(){return B(_6C(_88));}),_])),_9q=_91;return _8B;}};break;case 2:var _9r=_89[1],_8z=function(_9s,_){var _9t=E(_9s),_9u=jsQuerySelectorAll(_9t[1],toJSStr(E(_7x))),_9v=_9u,_9w=_9v,_9x=E(_9r),_9y=E(_9x[1])[1];if(_9y>=0){var _9z=E(_6G),_9A=jsQuerySelectorAll(B(_4(_9w,_9y))[1],toJSStr(_9z)),_9B=_9A,_9C=E(_9x[2])[1];if(_9C>=0){var _9D=jsSetAttr(B(_4(_9B,_9C))[1],toJSStr(E(_7b)),toJSStr(E(_7c))),_9E=function(_,_9F){var _9G=B((function(_9H,_){while(1){var _9I=(function(_9J,_){var _9K=E(_9J);if(!_9K[0]){return _5i;}else{var _9L=_9K[1],_9M=B(A(_6r,[new T(function(){return B(_2I(new T(function(){return E(B(A(_9L,[_9x]))[1]);}),_9,function(_9N){return new F(function(){return _2I(new T(function(){return E(B(A(_9L,[_9x]))[2]);}),_9,_3x,_9N);});},_9F));},1),_7d,_5k,_])),_9O=_9M;_9H=_9K[2];return null;}})(_9H,_);if(_9I!=null){return _9I;}}})(new T(function(){var _9P=B(_2I(new T(function(){return E(E(_9r)[1]);}),_9,function(_9Q){return new F(function(){return _2I(new T(function(){return E(E(_9r)[2]);}),_9,_3x,_9Q);});},_88));if(!_9P[0]){var _9R=E(_70);}else{var _9R=B(_6H(function(_9S){var _9T=new T(function(){return B(A(_9S,[_9r]));});return new F(function(){return _4r(_85,new T(function(){return E(E(_9r)[1]);}),new T(function(){return E(E(_9r)[2]);}),new T(function(){return E(E(_9T)[1]);},1),new T(function(){return E(E(_9T)[2]);},1));});},B(_3u(_87,E(E(_9P[1])[2])[1]))));}return _9R;}),_)),_9U=_9G;return _9t;},_9V=E(_9w);if(!_9V[0]){return new F(function(){return _9E(_,_11);});}else{var _9W=jsQuerySelectorAll(E(_9V[1])[1],toJSStr(_9z)),_9X=_9W,_9Y=function(_9Z,_){var _a0=E(_9Z);if(!_a0[0]){return _11;}else{var _a1=jsQuerySelectorAll(E(_a0[1])[1],toJSStr(_9z)),_a2=_a1,_a3=B(_9Y(_a0[2],_)),_a4=_a3;return [1,_a2,_a4];}},_a5=B(_9Y(_9V[2],_)),_a6=_a5;return new F(function(){return _9E(_,[1,_9X,_a6]);});}}else{return E(_1);}}else{return E(_1);}};break;case 4:var _8z=function(_a7,_){var _a8=E(_a7),_a9=jsQuerySelectorAll(_a8[1],toJSStr([1,_7w,new T(function(){if(!E(_87)){var _aa=B(_K(E(E(_86)[2])[4],_7g));}else{var _aa=B(_K(E(E(_86)[1])[4],_7g));}return _aa;})])),_ab=_a9,_ac=B(_6x(_ab,_)),_ad=_ac;return _a8;};break;case 5:var _8z=function(_ae,_){var _af=E(_ae),_ag=_af[1],_ah=jsQuerySelectorAll(_ag,toJSStr([1,_7w,new T(function(){if(!E(_87)){var _ai=B(_K(E(E(_86)[2])[4],_7g));}else{var _ai=B(_K(E(E(_86)[1])[4],_7g));}return _ai;})])),_aj=_ah,_ak=E(_89[1])[1];if(_ak>=0){var _al=jsSetAttr(B(_4(_aj,_ak))[1],toJSStr(E(_7b)),toJSStr(E(_7e))),_am=jsQuerySelectorAll(_ag,toJSStr(E(_7x))),_an=_am,_ao=_an,_ap=function(_aq){var _ar=jsQuerySelectorAll(_aq,toJSStr(E(_6G))),_as=_ar,_at=B(A(_6R,[_7p,_7n,_as,new T(function(){if(!E(_87)){var _au=E(_88),_av=_au[0]==0?E(_3E):B(_1D(_71,_au[1]));}else{var _aw=E(_88);if(!_aw[0]){var _ax=E(_78);}else{var _ax=B(_1D(_71,B(_73(_aw[1],_aw[2]))));}var _av=_ax;}return _av;}),_])),_ay=_at;return _af;};if(!E(_87)){var _az=E(_ao);if(!_az[0]){return E(_3E);}else{return new F(function(){return _ap(E(_az[1])[1]);});}}else{var _aA=E(_ao);if(!_aA[0]){return E(_78);}else{return new F(function(){return _ap(B(_73(_aA[1],_aA[2]))[1]);});}}}else{return E(_1);}};break;default:var _8z=E(_7y);}var _aB=_8z;return _aB;}),_11]]]],_1Z,_20);});};},_aC=[6],_aD=function(_){var _aE=B(A(_17,["(function(){return document.body;})",_])),_aF=_aE;return [0,_aF];},_aG=function(_aH,_aI){return new F(function(){return A(_aH,[_aI]);});},_aJ=function(_aK,_aL){var _aM=new T(function(){var _aN=_aK+1|0;return _aN>=0?B(_2D(_aN,_aL)):E(_aL);});if(_aK>0){var _aO=function(_aP,_aQ){var _aR=E(_aP);if(!_aR[0]){return E(_aM);}else{var _aS=_aR[1];return _aQ>1?[1,_aS,new T(function(){return B(_aO(_aR[2],_aQ-1|0));})]:[1,_aS,_aM];}};return new F(function(){return _aO(_aL,_aK);});}else{return E(_aM);}},_aT=function(_aU,_aV){return new F(function(){return _aJ(E(_aU)[1],_aV);});},_aW=function(_aX,_aY){var _aZ=E(_aY);return _aZ[0]==0?[0]:[1,_aX,new T(function(){return B(_aW(_aZ[1],_aZ[2]));})];},_b0=new T(function(){return B(unCStr("init"));}),_b1=new T(function(){return B(_3B(_b0));}),_b2=[0,114514],_b3=[4,_b2],_b4=new T(function(){return B(unCStr("tail"));}),_b5=new T(function(){return B(_3B(_b4));}),_b6=function(_b7,_b8,_b9,_ba){var _bb=new T(function(){return E(E(_ba)[2]);});if(!B(_2I(_b8,_9,_3x,new T(function(){if(!E(_bb)){var _bc=E(E(_ba)[4]),_bd=_bc[0]==0?E(_3E):E(_bc[1]);}else{var _be=E(E(_ba)[4]),_bd=_be[0]==0?E(_78):B(_73(_be[1],_be[2]));}return _bd;})))[0]){var _bf=E(_b9),_bg=_bf[1],_bh=new T(function(){var _bi=E(_ba),_bj=_bi[1],_bk=_bi[2],_bl=_bi[4];return [0,new T(function(){if(!E(_bk)){var _bm=E(_bj),_bn=[0,_bm[1],new T(function(){var _bo=E(_bm[2]);return [0,new T(function(){return B(_aT(_b7,_bo[1]));}),_bo[2],_bo[3],_bo[4],_bo[5]];})];}else{var _bp=E(_bj),_bn=[0,new T(function(){var _bq=E(_bp[1]);return [0,new T(function(){return B(_aT(_b7,_bq[1]));}),_bq[2],_bq[3],_bq[4],_bq[5]];}),_bp[2]];}return _bn;}),_bk,_bi[3],new T(function(){var _br=new T(function(){return B(_2I(_b8,_aG,function(_bs){return E([1,[0,_bb,_bf]]);},new T(function(){if(!E(_bb)){var _bt=E(_bl),_bu=_bt[0]==0?E(_3E):E(_bt[1]);}else{var _bv=E(_bl),_bu=_bv[0]==0?E(_78):B(_73(_bv[1],_bv[2]));}return _bu;})));});if(!E(_bb)){var _bw=[1,_br,new T(function(){var _bx=E(_bl);return _bx[0]==0?E(_b5):E(_bx[2]);})];}else{var _by=E(_bl);if(!_by[0]){var _bz=E(_b1);}else{var _bz=B(_K(B(_aW(_by[1],_by[2])),[1,_br,_11]));}var _bw=_bz;}return _bw;}),_bi[5]];},1);return _bg<=10?E(_bg)==1?[0,_5i,new T(function(){var _bA=E(_bh);return [0,_bA[1],_bA[2],_b3,_bA[4],_bA[5]];})]:[0,_5i,new T(function(){var _bB=E(_bh);return [0,_bB[1],_bB[2],_aC,_bB[4],_bB[5]];})]:[0,_5i,new T(function(){var _bC=E(_bh);return [0,_bC[1],_bC[2],[4,_5a],_bC[4],_bC[5]];})];}else{return [0,_5i,_ba];}},_bD=function(_bE){return [0,E(E(_bE))];},_bF=function(_bG,_bH){return [0,imul(E(_bG)[1],E(_bH)[1])|0];},_bI=function(_bJ,_bK){return [0,E(_bJ)[1]-E(_bK)[1]|0];},_bL=function(_bM){var _bN=E(_bM),_bO=_bN[1];return _bO<0?[0, -_bO]:E(_bN);},_bP=function(_bQ){var _bR=E(_bQ);return _bR[0]==0?E(_bR[1]):I_toInt(_bR[1]);},_bS=function(_bT){return [0,B(_bP(_bT))];},_bU=function(_bV){return [0, -E(_bV)[1]];},_bW=[0,-1],_bX=[0,0],_bY=[0,1],_bZ=function(_c0){var _c1=E(_c0)[1];return _c1>=0?E(_c1)==0?E(_bX):E(_bY):E(_bW);},_c2=[0,_3b,_bF,_bI,_bU,_bL,_bZ,_bS],_c3=[0,2147483647],_c4=[0,-2147483648],_c5=[0,2147483562],_c6=[0,1],_c7=[0,_c6,_c5],_c8=function(_c9){return E(_c7);},_ca=function(_cb,_cc){return [0,E(_cb)[1],E(_cc)[1]];},_cd=function(_ce,_cf){var _cg=quot(_cf,52774),_ch=(imul(40692,_cf-(imul(_cg,52774)|0)|0)|0)-(imul(_cg,3791)|0)|0,_ci=new T(function(){if(_ch>=0){var _cj=[0,_ch];}else{var _cj=[0,_ch+2147483399|0];}var _ck=_cj;return _ck;}),_cl=quot(_ce,53668),_cm=(imul(40014,_ce-(imul(_cl,53668)|0)|0)|0)-(imul(_cl,12211)|0)|0,_cn=new T(function(){if(_cm>=0){var _co=[0,_cm];}else{var _co=[0,_cm+2147483563|0];}var _cp=_co;return _cp;});return [0,new T(function(){var _cq=E(_cn)[1]-E(_ci)[1]|0;if(_cq>=1){var _cr=[0,_cq];}else{var _cr=[0,_cq+2147483562|0];}var _cs=_cr,_ct=_cs,_cu=_ct,_cv=_cu;return _cv;}),new T(function(){return B(_ca(_cn,_ci));})];},_cw=function(_cx){var _cy=E(_cx),_cz=B(_cd(_cy[1],_cy[2]));return [0,_cz[1],_cz[2]];},_cA=function(_cB,_cC){var _cD=new T(function(){return E(B(_cd(_cB,_cC))[2]);});return [0,new T(function(){var _cE=E(_cB);if(_cE==2147483562){var _cF=[0,1,E(_cD)[2]];}else{var _cF=[0,_cE+1|0,E(_cD)[2]];}return _cF;}),new T(function(){var _cG=E(_cD)[1],_cH=E(_cC);if(_cH==1){var _cI=[0,_cG,2147483398];}else{var _cI=[0,_cG,_cH-1|0];}var _cJ=_cI;return _cJ;})];},_cK=function(_cL){var _cM=E(_cL),_cN=B(_cA(_cM[1],_cM[2]));return [0,_cN[1],_cN[2]];},_cO=[0,_cw,_c8,_cK],_cP=[0,1],_cQ=[0,2147483562],_cR=new T(function(){return B(unCStr("base"));}),_cS=new T(function(){return B(unCStr("GHC.Exception"));}),_cT=new T(function(){return B(unCStr("ArithException"));}),_cU=[0,I_fromBits([4194982440,719304104]),I_fromBits([3110813675,1843557400]),_cR,_cS,_cT],_cV=[0,I_fromBits([4194982440,719304104]),I_fromBits([3110813675,1843557400]),_cU,_11],_cW=function(_cX){return E(_cV);},_cY=function(_cZ){return E(E(_cZ)[1]);},_d0=function(_d1,_d2,_d3){var _d4=B(A(_d1,[_])),_d5=B(A(_d2,[_])),_d6=hs_eqWord64(_d4[1],_d5[1]),_d7=_d6;if(!E(_d7)){return [0];}else{var _d8=hs_eqWord64(_d4[2],_d5[2]),_d9=_d8;return E(_d9)==0?[0]:[1,_d3];}},_da=function(_db){var _dc=E(_db);return new F(function(){return _d0(B(_cY(_dc[1])),_cW,_dc[2]);});},_dd=new T(function(){return B(unCStr("arithmetic underflow"));}),_de=new T(function(){return B(unCStr("arithmetic overflow"));}),_df=new T(function(){return B(unCStr("Ratio has zero denominator"));}),_dg=new T(function(){return B(unCStr("denormal"));}),_dh=new T(function(){return B(unCStr("divide by zero"));}),_di=new T(function(){return B(unCStr("loss of precision"));}),_dj=function(_dk){switch(E(_dk)){case 0:return E(_de);case 1:return E(_dd);case 2:return E(_di);case 3:return E(_dh);case 4:return E(_dg);default:return E(_df);}},_dl=function(_dm){return new F(function(){return _K(_dd,_dm);});},_dn=function(_dm){return new F(function(){return _K(_de,_dm);});},_do=function(_dm){return new F(function(){return _K(_df,_dm);});},_dp=function(_dm){return new F(function(){return _K(_dg,_dm);});},_dq=function(_dm){return new F(function(){return _K(_dh,_dm);});},_dr=function(_dm){return new F(function(){return _K(_di,_dm);});},_ds=function(_dt){switch(E(_dt)){case 0:return E(_dn);case 1:return E(_dl);case 2:return E(_dr);case 3:return E(_dq);case 4:return E(_dp);default:return E(_do);}},_du=[0,44],_dv=[0,93],_dw=[0,91],_dx=function(_dy,_dz,_dA){var _dB=E(_dz);return _dB[0]==0?B(unAppCStr("[]",_dA)):[1,_dw,new T(function(){return B(A(_dy,[_dB[1],new T(function(){var _dC=function(_dD){var _dE=E(_dD);return _dE[0]==0?E([1,_dv,_dA]):[1,_du,new T(function(){return B(A(_dy,[_dE[1],new T(function(){return B(_dC(_dE[2]));})]));})];};return B(_dC(_dB[2]));})]));})];},_dF=function(_dG,_dH){return new F(function(){return _dx(_ds,_dG,_dH);});},_dI=function(_dJ,_dK){switch(E(_dK)){case 0:return E(_dn);case 1:return E(_dl);case 2:return E(_dr);case 3:return E(_dq);case 4:return E(_dp);default:return E(_do);}},_dL=[0,_dI,_dj,_dF],_dM=new T(function(){return [0,_cW,_dL,_dN,_da];}),_dN=function(_dm){return [0,_dM,_dm];},_dO=3,_dP=new T(function(){return B(_dN(_dO));}),_dQ=new T(function(){return die(_dP);}),_dR=function(_dS,_dT){var _dU=E(_dS);if(!_dU[0]){var _dV=_dU[1],_dW=E(_dT);return _dW[0]==0?_dV==_dW[1]:I_compareInt(_dW[1],_dV)==0?true:false;}else{var _dX=_dU[1],_dY=E(_dT);return _dY[0]==0?I_compareInt(_dX,_dY[1])==0?true:false:I_compare(_dX,_dY[1])==0?true:false;}},_dZ=function(_e0){return E(E(_e0)[7]);},_e1=function(_e2,_e3){var _e4=E(_e2);if(!_e4[0]){var _e5=_e4[1],_e6=E(_e3);return _e6[0]==0?_e5>=_e6[1]:I_compareInt(_e6[1],_e5)<=0;}else{var _e7=_e4[1],_e8=E(_e3);return _e8[0]==0?I_compareInt(_e7,_e8[1])>=0:I_compare(_e7,_e8[1])>=0;}},_e9=[0,0],_ea=function(_eb,_ec){var _ed=E(_eb);if(!_ed[0]){var _ee=_ed[1],_ef=E(_ec);return _ef[0]==0?_ee>_ef[1]:I_compareInt(_ef[1],_ee)<0;}else{var _eg=_ed[1],_eh=E(_ec);return _eh[0]==0?I_compareInt(_eg,_eh[1])>0:I_compare(_eg,_eh[1])>0;}},_ei=[0,1000],_ej=function(_ek,_el){while(1){var _em=E(_ek);if(!_em[0]){var _en=_em[1],_eo=E(_el);if(!_eo[0]){var _ep=_eo[1],_eq=subC(_en,_ep);if(!E(_eq[2])){return [0,_eq[1]];}else{_ek=[1,I_fromInt(_en)];_el=[1,I_fromInt(_ep)];continue;}}else{_ek=[1,I_fromInt(_en)];_el=_eo;continue;}}else{var _er=E(_el);if(!_er[0]){_ek=_em;_el=[1,I_fromInt(_er[1])];continue;}else{return [1,I_sub(_em[1],_er[1])];}}}},_es=function(_et,_eu){var _ev=_et%_eu;if(_et<=0){if(_et>=0){return E(_ev);}else{if(_eu<=0){return E(_ev);}else{var _ew=E(_ev);return _ew==0?0:_ew+_eu|0;}}}else{if(_eu>=0){if(_et>=0){return E(_ev);}else{if(_eu<=0){return E(_ev);}else{var _ex=E(_ev);return _ex==0?0:_ex+_eu|0;}}}else{var _ey=E(_ev);return _ey==0?0:_ey+_eu|0;}}},_ez=function(_eA,_eB){while(1){var _eC=E(_eA);if(!_eC[0]){var _eD=E(_eC[1]);if(_eD==(-2147483648)){_eA=[1,I_fromInt(-2147483648)];continue;}else{var _eE=E(_eB);if(!_eE[0]){return [0,B(_es(_eD,_eE[1]))];}else{_eA=[1,I_fromInt(_eD)];_eB=_eE;continue;}}}else{var _eF=_eC[1],_eG=E(_eB);return _eG[0]==0?[0,I_toInt(I_mod(_eF,I_fromInt(_eG[1])))]:[1,I_mod(_eF,_eG[1])];}}},_eH=function(_eI,_eJ){while(1){var _eK=E(_eI);if(!_eK[0]){var _eL=_eK[1],_eM=E(_eJ);if(!_eM[0]){var _eN=_eM[1],_eO=addC(_eL,_eN);if(!E(_eO[2])){return [0,_eO[1]];}else{_eI=[1,I_fromInt(_eL)];_eJ=[1,I_fromInt(_eN)];continue;}}else{_eI=[1,I_fromInt(_eL)];_eJ=_eM;continue;}}else{var _eP=E(_eJ);if(!_eP[0]){_eI=_eK;_eJ=[1,I_fromInt(_eP[1])];continue;}else{return [1,I_add(_eK[1],_eP[1])];}}}},_eQ=function(_eR){return [0,_eR];},_eS=function(_eT,_eU){while(1){var _eV=E(_eT);if(!_eV[0]){var _eW=_eV[1],_eX=E(_eU);if(!_eX[0]){var _eY=_eX[1];if(!(imul(_eW,_eY)|0)){return [0,imul(_eW,_eY)|0];}else{_eT=[1,I_fromInt(_eW)];_eU=[1,I_fromInt(_eY)];continue;}}else{_eT=[1,I_fromInt(_eW)];_eU=_eX;continue;}}else{var _eZ=E(_eU);if(!_eZ[0]){_eT=_eV;_eU=[1,I_fromInt(_eZ[1])];continue;}else{return [1,I_mul(_eV[1],_eZ[1])];}}}},_f0=function(_f1,_f2,_f3,_f4){while(1){var _f5=(function(_f6,_f7,_f8,_f9){if(!B(_ea(_f7,_f8))){var _fa=B(_eH(B(_ej(_f8,_f7)),_cP)),_fb=B((function(_fc,_fd,_fe){while(1){if(!B(_e1(_fc,B(_eS(_fa,_ei))))){var _ff=E(_fe),_fg=B(_cd(_ff[1],_ff[2])),_fh=B(_eS(_fc,_cQ)),_fi=B(_eH(B(_eS(_fd,_cQ)),B(_ej(B(_eQ(E(_fg[1])[1])),_cP))));_fe=_fg[2];_fc=_fh;_fd=_fi;continue;}else{return [0,_fd,_fe];}}})(_cP,_e9,_f9));return [0,new T(function(){return B(A(_dZ,[_f6,new T(function(){if(!B(_dR(_fa,_e9))){var _fj=B(_eH(_f7,B(_ez(_fb[1],_fa))));}else{var _fj=E(_dQ);}return _fj;})]));}),_fb[2]];}else{var _fk=_f6,_fl=_f8,_fm=_f7,_fn=_f9;_f1=_fk;_f2=_fl;_f3=_fm;_f4=_fn;return null;}})(_f1,_f2,_f3,_f4);if(_f5!=null){return _f5;}}},_fo=function(_fp){return E(E(_fp)[2]);},_fq=function(_fr){return E(E(_fr)[1]);},_fs=function(_ft,_fu,_fv,_fw,_fx){while(1){var _fy=(function(_fz,_fA,_fB,_fC,_fD){if(!B(_ea(_fB,_fC))){var _fE=B(_eH(B(_ej(_fC,_fB)),_cP)),_fF=new T(function(){return B(A(_fo,[_fz,_fD]));}),_fG=new T(function(){return E(E(_fF)[1]);}),_fH=new T(function(){return B(_eH(B(_ej(B(_eQ(E(E(_fF)[2])[1])),B(_eQ(E(_fG)[1])))),_cP));}),_fI=B((function(_fJ,_fK,_fL){while(1){if(!B(_e1(_fJ,B(_eS(_fE,_ei))))){var _fM=B(A(new T(function(){return B(_fq(_fz));}),[_fL])),_fN=B(_eS(_fJ,_fH)),_fO=B(_eH(B(_eS(_fK,_fH)),B(_ej(B(_eQ(E(_fM[1])[1])),new T(function(){return B(_eQ(E(_fG)[1]));})))));_fL=_fM[2];_fJ=_fN;_fK=_fO;continue;}else{return [0,_fK,_fL];}}})(_cP,_e9,_fD));return [0,new T(function(){return B(A(_dZ,[_fA,new T(function(){if(!B(_dR(_fE,_e9))){var _fP=B(_eH(_fB,B(_ez(_fI[1],_fE))));}else{var _fP=E(_dQ);}return _fP;})]));}),_fI[2]];}else{var _fQ=_fz,_fR=_fA,_fS=_fC,_fT=_fB,_fU=_fD;_ft=_fQ;_fu=_fR;_fv=_fS;_fw=_fT;_fx=_fU;return null;}})(_ft,_fu,_fv,_fw,_fx);if(_fy!=null){return _fy;}}},_fV=[0,0],_fW=function(_fX,_fY,_fZ){var _g0=E(_fY);if(!_g0){return [0];}else{var _g1=new T(function(){var _g2=B(_fs(_fX,_c2,_fV,B(_eQ(_g0)),_fZ));return [0,_g2[1],_g2[2]];});return [1,[0,new T(function(){return E(E(_g1)[1]);}),_fZ],new T(function(){return B(_fW(_fX,_g0-1|0,new T(function(){return E(E(_g1)[2]);})));})];}},_g3=function(_g4){var _g5=E(_g4);if(!_g5[0]){return [0,_11,_11];}else{var _g6=E(_g5[1]),_g7=new T(function(){var _g8=B(_g3(_g5[2]));return [0,_g8[1],_g8[2]];});return [0,[1,_g6[1],new T(function(){return E(E(_g7)[1]);})],[1,_g6[2],new T(function(){return E(E(_g7)[2]);})]];}},_g9=new T(function(){return B(unCStr("[extractTree] impossible"));}),_ga=new T(function(){return B(err(_g9));}),_gb=function(_gc,_gd){var _ge=function(_gf){var _gg=E(_gd);if(!_gg[0]){return E(_ga);}else{var _gh=_gg[1],_gi=_gg[3],_gj=E(_gg[2]);if(!_gj[0]){var _gk=new T(function(){var _gl=B(_gb(_gc-1|0,_gi));return [0,_gl[1],_gl[2]];});return [0,new T(function(){return E(E(_gk)[1]);}),new T(function(){return [1,_gh-1|0,E(_gj),E(E(E(_gk)[2]))];})];}else{var _gm=_gj[1],_gn=function(_go){if(_gc>=_gm){var _gp=new T(function(){var _gq=B(_gb(_gc-_gm|0,_gi));return [0,_gq[1],_gq[2]];});return [0,new T(function(){return E(E(_gp)[1]);}),new T(function(){return [1,_gh-1|0,E(_gj),E(E(E(_gp)[2]))];})];}else{var _gr=new T(function(){var _gs=B(_gb(_gc,_gj));return [0,_gs[1],_gs[2]];});return [0,new T(function(){return E(E(_gr)[1]);}),new T(function(){return [1,_gh-1|0,E(E(E(_gr)[2])),E(_gi)];})];}},_gt=E(_gi);if(!_gt[0]){return (_gc+1|0)!=_gh?B(_gn(_)):[0,_gt[1],_gj];}else{return new F(function(){return _gn(_);});}}}};switch(E(_gc)){case 0:var _gu=E(_gd);if(!_gu[0]){return new F(function(){return _ge(_);});}else{var _gv=E(_gu[2]);return _gv[0]==0?[0,_gv[1],_gu[3]]:B(_ge(_));}break;case 1:var _gw=E(_gd);if(!_gw[0]){return new F(function(){return _ge(_);});}else{if(E(_gw[1])==2){var _gx=E(_gw[2]);if(!_gx[0]){var _gy=E(_gw[3]);return _gy[0]==0?[0,_gy[1],_gx]:B(_ge(_));}else{return new F(function(){return _ge(_);});}}else{return new F(function(){return _ge(_);});}}break;default:return new F(function(){return _ge(_);});}},_gz=new T(function(){return B(unCStr("[shuffle] called with lists of different lengths"));}),_gA=new T(function(){return B(err(_gz));}),_gB=function(_gC,_gD){var _gE=function(_gF){var _gG=E(_gD);if(!_gG[0]){return E(_gA);}else{var _gH=new T(function(){var _gI=B(_gb(E(_gG[1])[1],_gC));return [0,_gI[1],_gI[2]];});return [1,new T(function(){return E(E(_gH)[1]);}),new T(function(){return B(_gB(E(_gH)[2],_gG[2]));})];}},_gJ=E(_gC);return _gJ[0]==0?E(_gD)[0]==0?[1,_gJ[1],_11]:B(_gE(_)):B(_gE(_));},_gK=function(_gL){var _gM=E(_gL);if(!_gM[0]){return [0];}else{var _gN=_gM[1],_gO=E(_gM[2]);if(!_gO[0]){return [1,_gN,_11];}else{var _gP=E(_gO[1]);return [1,new T(function(){var _gQ=E(E(_gN));if(!_gQ[0]){var _gR=E(_gP);if(!_gR[0]){var _gS=[1,2,E(_gQ),E(_gR)];}else{var _gS=[1,_gR[1]+1|0,E(_gQ),E(_gR)];}var _gT=_gS;}else{var _gU=_gQ[1],_gV=E(_gP);if(!_gV[0]){var _gW=[1,_gU+1|0,E(_gQ),E(_gV)];}else{var _gW=[1,_gU+_gV[1]|0,E(_gQ),E(_gV)];}var _gT=_gW;}return _gT;}),new T(function(){return B(_gK(_gO[2]));})];}}},_gX=new T(function(){return B(_gK(_11));}),_gY=new T(function(){return B(_gZ(_gX));}),_gZ=function(_h0){while(1){var _h1=E(_h0);if(!_h1[0]){return E(_gY);}else{if(!E(_h1[2])[0]){return E(_h1[1]);}else{_h0=B(_gK(_h1));continue;}}}},_h2=function(_h3,_h4,_h5,_h6,_h7,_h8,_h9){var _ha=B(_2I(_h4,_9,_3x,new T(function(){if(!E(_h6)){var _hb=E(E(E(_h5)[2])[1]);}else{var _hb=E(E(E(_h5)[1])[1]);}return _hb;})))[1],_hc=new T(function(){if(!E(_h6)){var _hd=E(E(_h5)[2]);}else{var _hd=E(E(_h5)[1]);}return _hd;}),_he=new T(function(){var _hf=new T(function(){return E(E(_hc)[2]);});return B(_gB(B(_gZ(B(_1D(_bD,_hf)))),B(_g3(B(_fW(_cO,B(_F(_hf,0))-1|0,_h9))))[1]));}),_hg=new T(function(){if(!E(_h6)){var _hh=[0,E(_h5)[1],new T(function(){var _hi=E(_hc);return [0,_hi[1],[1,[0,_ha],_he],_hi[3],_hi[4],_hi[5]];})];}else{var _hh=[0,new T(function(){var _hj=E(_hc);return [0,_hj[1],[1,[0,_ha],_he],_hj[3],_hj[4],_hj[5]];}),E(_h5)[2]];}return _hh;});return E(_ha)==1?_h3<=2?[0,new T(function(){if(!E(_h6)){var _hk=E(_hg),_hl=[0,_hk[1],new T(function(){var _hm=E(_hk[2]);return [0,new T(function(){return B(_aT(_h4,_hm[1]));}),_hm[2],_hm[3],_hm[4],_hm[5]];})];}else{var _hn=E(_hg),_hl=[0,new T(function(){var _ho=E(_hn[1]);return [0,new T(function(){return B(_aT(_h4,_ho[1]));}),_ho[2],_ho[3],_ho[4],_ho[5]];}),_hn[2]];}return _hl;}),_h6,_aC,_h8,new T(function(){return E(B(_f0(_c2,_c4,_c3,_h9))[2]);})]:[0,new T(function(){if(!E(_h6)){var _hp=E(_hg),_hq=[0,_hp[1],new T(function(){var _hr=E(_hp[2]);return [0,new T(function(){return B(_aT(_h4,_hr[1]));}),_hr[2],_hr[3],_hr[4],_hr[5]];})];}else{var _hs=E(_hg),_hq=[0,new T(function(){var _ht=E(_hs[1]);return [0,new T(function(){return B(_aT(_h4,_ht[1]));}),_ht[2],_ht[3],_ht[4],_ht[5]];}),_hs[2]];}return _hq;}),_h6,[4,[0,_h3-2|0]],_h8,new T(function(){return E(B(_f0(_c2,_c4,_c3,_h9))[2]);})]:_h3<=1?[0,new T(function(){if(!E(_h6)){var _hu=E(_hg),_hv=[0,_hu[1],new T(function(){var _hw=E(_hu[2]);return [0,new T(function(){return B(_aT(_h4,_hw[1]));}),_hw[2],_hw[3],_hw[4],_hw[5]];})];}else{var _hx=E(_hg),_hv=[0,new T(function(){var _hy=E(_hx[1]);return [0,new T(function(){return B(_aT(_h4,_hy[1]));}),_hy[2],_hy[3],_hy[4],_hy[5]];}),_hx[2]];}return _hv;}),_h6,_aC,_h8,new T(function(){return E(B(_f0(_c2,_c4,_c3,_h9))[2]);})]:[0,new T(function(){if(!E(_h6)){var _hz=E(_hg),_hA=[0,_hz[1],new T(function(){var _hB=E(_hz[2]);return [0,new T(function(){return B(_aT(_h4,_hB[1]));}),_hB[2],_hB[3],_hB[4],_hB[5]];})];}else{var _hC=E(_hg),_hA=[0,new T(function(){var _hD=E(_hC[1]);return [0,new T(function(){return B(_aT(_h4,_hD[1]));}),_hD[2],_hD[3],_hD[4],_hD[5]];}),_hC[2]];}return _hA;}),_h6,[4,[0,_h3-1|0]],_h8,new T(function(){return E(B(_f0(_c2,_c4,_c3,_h9))[2]);})];},_hE=[1],_hF=[3],_hG=[7,_5j],_hH=[7,_5k],_hI=function(_hJ){return [0];},_hK=function(_hL,_hM,_hN,_hO,_hP,_hQ){var _hR=E(_hL);switch(_hR[0]){case 0:var _hS=_hR[1],_hT=E(_hO);switch(_hT[0]){case 1:var _hU=E(_hM),_hV=_hU[1],_hW=_hU[2],_hX=B(_2I(_hS,_9,_3x,new T(function(){if(!E(_hN)){var _hY=E(E(_hW)[1]);}else{var _hY=E(E(_hV)[1]);}return _hY;})))[1];return !E(_hN)?!B(_5d(_hX,E(_hW)[1]))?[0,_hU,_5j,_hE,_hP,_hQ]:[0,_hU,_5j,[5,_hS],_hP,_hQ]:!B(_5d(_hX,E(_hV)[1]))?[0,_hU,_5k,_hE,_hP,_hQ]:[0,_hU,_5k,[5,_hS],_hP,_hQ];case 4:var _hZ=B(_h2(E(_hT[1])[1],_hS,_hM,_hN,_hT,_hP,_hQ));return [0,_hZ[1],_hZ[2],_hZ[3],_hZ[4],_hZ[5]];case 5:var _i0=E(_hS);if(_i0[1]!=E(_hT[1])[1]){var _i1=E(_hM),_i2=_i1[1],_i3=_i1[2],_i4=B(_2I(_i0,_9,_3x,new T(function(){if(!E(_hN)){var _i5=E(E(_i3)[1]);}else{var _i5=E(E(_i2)[1]);}return _i5;})))[1];return !E(_hN)?!B(_5d(_i4,E(_i3)[1]))?[0,_i1,_5j,_hT,_hP,_hQ]:[0,_i1,_5j,[5,_i0],_hP,_hQ]:!B(_5d(_i4,E(_i2)[1]))?[0,_i1,_5k,_hT,_hP,_hQ]:[0,_i1,_5k,[5,_i0],_hP,_hQ];}else{return [0,_hM,_hN,_hE,_hP,_hQ];}break;default:return [0,_hM,_hN,_hT,_hP,_hQ];}break;case 1:var _i6=E(_hR[1]),_i7=_i6[1],_i8=_i6[2],_i9=E(_hO);switch(_i9[0]){case 1:var _ia=B(_2I(_i7,_9,function(_ib){return new F(function(){return _2I(_i8,_9,_3x,_ib);});},_hP));if(!_ia[0]){return [0,_hM,_hN,_hE,_hP,_hQ];}else{var _ic=_ia[1];if(!E(_hN)){var _id=E(_ic);return !E(_id[1])?B(_4S(E(_id[2])[1],_5j,_hP,_i7,_i8))[0]==0?[0,_hM,_5j,_hE,_hP,_hQ]:[0,_hM,_5j,[2,_i6],_hP,_hQ]:[0,_hM,_5j,_hE,_hP,_hQ];}else{var _ie=E(_ic);return !E(_ie[1])?[0,_hM,_5k,_hE,_hP,_hQ]:B(_4S(E(_ie[2])[1],_5k,_hP,_i7,_i8))[0]==0?[0,_hM,_5k,_hE,_hP,_hQ]:[0,_hM,_5k,[2,_i6],_hP,_hQ];}}break;case 2:var _if=E(_i9[1]),_ig=_if[2],_ih=E(_if[1]),_ii=E(_i7),_ij=_ii[1],_ik=[0,_hM,_hN,_i9,_hP,_hQ];if(_ih[1]!=_ij){if(!B(_4r(_ik,_ih,_ig,_ii,_i8))){return E(_ik);}else{var _il=B(_2I(_ih,_9,function(_im){return new F(function(){return _2I(_ig,_9,_3x,_im);});},_hP));if(!_il[0]){return E(_ik);}else{var _in=E(_i8),_io=new T(function(){return B(_2I(_ih,_aG,function(_ip){return new F(function(){return _2I(_ig,_aG,_hI,_ip);});},new T(function(){return B(_2I(_ii,_aG,function(_iq){return new F(function(){return _2I(_in,_aG,function(_ir){return [1,_il[1]];},_iq);});},_hP));})));});return E(_in[1])==1?!E(_hN)?(_ij+1|0)!=B(_F(_hP,0))?[0,_hM,_5j,_aC,_io,_hQ]:[0,_hM,_5j,_hG,_io,_hQ]:E(_ij)==0?[0,_hM,_5k,_hH,_io,_hQ]:[0,_hM,_5k,_aC,_io,_hQ]:[0,_hM,_hN,_aC,_io,_hQ];}}}else{var _is=E(_ig),_it=E(_i8),_iu=_it[1];if(_is[1]!=_iu){if(!B(_4r(_ik,_ih,_is,_ii,_it))){return E(_ik);}else{var _iv=B(_2I(_ih,_9,function(_iw){return new F(function(){return _2I(_is,_9,_3x,_iw);});},_hP));if(!_iv[0]){return E(_ik);}else{var _ix=new T(function(){return B(_2I(_ih,_aG,function(_iy){return new F(function(){return _2I(_is,_aG,_hI,_iy);});},new T(function(){return B(_2I(_ii,_aG,function(_iz){return new F(function(){return _2I(_it,_aG,function(_iA){return [1,_iv[1]];},_iz);});},_hP));})));});return E(_iu)==1?!E(_hN)?(_ij+1|0)!=B(_F(_hP,0))?[0,_hM,_5j,_aC,_ix,_hQ]:[0,_hM,_5j,_hG,_ix,_hQ]:E(_ij)==0?[0,_hM,_5k,_hH,_ix,_hQ]:[0,_hM,_5k,_aC,_ix,_hQ]:[0,_hM,_hN,_aC,_ix,_hQ];}}}else{return [0,_hM,_hN,_hE,_hP,_hQ];}}break;case 5:var _iB=_i9[1],_iC=[0,_hM,_hN,_i9,_hP,_hQ],_iD=function(_iE){var _iF=function(_iG){return E(B(_b6(_iB,_i8,new T(function(){var _iH=E(_iB)[1];if(_iH>=0){if(!E(_hN)){var _iI=B(_4(E(E(_hM)[2])[1],_iH));}else{var _iI=B(_4(E(E(_hM)[1])[1],_iH));}var _iJ=_iI;}else{var _iJ=E(_1);}var _iK=_iJ,_iL=_iK;return _iL;}),_iC))[2]);};if(!E(_hN)){return E(E(_i7)[1])==0?B(_iF(_)):E(_iC);}else{return new F(function(){return _iF(_);});}};if(!E(_hN)){return new F(function(){return _iD(_);});}else{return E(_i7)[1]!=(B(_F(_hP,0))-1|0)?E(_iC):B(_iD(_));}break;default:return [0,_hM,_hN,_i9,_hP,_hQ];}break;default:var _iM=E(_hO);return _iM[0]==4?[0,_hM,_hN,_iM,_hP,_hQ]:[0,_hM,_hN,_hF,_hP,_hQ];}},_iN=function(_iO,_iP){var _iQ=E(_iP);return new F(function(){return _hK(_iO,_iQ[1],_iQ[2],_iQ[3],_iQ[4],_iQ[5]);});},_iR=function(_iS,_iT,_iU,_){var _iV=rMV(_iU),_iW=_iV,_=wMV(_iU,new T(function(){return B(_iN(_iS,_iW));})),_iX=rMV(_iU),_iY=_iX,_iZ=B(_aD(_)),_j0=_iZ,_j1=B(A(_7A,[_iY,_j0,_])),_j2=_j1,_j3=rMV(_iU),_j4=_j3;switch(E(E(_j4)[3])[0]){case 3:var _j5=jsSetTimeout(1000,function(_){var _j6=rMV(_iU),_j7=_j6,_=wMV(_iU,new T(function(){var _j8=E(_j7);return [0,_j8[1],_j8[2],_aC,_j8[4],_j8[5]];})),_j9=rMV(_iU),_ja=_j9,_jb=B(_aD(_)),_jc=_jb,_jd=B(A(_7A,[_ja,_jc,_])),_je=_jd,_jf=jsSetTimeout(1000,function(_){return new F(function(){return _jg(E(_iT)[1],_iU,_,_);});});return _5i;});return _5i;case 6:return new F(function(){return _jg(E(_iT)[1],_iU,_,_);});break;default:return _5i;}},_jh=new T(function(){return B(unCStr("Prelude.Enum.pred{Int}: tried to take `pred\' of minBound"));}),_ji=new T(function(){return B(err(_jh));}),_jj=function(_jk){var _jl=E(_jk);if(_jl==(-2147483648)){return E(_ji);}else{var _jm=_jl-1|0;if(0<=_jm){var _jn=function(_jo){return [1,[0,[0,_jo]],new T(function(){if(_jo!=_jm){var _jp=B(_jn(_jo+1|0));}else{var _jp=[0];}var _jq=_jp;return _jq;})];};return new F(function(){return _jn(0);});}else{return [0];}}},_jr=function(_js,_jt,_ju,_jv){return new F(function(){return A(_jt,[function(_jw){if(!E(_js)){return [1,_jw,new T(function(){var _jx=E(_jv);return _jx[0]==0?E(_b5):E(_jx[2]);})];}else{var _jy=E(_jv);if(!_jy[0]){return E(_b1);}else{return new F(function(){return _K(B(_aW(_jy[1],_jy[2])),[1,_jw,_11]);});}}},new T(function(){return B(A(_ju,[new T(function(){if(!E(_js)){var _jz=E(_jv),_jA=_jz[0]==0?E(_3E):E(_jz[1]);}else{var _jB=E(_jv),_jA=_jB[0]==0?E(_78):B(_73(_jB[1],_jB[2]));}return _jA;})]));})]);});},_jC=[2],_jD=function(_jE){return [1,_jE];},_jF=function(_jE){return [0,_jE];},_jG=function(_jH,_jI){while(1){var _jJ=E(_jI);if(!_jJ[0]){return true;}else{if(!B(A(_jH,[_jJ[1]]))){return false;}else{_jI=_jJ[2];continue;}}}},_jK=function(_jL){while(1){var _jM=(function(_jN){var _jO=E(_jN);if(!_jO[0]){return [0];}else{var _jP=_jO[2],_jQ=E(_jO[1]);if(!_jQ[0]){_jL=_jP;return null;}else{return [1,_jQ[1],new T(function(){return B(_jK(_jP));})];}}})(_jL);if(_jM!=null){return _jM;}}},_jR=function(_jS,_jT){if(_jS<=_jT){var _jU=function(_jV){return [1,[0,_jV],new T(function(){if(_jV!=_jT){var _jW=B(_jU(_jV+1|0));}else{var _jW=[0];}var _jX=_jW;return _jX;})];};return new F(function(){return _jU(_jS);});}else{return [0];}},_jY=function(_jZ,_k0){var _k1=E(_jZ);if(!_k1[0]){return E(_3E);}else{var _k2=B(_F(_k1[1],0));if(_k2==(-2147483648)){return E(_ji);}else{var _k3=_k2-1|0;if(0<=_k3){var _k4=function(_k5){return [1,new T(function(){var _k6=[0,_k5],_k7=function(_k8){var _k9=E(_k8);if(!_k9[0]){return [0];}else{var _ka=_k9[1];return [1,new T(function(){return B(A(_k0,[_ka,_k6,new T(function(){return B(_2I(_ka,_9,function(_kb){return new F(function(){return _2I(_k6,_9,_3x,_kb);});},_k1));})]));}),new T(function(){return B(_k7(_k9[2]));})];}};return B(_k7(new T(function(){var _kc=B(_F(_k1,0));if(_kc==(-2147483648)){var _kd=E(_ji);}else{var _kd=B(_jR(0,_kc-1|0));}return _kd;})));}),new T(function(){if(_k5!=_k3){var _ke=B(_k4(_k5+1|0));}else{var _ke=[0];}var _kf=_ke;return _kf;})];};return new F(function(){return _k4(0);});}else{return [0];}}}},_kg=function(_kh){return E(_kh)[0]==0?false:true;},_ki=function(_kj){var _kk=E(_kj);return !E(_kk[2])?[0]:[1,_kk[1]];},_kl=new T(function(){return B(_jR(0,2147483647));}),_km=[0,0],_kn=function(_ko){var _kp=E(_ko);return E(_kp[2])[0]==0?[1,_kp[1]]:[0];},_kq=function(_kr,_ks){while(1){var _kt=(function(_ku,_kv){var _kw=E(_kv);if(!_kw[0]){return [0];}else{var _kx=_kw[2],_ky=B(A(_ku,[_kw[1]]));if(!_ky[0]){var _kz=_ku;_ks=_kx;_kr=_kz;return null;}else{return [1,_ky[1],new T(function(){return B(_kq(_ku,_kx));})];}}})(_kr,_ks);if(_kt!=null){return _kt;}}},_kA=function(_kB,_kC){var _kD=E(_kB);if(!_kD[0]){return [0];}else{var _kE=E(_kC);return _kE[0]==0?[0]:[1,[0,_kD[1],_kE[1]],new T(function(){return B(_kA(_kD[2],_kE[2]));})];}},_kF=function(_kG){return [0,_jC,new T(function(){var _kH=E(_kG),_kI=_kH[1],_kJ=_kH[2],_kK=_kH[4],_kL=E(_kH[3]);switch(_kL[0]){case 1:var _kM=function(_kN){var _kO=E(_kN);return _kO[0]==0?E(new T(function(){if(!B(_jG(_kg,B(_jr(_kJ,_9,_3x,_kK))))){var _kP=B(_1D(_jF,B(_kq(_ki,B(_kA(_kl,new T(function(){if(!E(_kJ)){var _kQ=E(E(_kI)[2])[1],_kR=B(_1D(function(_kS){return new F(function(){return _5d(E(_kS)[1],_kQ);});},_kQ));}else{var _kT=E(E(_kI)[1])[1],_kR=B(_1D(function(_kU){return new F(function(){return _5d(E(_kU)[1],_kT);});},_kT));}return _kR;},1)))))));}else{var _kP=[0];}var _kV=_kP;return _kV;})):[1,[1,_kO[1]],new T(function(){return B(_kM(_kO[2]));})];},_kW=B(_kM(B(_jK(B(_6C(B(_jY(_kK,function(_kX,_kY,_kZ){var _l0=E(_kZ);if(!_l0[0]){return [0];}else{var _l1=E(_l0[1]),_l2=function(_l3){return B(_4S(E(_l1[2])[1],_kJ,_kK,_kX,_kY))[0]==0?[0]:[1,[0,_kX,_kY]];};return !E(_l1[1])?!E(_kJ)?B(_l2(_)):[0]:!E(_kJ)?[0]:B(_l2(_));}}))))))));break;case 2:var _l4=E(_kL[1]),_l5=_l4[1],_l6=_l4[2],_l7=B(_2I(_l5,_9,function(_l8){return new F(function(){return _2I(_l6,_9,_3x,_l8);});},_kK));if(!_l7[0]){var _l9=E(_70);}else{var _l9=B(_1D(_jD,B(_4S(E(E(_l7[1])[2])[1],_kJ,_kK,_l5,_l6))));}var _la=_l9,_kW=_la;break;case 4:if(!E(_kJ)){var _lb=B(_jj(B(_F(E(E(_kI)[2])[1],0))));}else{var _lb=B(_jj(B(_F(E(E(_kI)[1])[1],0))));}var _kW=_lb;break;case 5:var _kW=B(_1D(function(_lc){return [1,[0,new T(function(){if(!E(_kJ)){var _ld=E(_km);}else{var _le=B(_F(_kK,0));if(_le==(-2147483648)){var _lf=E(_ji);}else{var _lf=[0,_le-1|0];}var _ld=_lf;}return _ld;}),_lc]];},B(_kq(_kn,B(_kA(_kl,new T(function(){return B(_jr(_kJ,_9,_3x,_kK));},1)))))));break;default:var _kW=[0];}var _lg=_kW;return _lg;})];},_lh=function(_li){var _lj=B(_kF(_li));return [1,_lj[1],_lj[2]];},_lk=[0,0],_ll=function(_lm){var _ln=new T(function(){var _lo=B(_F(B(_lh(_lm)),0));if(_lo==(-2147483648)){var _lp=E(_ji);}else{var _lq=B(_f0(_c2,_lk,B(_eQ(_lo-1|0)),new T(function(){return E(E(_lm)[5]);}))),_lp=[0,_lq[1],_lq[2]];}var _lr=_lp;return _lr;});return [0,new T(function(){return B(_2I(new T(function(){return E(E(_ln)[1]);}),_9,_3x,new T(function(){return B(_lh(_lm));})));}),new T(function(){return E(E(_ln)[2]);})];},_ls=function(_lt,_lu,_){var _lv=rMV(_lu),_lw=_lv,_lx=E(_lw);if(!E(_lx[2])){var _ly=new T(function(){var _lz=B(_ll(_lx));return [0,_lz[1],_lz[2]];}),_lA=B(_iR(new T(function(){return E(E(_ly)[1]);},1),_lt,_lu,_)),_lB=_lA,_lC=rMV(_lu),_lD=_lC,_=wMV(_lu,new T(function(){var _lE=E(_lD);return [0,_lE[1],_lE[2],_lE[3],_lE[4],new T(function(){return E(E(_ly)[2]);})];})),_lF=jsSetTimeout(1000,function(_){return new F(function(){return _ls(_lt,_lu,_);});});return _5i;}else{return _5i;}},_lG=[0],_lH=function(_lI,_lJ,_){var _lK=B(A(_lI,[_])),_lL=_lK,_lM=E(_lJ),_lN=jsSetTimeout(1000,_lM);return _5i;},_lO=function(_lP,_lQ,_lR,_lS,_lT){var _lU=function(_lV,_lW){var _lX=new T(function(){return !E(_lR)?[0,_lP,new T(function(){var _lY=E(_lQ);return [0,[1,_lV,_lY[1]],_lY[2],_lY[3],_lY[4],_lY[5]];})]:[0,new T(function(){var _lZ=E(_lP);return [0,[1,_lV,_lZ[1]],_lZ[2],_lZ[3],_lZ[4],_lZ[5]];}),_lQ];}),_m0=new T(function(){if(!E(_lR)){var _m1=E(E(_lX)[2]),_m2=[0,_m1[1],_lW,_m1[3],_m1[4],_m1[5]];}else{var _m3=E(E(_lX)[1]),_m2=[0,_m3[1],_lW,_m3[3],_m3[4],_m3[5]];}return _m2;});return [0,new T(function(){if(!E(_lR)){var _m4=[0,E(_lX)[1],_m0];}else{var _m4=[0,_m0,E(_lX)[2]];}return _m4;}),_lR,_hE,_lS,_lT];};if(!E(_lR)){var _m5=E(_lQ),_m6=E(_m5[2]);return _m6[0]==0?[0,[0,_lP,_m5],_5j,_hH,_lS,_lT]:B(_lU(_m6[1],_m6[2]));}else{var _m7=E(_lP),_m8=E(_m7[2]);return _m8[0]==0?[0,[0,_m7,_lQ],_5k,_hG,_lS,_lT]:B(_lU(_m8[1],_m8[2]));}},_m9=function(_ma){var _mb=E(_ma),_mc=E(_mb[1]),_md=B(_lO(_mc[1],_mc[2],_mb[2],_mb[4],_mb[5]));return [0,_md[1],_md[2],_md[3],_md[4],_md[5]];},_me=new T(function(){return B(unCStr("foldr1"));}),_mf=new T(function(){return B(_3B(_me));}),_mg=function(_mh,_mi){var _mj=E(_mi);if(!_mj[0]){return E(_mf);}else{var _mk=_mj[1],_ml=E(_mj[2]);if(!_ml[0]){return E(_mk);}else{return new F(function(){return A(_mh,[_mk,new T(function(){return B(_mg(_mh,_ml));})]);});}}},_jg=function(_mm,_mn,_mo,_){var _mp=rMV(_mm),_mq=_mp;if(!E(_mq)){return new F(function(){return A(_mg,[_lH,[1,function(_){var _=wMV(_mm,_5k);return _5i;},[1,function(_){var _mr=rMV(_mn),_ms=_mr,_=wMV(_mn,new T(function(){var _mt=E(_ms);return [0,_mt[1],new T(function(){return !E(_mt[2])?true:false;}),_lG,_mt[4],_mt[5]];})),_mu=rMV(_mn),_mv=_mu,_mw=B(_aD(_)),_mx=_mw,_my=B(A(_7A,[_mv,_mx,_])),_mz=_my;return _5i;},[1,function(_){var _mA=rMV(_mn),_mB=_mA,_=wMV(_mn,new T(function(){return B(_m9(_mB));})),_mC=rMV(_mn),_mD=_mC,_mE=B(_aD(_)),_mF=_mE,_mG=B(A(_7A,[_mD,_mF,_])),_mH=_mG;return _5i;},[1,function(_){var _mI=rMV(_mn),_mJ=_mI;if(!E(E(_mJ)[2])){var _mK=B(_ls([0,_mm],_mn,_)),_mL=_mK,_=wMV(_mm,_5j);return _5i;}else{var _=wMV(_mm,_5j);return _5i;}},_11]]]],_]);});}else{return _5i;}},_mM=function(_mN,_mO,_mP,_mQ,_){var _mR=rMV(_mQ),_mS=_mR,_=wMV(_mQ,new T(function(){return B(_iN(_mN,_mS));})),_mT=rMV(_mQ),_mU=_mT,_mV=B(_aD(_)),_mW=_mV,_mX=B(A(_7A,[_mU,_mW,_])),_mY=_mX,_mZ=rMV(_mQ),_n0=_mZ;switch(E(E(_n0)[3])[0]){case 3:var _n1=jsSetTimeout(1000,function(_){var _n2=rMV(_mQ),_n3=_n2,_=wMV(_mQ,new T(function(){var _n4=E(_n3);return [0,_n4[1],_n4[2],_aC,_n4[4],_n4[5]];})),_n5=rMV(_mQ),_n6=_n5,_n7=B(_aD(_)),_n8=_n7,_n9=B(A(_7A,[_n6,_n8,_])),_na=_n9,_nb=jsSetTimeout(1000,function(_){return new F(function(){return _jg(_mO,_mQ,_,_);});});return _5i;});return _5i;case 6:return new F(function(){return _jg(_mO,_mQ,_,_);});break;default:return _5i;}},_nc=function(_nd){return new F(function(){return _5t(E(_nd)[1]);});},_ne=function(_nf){return [0,[0,_nf],new T(function(){var _ng=E(_nf);if(_ng==13){var _nh=[0];}else{var _ni=B(_ne(_ng+1|0)),_nh=[1,_ni[1],_ni[2]];}return _nh;})];},_nj=new T(function(){var _nk=B(_ne(1));return [1,_nk[1],_nk[2]];}),_nl=function(_nm){return _nm>1?[0,_nj,new T(function(){var _nn=B(_nl(_nm-1|0));return [1,_nn[1],_nn[2]];})]:[0,_nj,_11];},_no=new T(function(){var _np=B(_nl(2));return B(_6C([1,_np[1],_np[2]]));}),_nq=new T(function(){return [0,B(_F(_no,0))];}),_nr=function(_ns,_nt,_nu){return new F(function(){return _fW(_ns,E(_nt)[1],_nu);});},_nv=function(_nw,_nx,_ny){return function(_nz){return new F(function(){return _gB(new T(function(){return B(_gZ(B(_1D(_bD,_nx))));}),B(_g3(B(_nr(_nw,new T(function(){return [0,E(_ny)[1]-1|0];}),_nz))))[1]);});};},_nA=new T(function(){return B(_nv(_cO,_no,_nq));}),_nB=function(_nC){return _nC>1?[0,_28,new T(function(){var _nD=B(_nB(_nC-1|0));return [1,_nD[1],_nD[2]];})]:[0,_28,_11];},_nE=new T(function(){var _nF=B(_nB(3));return [1,_nF[1],_nF[2]];}),_nG=function(_nH){return _nH>1?[0,_nE,new T(function(){var _nI=B(_nG(_nH-1|0));return [1,_nI[1],_nI[2]];})]:[0,_nE,_11];},_nJ=new T(function(){var _nK=B(_nG(5));return [1,_nK[1],_nK[2]];}),_nL=[0,63],_nM=[1,_nL,_11],_nN=function(_nO){return E(_nM);},_nP=new T(function(){return B(unCStr("computers"));}),_nQ=new T(function(){return B(unCStr("\u30b3\u30f3\u30d4\u30e5\u30fc\u30bf"));}),_nR=new T(function(){return B(unCStr("yours"));}),_nS=new T(function(){return B(unCStr("\u3042\u306a\u305f"));}),_nT=function(_nU,_nV){var _nW=E(_nU);if(!_nW){return [0];}else{var _nX=E(_nV);return _nX[0]==0?[0]:[1,_nX[1],new T(function(){return B(_nT(_nW-1|0,_nX[2]));})];}},_nY=function(_nZ,_o0,_o1){var _o2=new T(function(){return B(A(_nA,[_o0]));}),_o3=new T(function(){return B(A(_nA,[_nZ]));});return [0,[0,[0,new T(function(){return B(_nT(3,_o3));}),new T(function(){return B(_2D(3,_o3));}),_nS,_nR,_nc],[0,new T(function(){return B(_nT(3,_o2));}),new T(function(){return B(_2D(3,_o2));}),_nQ,_nP,_nN]],_5k,_lG,_nJ,_o1];},_o4=[0,0],_o5=0,_o6=new T(function(){return B(_dN(_o5));}),_o7=new T(function(){return die(_o6);}),_o8=function(_o9,_oa){var _ob=E(_oa);if(!_ob){return E(_dQ);}else{var _oc=function(_od){if(_o9<=0){if(_o9>=0){var _oe=quotRemI(_o9,_ob);return [0,[0,_oe[1]],[0,_oe[2]]];}else{if(_ob<=0){var _of=quotRemI(_o9,_ob);return [0,[0,_of[1]],[0,_of[2]]];}else{var _og=quotRemI(_o9+1|0,_ob);return [0,[0,_og[1]-1|0],[0,(_og[2]+_ob|0)-1|0]];}}}else{if(_ob>=0){if(_o9>=0){var _oh=quotRemI(_o9,_ob);return [0,[0,_oh[1]],[0,_oh[2]]];}else{if(_ob<=0){var _oi=quotRemI(_o9,_ob);return [0,[0,_oi[1]],[0,_oi[2]]];}else{var _oj=quotRemI(_o9+1|0,_ob);return [0,[0,_oj[1]-1|0],[0,(_oj[2]+_ob|0)-1|0]];}}}else{var _ok=quotRemI(_o9-1|0,_ob);return [0,[0,_ok[1]-1|0],[0,(_ok[2]+_ob|0)+1|0]];}}};return E(_ob)==(-1)?E(_o9)==(-2147483648)?[0,_o7,_o4]:B(_oc(_)):B(_oc(_));}},_ol=function(_om){var _on=B(_o8((_om>>>0&2147483647>>>0)>>>0&4.294967295e9,2147483562));return [0,E(_on[2])[1]+1|0,B(_es(E(_on[1])[1],2147483398))+1|0];},_oo=function(_){var _op=B(A(_17,["(function(){ return Math.floor(Math.random() * Math.pow(2, 53)); })",_])),_oq=_op;return new T(function(){var _or=jsTrunc(_oq),_os=_or,_ot=B(_ol(_os));return [0,_ot[1],_ot[2]];});},_ou=new T(function(){return [0,"click"];}),_ov=[8,_],_ow=function(_ox){var _oy=String(_ox),_oz=_oy;return new F(function(){return fromJSStr(_oz);});},_oA=function(_oB,_oC){while(1){var _oD=E(_oB);if(!_oD[0]){return E(_oC)[0]==0?true:false;}else{var _oE=E(_oC);if(!_oE[0]){return false;}else{if(E(_oD[1])[1]!=E(_oE[1])[1]){return false;}else{_oB=_oD[2];_oC=_oE[2];continue;}}}}},_oF=new T(function(){return B(unCStr("LI"));}),_oG=new T(function(){return B(_17("(function(e){ return e.tagName })"));}),_oH=new T(function(){return B(unCStr("wheel"));}),_oI=new T(function(){return B(unCStr("mouseout"));}),_oJ=new T(function(){return B(unCStr("mouseover"));}),_oK=new T(function(){return B(unCStr("mousemove"));}),_oL=new T(function(){return B(unCStr("blur"));}),_oM=new T(function(){return B(unCStr("focus"));}),_oN=new T(function(){return B(unCStr("change"));}),_oO=new T(function(){return B(unCStr("unload"));}),_oP=new T(function(){return B(unCStr("load"));}),_oQ=new T(function(){return B(unCStr("submit"));}),_oR=new T(function(){return B(unCStr("keydown"));}),_oS=new T(function(){return B(unCStr("keyup"));}),_oT=new T(function(){return B(unCStr("keypress"));}),_oU=new T(function(){return B(unCStr("mouseup"));}),_oV=new T(function(){return B(unCStr("mousedown"));}),_oW=new T(function(){return B(unCStr("dblclick"));}),_oX=new T(function(){return B(unCStr("click"));}),_oY=function(_oZ){switch(E(_oZ)[0]){case 0:return E(_oP);case 1:return E(_oO);case 2:return E(_oN);case 3:return E(_oM);case 4:return E(_oL);case 5:return E(_oK);case 6:return E(_oJ);case 7:return E(_oI);case 8:return E(_oX);case 9:return E(_oW);case 10:return E(_oV);case 11:return E(_oU);case 12:return E(_oT);case 13:return E(_oS);case 14:return E(_oR);case 15:return E(_oQ);default:return E(_oH);}},_p0=new T(function(){return E(0);}),_p1=new T(function(){return B(_17("(function(f) {  return (function() {      return (function(){        var args=Array.prototype.slice.call(arguments,0);        args.push(0);        return E(B(A(f, args)));    });  });})"));}),_p2=function(_p3,_){return new F(function(){return A(_p1,[E(_p3),_]);});},_p4=function(_p5,_){return new F(function(){return _p2(_p5,_);});},_p6=new T(function(){return B(_17("(function(node, evtName, f){ node.addEventListener(evtName, (function(e){ f(e.target) }))})"));}),_p7=function(_p8,_p9){return function(_pa,_){var _pb=E(_pa),_pc=B(A(_p6,[E(_pb[1]),E(toJSStr(E(new T(function(){return B(_oY(_p8));})))),E(new T(function(){return B(_13(function(_){var _=0;return new F(function(){return _p4(function(_pd){return new F(function(){return _13(function(_){var _=0,_pe=B(A(_p9,[[0,_pd],_])),_pf=_pe;return E(_p0);});});},_);});}));})),_])),_pg=_pc;return _pb;};},_ph=function(_pi){return new F(function(){return _p7(_ov,function(_pj,_){var _pk=E(_pj)[1],_pl=B(A(_oG,[E(_pk),_])),_pm=_pl;if(!B(_oA(B(_ow(_pm)),_oF))){return _5i;}else{var _pn=B(_2u(_6Y,_pk,_)),_po=_pn;return new F(function(){return A(_pi,[_po,_]);});}});});},_pp=new T(function(){return B(unCStr("TD"));}),_pq=function(_pr){return new F(function(){return _p7(_ov,function(_ps,_){var _pt=E(_ps)[1],_pu=E(_pt),_pv=B(A(_oG,[_pu,_])),_pw=_pv;if(!B(_oA(B(_ow(_pw)),_pp))){return _5i;}else{var _px=B(A(_6F,[_pu,_])),_py=_px,_pz=B(_2u(_6Y,_py,_)),_pA=_pz,_pB=B(_2u(_6Y,_pt,_)),_pC=_pB;return new F(function(){return A(_pr,[_pA,_pC,_]);});}});});},_pD=new T(function(){return B(unCStr("#field"));}),_pE=new T(function(){return B(unCStr("#yours ol.hand"));}),_pF=new T(function(){return B(unCStr("button#pass"));}),_pG=function(_){var _pH=B(_oo(_)),_pI=_pH,_pJ=B(_oo(_)),_pK=_pJ,_pL=B(_oo(_)),_pM=_pL,_pN=new T(function(){var _pO=B(_nY(_pI,_pK,_pM));return [0,_pO[1],_pO[2],_pO[3],_pO[4],_pO[5]];}),_pP=nMV(_pN),_pQ=_pP,_pR=nMV(_5j),_pS=_pR,_pT=B(_aD(_)),_pU=_pT,_pV=B(_1d(_pF,function(_pW,_){var _pX=E(_pW),_pY=jsSetCB(_pX[1],E(_ou)[1],function(_pZ,_q0,_){var _q1=rMV(_pQ),_q2=_q1;return !E(E(_q2)[2])?_5i:B(_mM(_jC,_pS,_,_pQ,_));}),_q3=_pY;return _pX;},_pU,_)),_q4=_pV,_q5=B(_1d(_pE,new T(function(){return B(_ph(function(_q6,_){var _q7=rMV(_pQ),_q8=_q7;return !E(E(_q8)[2])?_5i:B(_mM([0,_q6],_pS,_,_pQ,_));}));}),_pU,_)),_q9=_q5,_qa=B(_1d(_pD,new T(function(){return B(_pq(function(_qb,_qc,_){var _qd=rMV(_pQ),_qe=_qd;return !E(E(_qe)[2])?_5i:B(_mM([1,[0,_qb,_qc]],_pS,_,_pQ,_));}));}),_pU,_)),_qf=_qa,_qg=B(A(_7A,[_pN,_pU,_])),_qh=_qg,_qi=jsSetTimeout(1000,function(_){var _qj=rMV(_pQ),_qk=_qj,_=wMV(_pQ,new T(function(){return B(_m9(_qk));})),_ql=rMV(_pQ),_qm=_ql,_qn=B(_aD(_)),_qo=_qn,_qp=B(A(_7A,[_qm,_qo,_])),_qq=_qp;return _5i;});return _5i;},_qr=function(_){return new F(function(){return _pG(_);});};
var hasteMain = function() {B(A(_qr, [0]));};window.onload = hasteMain;