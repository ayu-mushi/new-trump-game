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

var _0=new T(function(){return B(unCStr("Prelude.(!!): negative index\n"));}),_1=new T(function(){return B(err(_0));}),_2=new T(function(){return B(unCStr("Prelude.(!!): index too large\n"));}),_3=new T(function(){return B(err(_2));}),_4=function(_5,_6){while(1){var _7=E(_5);if(!_7[0]){return E(_3);}else{var _8=E(_6);if(!_8){return E(_7[1]);}else{_5=_7[2];_6=_8-1|0;continue;}}}},_9=function(_a,_b){return E(_b);},_c=function(_d,_e,_){var _f=jsCreateTextNode(toJSStr(E(_d))),_g=_f,_h=jsAppendChild(_g,E(_e)[1]);return [0,_g];},_i=function(_j,_k,_){var _l=E(_j);if(!_l[0]){return _k;}else{var _m=B(A(_l[1],[_k,_])),_n=_m,_o=B(_i(_l[2],_k,_)),_p=_o;return _k;}},_q=new T(function(){return B(unCStr("\u30c9\u30ed\u30fc"));}),_r=new T(function(){return B(unCStr("\u3042\u306a\u305f\u306e\u52dd\u3061\u3067\u3059!"));}),_s=new T(function(){return B(unCStr("\u30b3\u30f3\u30d4\u30e5\u30fc\u30bf\u304c\u52dd\u3061!"));}),_t=new T(function(){return B(unCStr("\u624b\u756a\u3092\u4ea4\u4ee3"));}),_u=new T(function(){return B(unCStr("\u53ec\u559a\u3059\u308b\u4f4d\u7f6e\u3092\u9078\u629e"));}),_v=new T(function(){return B(unCStr("\u751f\u8d04\u3092\u9078\u629e: \u30a8\u30cd\u30eb\u30ae\u30fc\u304c\u3042\u3068"));}),_w=new T(function(){return B(unCStr("\u5fc5\u8981"));}),_x=new T(function(){return B(unCStr("\u79fb\u52d5\u3059\u308b\u4f4d\u7f6e\u3092\u9078\u629e"));}),_y=new T(function(){return B(unCStr("\u884c\u52d5\u3092\u9078\u629e"));}),_z=new T(function(){return B(unCStr("class"));}),_A=new T(function(){return B(unCStr("your-card"));}),_B=new T(function(){return B(unCStr("computers-card"));}),_C=[0],_D=new T(function(){return B(unCStr("td"));}),_E=function(_F,_G,_H,_){var _I=jsCreateElem(toJSStr(E(_D))),_J=_I,_K=jsAppendChild(_J,E(_H)[1]),_L=[0,_J],_M=B(A(_F,[_G,_L,_])),_N=_M;return _L;},_O=function(_P,_){return new F(function(){return _E(_c,_C,_P,_);});},_Q=[0,75],_R=[1,_Q,_C],_S=[0,81],_T=[1,_S,_C],_U=[0,74],_V=[1,_U,_C],_W=[0,65],_X=[1,_W,_C],_Y=function(_Z,_10){var _11=E(_Z);return _11[0]==0?E(_10):[1,_11[1],new T(function(){return B(_Y(_11[2],_10));})];},_12=function(_13,_14){var _15=jsShowI(_13),_16=_15;return new F(function(){return _Y(fromJSStr(_16),_14);});},_17=[0,41],_18=[0,40],_19=function(_1a,_1b,_1c){if(_1b>=0){return new F(function(){return _12(_1b,_1c);});}else{return _1a<=6?B(_12(_1b,_1c)):[1,_18,new T(function(){var _1d=jsShowI(_1b),_1e=_1d;return B(_Y(fromJSStr(_1e),[1,_17,_1c]));})];}},_1f=function(_1g){var _1h=E(_1g);switch(_1h){case 1:return E(_X);case 11:return E(_V);case 12:return E(_T);case 13:return E(_R);default:return new F(function(){return _19(0,_1h,_C);});}},_1i=0,_1j=function(_1k,_1l,_1m,_1n){return new F(function(){return A(_1k,[function(_){var _1o=jsSetAttr(E(_1l)[1],toJSStr(E(_1m)),toJSStr(E(_1n)));return _1i;}]);});},_1p=function(_1q){return E(_1q);},_1r=function(_1s){var _1t=E(_1s);if(!_1t[0]){return E(_O);}else{var _1u=E(_1t[1]);return function(_1v,_){var _1w=B(_E(_c,new T(function(){return B(_1f(E(_1u[2])[1]));}),_1v,_)),_1x=_1w,_1y=B(A(_1j,[_1p,_1x,_z,new T(function(){return !E(_1u[1])?E(_B):E(_A);}),_])),_1z=_1y;return _1x;};}},_1A=function(_1B,_1C){var _1D=E(_1C);return _1D[0]==0?[0]:[1,new T(function(){return B(A(_1B,[_1D[1]]));}),new T(function(){return B(_1A(_1B,_1D[2]));})];},_1E=new T(function(){return B(unCStr("tr"));}),_1F=function(_1G,_1H,_1I,_){var _1J=jsCreateElem(toJSStr(E(_1E))),_1K=_1J,_1L=jsAppendChild(_1K,E(_1I)[1]),_1M=[0,_1K],_1N=B(A(_1G,[_1H,_1M,_])),_1O=_1N;return _1M;},_1P=function(_1Q){return E(_1Q);},_1R=function(_1S){return function(_1T,_1U){return new F(function(){return _1F(_1P,function(_1V,_){return new F(function(){return _i(new T(function(){return B(_1A(_1r,_1S));}),_1V,_);});},_1T,_1U);});};},_1W=new T(function(){return B(unCStr("table#field"));}),_1X=new T(function(){return [0,"arr2lst"];}),_1Y=function(_1Z){var _20=B(A(_1Z,[_])),_21=_20;return E(_21);},_22=function(_23){return new F(function(){return _1Y(function(_){var _=0;return new F(function(){return eval(_23);});});});},_24=function(_25,_26){return new F(function(){return _1Y(function(_){var _=0;return new F(function(){return A(_22,[E(_1X)[1],E(_25),E(_26),_]);});});});},_27=new T(function(){return B(_22("(function(sel){return document.querySelectorAll(sel);})"));}),_28=function(_29,_2a,_2b,_){var _2c=B(A(_27,[E(toJSStr(E(_29))),_])),_2d=_2c,_2e=function(_2f,_){var _2g=E(_2f);if(!_2g[0]){return _C;}else{var _2h=B(A(_2a,[[0,_2g[1]],_])),_2i=_2h,_2j=B(_2e(_2g[2],_)),_2k=_2j;return [1,_2i,_2k];}},_2l=B(_2e(B(_24(_2d,0)),_)),_2m=_2l;return _2b;},_2n=function(_2o){return function(_1T,_1U){return new F(function(){return _28(_1W,function(_2p,_){var _2q=E(_2p),_2r=jsClearChildren(_2q[1]),_2s=B(_i(new T(function(){return B(_1A(_1R,_2o));}),_2q,_)),_2t=_2s;return _2q;},_1T,_1U);});};},_2u=new T(function(){return B(unCStr(" .deck"));}),_2v=new T(function(){return B(unCStr("\u306e\u6b8b\u308a\u5c71\u672d: "));}),_2w=[0,35],_2x=new T(function(){return B(unCStr(" .hand"));}),_2y=function(_2z,_2A){while(1){var _2B=E(_2z);if(!_2B[0]){return E(_2A);}else{_2z=_2B[2];var _2C=_2A+1|0;_2A=_2C;continue;}}},_2D=new T(function(){return B(unCStr("li"));}),_2E=function(_2F,_2G,_2H,_){var _2I=jsCreateElem(toJSStr(E(_2D))),_2J=_2I,_2K=jsAppendChild(_2J,E(_2H)[1]),_2L=[0,_2J],_2M=B(A(_2F,[_2G,_2L,_])),_2N=_2M;return _2L;},_2O=function(_2P){return function(_2Q,_){var _2R=B(_28([1,_2w,new T(function(){return B(_Y(E(_2P)[4],_2u));})],function(_2S,_){var _2T=E(_2S),_2U=jsClearChildren(_2T[1]),_2V=B(_c(new T(function(){var _2W=E(_2P);return B(_Y(_2W[3],new T(function(){return B(_Y(_2v,new T(function(){return B(_19(0,B(_2y(_2W[2],0)),_C));},1)));},1)));}),_2T,_)),_2X=_2V;return _2T;},_2Q,_)),_2Y=_2R,_2Z=B(_28([1,_2w,new T(function(){return B(_Y(E(_2P)[4],_2x));})],function(_30,_){var _31=E(_30),_32=jsClearChildren(_31[1]),_33=B(_i(new T(function(){var _34=E(_2P);return B(_1A(function(_35){return function(_1T,_1U){return new F(function(){return _2E(_c,new T(function(){return B(A(_34[5],[_35]));}),_1T,_1U);});};},_34[1]));}),_31,_)),_36=_33;return _31;},_2Q,_)),_37=_2Z;return _2Q;};},_38=new T(function(){return B(unCStr("Prelude.Enum.succ{Int}: tried to take `succ\' of maxBound"));}),_39=new T(function(){return B(err(_38));}),_3a=function(_3b){var _3c=E(E(_3b)[1]);return _3c==2147483647?E(_39):[0,_3c+1|0];},_3d=[0],_3e=new T(function(){return [0,"(function(e){ return e.previousSibling })"];}),_3f=new T(function(){return B(_22("(function(x) {return x === null})"));}),_3g=new T(function(){return B(_22("(function(node) {return node.nodeType === 1})"));}),_3h=function(_3i,_){var _3j=E(_3e)[1],_3k=B(A(_22,[_3j,E(_3i),_])),_3l=_3k,_3m=E(_3l),_3n=B(A(_3f,[_3m,_])),_3o=_3n;if(_3o<=0){var _3p=B(A(_3g,[_3m,_])),_3q=_3p;if(_3q<=0){return new F(function(){return (function(_3r,_){while(1){var _3s=B(A(_22,[_3j,E(_3r),_])),_3t=_3s,_3u=E(_3t),_3v=B(A(_3f,[_3u,_])),_3w=_3v;if(_3w<=0){var _3x=B(A(_3g,[_3u,_])),_3y=_3x;if(_3y<=0){_3r=_3t;continue;}else{return [1,[0,_3u]];}}else{return _3d;}}})(_3l,_);});}else{return [1,[0,_3m]];}}else{return _3d;}},_3z=function(_3A,_3B,_){while(1){var _3C=(function(_3D,_3E,_){var _3F=B(_3h(_3E,_)),_3G=_3F,_3H=E(_3G);if(!_3H[0]){return _3D;}else{_3A=new T(function(){return B(_3a(_3D));});_3B=E(_3H[1])[1];return null;}})(_3A,_3B,_);if(_3C!=null){return _3C;}}},_3I=function(_3J,_3K){while(1){var _3L=E(_3J);if(!_3L){return E(_3K);}else{var _3M=E(_3K);if(!_3M[0]){return [0];}else{_3J=_3L-1|0;_3K=_3M[2];continue;}}}},_3N=function(_3O,_3P,_3Q,_3R){return new F(function(){return A(_3P,[function(_3S){var _3T=E(_3O)[1],_3U=[1,_3S,new T(function(){var _3V=E(_3O)[1]+1|0;return _3V>=0?B(_3I(_3V,_3R)):E(_3R);})];if(_3T>0){var _3W=function(_3X,_3Y){var _3Z=E(_3X);if(!_3Z[0]){return E(_3U);}else{var _40=_3Z[1];return _3Y>1?[1,_40,new T(function(){return B(_3W(_3Z[2],_3Y-1|0));})]:[1,_40,_3U];}};return new F(function(){return _3W(_3R,_3T);});}else{return E(_3U);}},new T(function(){return B(A(_3Q,[new T(function(){var _41=E(_3O)[1];return _41>=0?B(_4(_3R,_41)):E(_1);})]));})]);});},_42=function(_43){return E(_43);},_44=function(_45){return E(E(_45)[1]);},_46=function(_47,_48,_49){while(1){var _4a=E(_49);if(!_4a[0]){return false;}else{if(!B(A(_44,[_47,_48,_4a[1]]))){_49=_4a[2];continue;}else{return true;}}}},_4b=function(_4c){var _4d=E(_4c);return [0,new T(function(){return [0,-1+E(_4d[1])[1]|0];}),_4d[2]];},_4e=[1,_4b,_C],_4f=[0,0],_4g=[0,1],_4h=[0,_4g,_4f],_4i=[1,_4h,_C],_4j=function(_4k,_4l){return [0,E(_4k)[1]+E(_4l)[1]|0];},_4m=function(_4n,_4o){var _4p=E(_4n),_4q=E(_4o);return [0,new T(function(){return B(_4j(_4p[1],_4q[1]));}),new T(function(){return B(_4j(_4p[2],_4q[2]));})];},_4r=new T(function(){return B(_1A(_4m,_4i));}),_4s=function(_4t,_4u,_4v,_4w,_4x,_4y){return !B(A(_4t,[_4v,_4x]))?true:!B(A(_44,[_4u,_4w,_4y]))?true:false;},_4z=function(_4A,_4B,_4C,_4D){var _4E=E(_4C),_4F=E(_4D);return new F(function(){return _4s(E(_4A)[1],_4B,_4E[1],_4E[2],_4F[1],_4F[2]);});},_4G=function(_4H,_4I,_4J,_4K,_4L,_4M){return !B(A(_4H,[_4J,_4L]))?false:B(A(_44,[_4I,_4K,_4M]));},_4N=function(_4O,_4P,_4Q,_4R){var _4S=E(_4Q),_4T=E(_4R);return new F(function(){return _4G(E(_4O)[1],_4P,_4S[1],_4S[2],_4T[1],_4T[2]);});},_4U=function(_4V,_4W){return [0,function(_4X,_4Y){return new F(function(){return _4N(_4V,_4W,_4X,_4Y);});},function(_4X,_4Y){return new F(function(){return _4z(_4V,_4W,_4X,_4Y);});}];},_4Z=function(_50,_51){return E(_50)[1]==E(_51)[1];},_52=function(_53,_54){return E(_53)[1]!=E(_54)[1];},_55=[0,_4Z,_52],_56=new T(function(){return B(_4U(_55,_55));}),_57=function(_58,_59,_5a,_5b,_5c){var _5d=B(_3N(_59,_9,function(_5e){return new F(function(){return _3N(_5a,_9,_42,_5e);});},new T(function(){return E(E(_58)[4]);})));if(!_5d[0]){return false;}else{var _5f=E(_58),_5g=function(_5h){return new F(function(){return A(_5h,[[0,_59,_5a]]);});},_5i=function(_5j){var _5k=B(_3N(_5b,_9,function(_5l){return new F(function(){return _3N(_5c,_9,_42,_5l);});},_5f[4]));if(!_5k[0]){return true;}else{var _5m=E(_5d[1]),_5n=_5m[1],_5o=E(_5m[2])[1],_5p=E(_5k[1]),_5q=E(_5p[2])[1];return _5o>=_5q?_5o!=_5q?!E(_5p[1])?E(_5n):!E(_5n)?true:false:false:false;}};return !E(_5f[2])?!B(_46(_56,[0,_5b,_5c],B(_1A(_5g,_4r))))?false:B(_5i(_)):!B(_46(_56,[0,_5b,_5c],B(_1A(_5g,_4e))))?false:B(_5i(_));}},_5r=function(_5s){return new F(function(){return err(B(unAppCStr("Oops!  Entered absent arg ",new T(function(){return B(unCStr(_5s));}))));});},_5t=new T(function(){return B(_5r("ww_s7Gy{v} [lid] random-1.1:System.Random.StdGen{tc rYH}"));}),_5u=new T(function(){return B(_5r("ww_s7Gw{v} [lid] main:Game.BoardTrump.GameState.Phase{tc r1WH}"));}),_5v=new T(function(){return B(_5r("ww_s7Gu{v} [lid] (main:Game.BoardTrump.Player.Player{tc r1Hl},\n                  main:Game.BoardTrump.Player.Player{tc r1Hl})"));}),_5w=function(_5x,_5y,_5z,_5A){var _5B=[0,_5v,_5x,_5u,_5y,_5t],_5C=function(_5D){while(1){var _5E=(function(_5F){var _5G=E(_5F);if(!_5G[0]){return [0];}else{var _5H=_5G[2],_5I=new T(function(){return B(A(_5G[1],[[0,_5z,_5A]]));});if(!B(_57(_5B,_5z,_5A,new T(function(){return E(E(_5I)[1]);}),new T(function(){return E(E(_5I)[2]);})))){_5D=_5H;return null;}else{return [1,_5I,new T(function(){return B(_5C(_5H));})];}}})(_5D);if(_5E!=null){return _5E;}}};return !E(_5x)?B(_5C(_4r)):B((function(_5J){var _5K=new T(function(){return [0,-1+E(_5z)[1]|0];});return !B(_57(_5B,_5z,_5A,_5K,_5A))?B(_5C(_5J)):[1,[0,_5K,_5A],new T(function(){return B(_5C(_5J));})];})(_C));},_5L=false,_5M=true,_5N=function(_5O,_){return _1i;},_5P=new T(function(){return B(unCStr("selectable-hand"));}),_5Q=new T(function(){return B(_22("(function(e,c,x){x?e.classList.add(c):e.classList.remove(c);})"));}),_5R=function(_){var _=0;return new F(function(){return A(_22,["false",_]);});},_5S=new T(function(){return B(_1Y(_5R));}),_5T=function(_){var _=0;return new F(function(){return A(_22,["true",_]);});},_5U=new T(function(){return B(_1Y(_5T));}),_5V=function(_5W){return function(_5X){return function(_5Y,_){var _5Z=B(A(new T(function(){return B(A(new T(function(){return B(A(_5Q,[E(E(_5W)[1])]));}),[E(toJSStr(E(_5X)))]));}),[!E(_5Y)?E(_5S):E(_5U),_])),_60=_5Z;return _1i;};};},_61=function(_62,_){while(1){var _63=E(_62);if(!_63[0]){return _1i;}else{var _64=B(A(_5V,[_63[1],_5P,_5M,_])),_65=_64;_62=_63[2];continue;}}},_66=new T(function(){return B(unCStr(": empty list"));}),_67=new T(function(){return B(unCStr("Prelude."));}),_68=function(_69){return new F(function(){return err(B(_Y(_67,new T(function(){return B(_Y(_69,_66));},1))));});},_6a=new T(function(){return B(unCStr("head"));}),_6b=new T(function(){return B(_68(_6a));}),_6c=function(_6d){var _6e=E(_6d);if(!_6e[0]){return [0];}else{return new F(function(){return _Y(_6e[1],new T(function(){return B(_6c(_6e[2]));},1));});}},_6f=new T(function(){return B(_22("(function(e){return e.parentNode;})"));}),_6g=new T(function(){return B(unCStr("td"));}),_6h=function(_6i,_6j){while(1){var _6k=(function(_6l,_6m){var _6n=E(_6m);if(!_6n[0]){return [0];}else{var _6o=_6n[1],_6p=_6n[2];if(!B(A(_6l,[_6o]))){var _6q=_6l;_6j=_6p;_6i=_6q;return null;}else{return [1,_6o,new T(function(){return B(_6h(_6l,_6p));})];}}})(_6i,_6j);if(_6k!=null){return _6k;}}},_6r=function(_6s,_6t,_6u,_6v){var _6w=E(_6u);if(!_6w[0]){return E(_6t);}else{var _6x=E(_6v);if(!_6x[0]){return E(_6t);}else{return new F(function(){return A(_6s,[_6w[1],_6x[1],new T(function(){return B(_6r(_6s,_6t,_6w[2],_6x[2]));})]);});}}},_6y=[0,0],_6z=function(_6A){return E(_6A)[0]==0?true:false;},_6B=function(_6C,_6D){while(1){var _6E=E(_6D);if(!_6E[0]){return E(_6C);}else{_6C=_6E[1];_6D=_6E[2];continue;}}},_6F=new T(function(){return B(unCStr("last"));}),_6G=new T(function(){return B(_68(_6F));}),_6H=new T(function(){return B(unCStr("#status"));}),_6I=[0,35],_6J=new T(function(){return B(unCStr("\u306e\u756a\u3067\u3059\u3001"));}),_6K=new T(function(){return B(unCStr(" ol.hand li"));}),_6L=function(_6M,_6N,_6O,_){if(!E(_6N)){return new F(function(){return A(_6O,[_]);});}else{var _6P=B(A(_5V,[_6M,_5P,_5M,_])),_6Q=_6P;return new F(function(){return A(_6O,[_]);});}},_6R=function(_){return _1i;},_6S=new T(function(){return B(unCStr("summonable-zone"));}),_6T=function(_6U,_6V,_6W,_){if(!E(_6V)){return new F(function(){return A(_6W,[_]);});}else{var _6X=B(A(_1j,[_1p,_6U,_z,_6S,_])),_6Y=_6X;return new F(function(){return A(_6W,[_]);});}},_6Z=new T(function(){return B(unCStr("#field tr"));}),_70=[0,0],_71=new T(function(){return B(unCStr("movable-card"));}),_72=new T(function(){return B(unCStr("id"));}),_73=new T(function(){return B(unCStr("moving-subject"));}),_74=new T(function(){return B(unCStr("motion-scope"));}),_75=new T(function(){return B(unCStr("obj-of-summon"));}),_76=function(_77,_){return _77;},_78=function(_79,_7a){while(1){var _7b=E(_7a);if(!_7b[0]){return E(_79);}else{var _7c=_79+E(_7b[1])[1]|0;_7a=_7b[2];_79=_7c;continue;}}},_7d=[0,2],_7e=function(_7f){return E(E(_7f)[1])==1?E(_7d):E(_4g);},_7g=function(_7h,_7i){var _7j=function(_7k){return E(_7h)==1?_7k<=B(_78(-2,B(_1A(_7e,_7i)))):_7k<=B(_78(-1,B(_1A(_7e,_7i))));};return _7h<=10?E(_7h)==1?B(_7j(114514)):B(_7j(0)):B(_7j(2));},_7l=function(_7m,_7n){return new F(function(){return _7g(E(_7m)[1],_7n);});},_7o=function(_7p){return function(_1T,_1U){return new F(function(){return _i([1,new T(function(){return B(_2n(new T(function(){return E(E(_7p)[4]);},1)));}),[1,function(_P,_){return new F(function(){return _28(_6H,function(_7q,_){var _7r=E(_7q),_7s=jsClearChildren(_7r[1]),_7t=B(A(new T(function(){var _7u=E(_7p),_7v=_7u[1],_7w=E(_7u[3]);if(_7w[0]==6){var _7x=function(_1T,_1U){return new F(function(){return _c(new T(function(){return !E(_7w[1])?E(_s):E(_r);}),_1T,_1U);});};}else{var _7x=function(_1T,_1U){return new F(function(){return _c(new T(function(){return B(unAppCStr("-- ",new T(function(){var _7y=new T(function(){return B(_Y(_6J,new T(function(){var _7z=E(_7w);switch(_7z[0]){case 0:var _7A=E(_q);break;case 1:var _7A=E(_y);break;case 2:var _7A=E(_x);break;case 3:var _7B=function(_7C){var _7D=E(_7C);return _7D[0]==0?E(new T(function(){return B(_Y(B(_19(0,E(_7z[1])[1],_C)),_w));})):[1,_7D[1],new T(function(){return B(_7B(_7D[2]));})];},_7A=B(_7B(_v));break;case 4:var _7A=E(_u);break;default:var _7A=E(_t);}return _7A;},1)));},1);if(!E(_7u[2])){var _7E=B(_Y(E(E(_7v)[2])[3],_7y));}else{var _7E=B(_Y(E(E(_7v)[1])[3],_7y));}return _7E;})));}),_1T,_1U);});};}var _7F=_7x;return _7F;}),[_7r,_])),_7G=_7t;return _7r;},_P,_);});},[1,function(_7H,_){var _7I=E(new T(function(){var _7J=E(E(_7p)[1]);return [0,new T(function(){return B(_2O(_7J[1]));}),new T(function(){return B(_2O(_7J[2]));})];})),_7K=B(A(_7I[1],[_7H,_])),_7L=_7K,_7M=B(A(_7I[2],[_7H,_])),_7N=_7M;return _7H;},[1,new T(function(){var _7O=E(_7p),_7P=_7O[1],_7Q=_7O[2],_7R=_7O[4],_7S=E(_7O[3]);switch(_7S[0]){case 1:var _7T=function(_7U){var _7V=E(_7U);if(!_7V[0]){return E(_5N);}else{var _7W=function(_7X){var _7Y=E(_7X);if(!_7Y[0]){return E(new T(function(){return B(_7T(_7V[2]));}));}else{var _7Z=new T(function(){return B(_7W(_7Y[2]));});return function(_80,_){var _81=E(_80);if(!_81[0]){return _1i;}else{var _82=_81[2],_83=E(_81[1]);if(!_83[0]){return new F(function(){return A(_7Z,[_82,_]);});}else{var _84=E(_7Y[1]),_85=_84[1],_86=B(A(_6f,[E(_85),_])),_87=_86,_88=B(_3z(_6y,_87,_)),_89=_88,_8a=B(_3z(_70,_85,_)),_8b=_8a;if(!E(E(_83[1])[1])){if(!E(_7Q)){if(!B(_5w(_5L,_7R,_89,_8b))[0]){return new F(function(){return A(_7Z,[_82,_]);});}else{var _8c=B(A(_5V,[_84,_71,_5M,_])),_8d=_8c;return new F(function(){return A(_7Z,[_82,_]);});}}else{return new F(function(){return A(_7Z,[_82,_]);});}}else{if(!E(_7Q)){return new F(function(){return A(_7Z,[_82,_]);});}else{if(!B(_5w(_5M,_7R,_89,_8b))[0]){return new F(function(){return A(_7Z,[_82,_]);});}else{var _8e=B(A(_5V,[_84,_71,_5M,_])),_8f=_8e;return new F(function(){return A(_7Z,[_82,_]);});}}}}}};}};return new F(function(){return _7W(_7V[1]);});}},_8g=function(_8h,_){var _8i=E(_8h),_8j=_8i[1],_8k=jsQuerySelectorAll(_8j,toJSStr([1,_6I,new T(function(){if(!E(_7Q)){var _8l=B(_Y(E(E(_7P)[2])[4],_6K));}else{var _8l=B(_Y(E(E(_7P)[1])[4],_6K));}return _8l;})])),_8m=_8k,_8n=B(A(_6r,[_6L,_6R,_8m,new T(function(){var _8o=new T(function(){if(!E(_7Q)){var _8p=E(E(E(_7P)[2])[1]);}else{var _8p=E(E(E(_7P)[1])[1]);}return _8p;});if(!E(_7Q)){var _8q=B(_1A(function(_8r){return new F(function(){return _7l(_8r,_8o);});},E(E(_7P)[2])[1]));}else{var _8q=B(_1A(function(_8s){return new F(function(){return _7l(_8s,_8o);});},E(E(_7P)[1])[1]));}return _8q;}),_])),_8t=_8n,_8u=jsQuerySelectorAll(_8j,toJSStr(E(_6Z))),_8v=_8u,_8w=E(_8v);if(!_8w[0]){return _8i;}else{var _8x=E(_6g),_8y=jsQuerySelectorAll(E(_8w[1])[1],toJSStr(_8x)),_8z=_8y,_8A=function(_8B,_){var _8C=E(_8B);if(!_8C[0]){return _C;}else{var _8D=jsQuerySelectorAll(E(_8C[1])[1],toJSStr(_8x)),_8E=_8D,_8F=B(_8A(_8C[2],_)),_8G=_8F;return [1,_8E,_8G];}},_8H=B(_8A(_8w[2],_)),_8I=_8H,_8J=B(A(function(_8K,_8L){var _8M=function(_8N){var _8O=E(_8N);if(!_8O[0]){return E(new T(function(){return B(_7T(_8L));}));}else{var _8P=new T(function(){return B(_8M(_8O[2]));});return function(_8Q,_){var _8R=E(_8Q);if(!_8R[0]){return _1i;}else{var _8S=_8R[2],_8T=E(_8R[1]);if(!_8T[0]){return new F(function(){return A(_8P,[_8S,_]);});}else{var _8U=E(_8O[1]),_8V=_8U[1],_8W=B(A(_6f,[E(_8V),_])),_8X=_8W,_8Y=B(_3z(_6y,_8X,_)),_8Z=_8Y,_90=B(_3z(_70,_8V,_)),_91=_90;if(!E(E(_8T[1])[1])){if(!E(_7Q)){if(!B(_5w(_5L,_7R,_8Z,_91))[0]){return new F(function(){return A(_8P,[_8S,_]);});}else{var _92=B(A(_5V,[_8U,_71,_5M,_])),_93=_92;return new F(function(){return A(_8P,[_8S,_]);});}}else{return new F(function(){return A(_8P,[_8S,_]);});}}else{if(!E(_7Q)){return new F(function(){return A(_8P,[_8S,_]);});}else{if(!B(_5w(_5M,_7R,_8Z,_91))[0]){return new F(function(){return A(_8P,[_8S,_]);});}else{var _94=B(A(_5V,[_8U,_71,_5M,_])),_95=_94;return new F(function(){return A(_8P,[_8S,_]);});}}}}}};}};return new F(function(){return _8M(_8K);});},[_8z,_8I,new T(function(){return B(_6c(_7R));}),_])),_96=_8J;return _8i;}};break;case 2:var _97=_7S[1],_8g=function(_98,_){var _99=E(_98),_9a=jsQuerySelectorAll(_99[1],toJSStr(E(_6Z))),_9b=_9a,_9c=_9b,_9d=E(_97),_9e=E(_9d[1])[1];if(_9e>=0){var _9f=E(_6g),_9g=jsQuerySelectorAll(B(_4(_9c,_9e))[1],toJSStr(_9f)),_9h=_9g,_9i=E(_9d[2])[1];if(_9i>=0){var _9j=jsSetAttr(B(_4(_9h,_9i))[1],toJSStr(E(_72)),toJSStr(E(_73))),_9k=function(_,_9l){var _9m=B((function(_9n,_){while(1){var _9o=(function(_9p,_){var _9q=E(_9p);if(!_9q[0]){return _1i;}else{var _9r=_9q[1],_9s=B(A(_5V,[new T(function(){return B(_3N(new T(function(){return E(B(A(_9r,[_9d]))[1]);}),_9,function(_9t){return new F(function(){return _3N(new T(function(){return E(B(A(_9r,[_9d]))[2]);}),_9,_42,_9t);});},_9l));},1),_74,_5M,_])),_9u=_9s;_9n=_9q[2];return null;}})(_9n,_);if(_9o!=null){return _9o;}}})(new T(function(){var _9v=function(_9w){var _9x=new T(function(){return B(A(_9w,[_97]));});return new F(function(){return _57(_7O,new T(function(){return E(E(_97)[1]);}),new T(function(){return E(E(_97)[2]);}),new T(function(){return E(E(_9x)[1]);}),new T(function(){return E(E(_9x)[2]);}));});};return !E(_7Q)?B(_6h(_9v,_4r)):B(_6h(_9v,_4e));}),_)),_9y=_9m;return _99;},_9z=E(_9c);if(!_9z[0]){return new F(function(){return _9k(_,_C);});}else{var _9A=jsQuerySelectorAll(E(_9z[1])[1],toJSStr(_9f)),_9B=_9A,_9C=function(_9D,_){var _9E=E(_9D);if(!_9E[0]){return _C;}else{var _9F=jsQuerySelectorAll(E(_9E[1])[1],toJSStr(_9f)),_9G=_9F,_9H=B(_9C(_9E[2],_)),_9I=_9H;return [1,_9G,_9I];}},_9J=B(_9C(_9z[2],_)),_9K=_9J;return new F(function(){return _9k(_,[1,_9B,_9K]);});}}else{return E(_1);}}else{return E(_1);}};break;case 3:var _8g=function(_9L,_){var _9M=E(_9L),_9N=jsQuerySelectorAll(_9M[1],toJSStr([1,_6I,new T(function(){if(!E(_7Q)){var _9O=B(_Y(E(E(_7P)[2])[4],_6K));}else{var _9O=B(_Y(E(E(_7P)[1])[4],_6K));}return _9O;})])),_9P=_9N,_9Q=B(_61(_9P,_)),_9R=_9Q;return _9M;};break;case 4:var _8g=function(_9S,_){var _9T=E(_9S),_9U=_9T[1],_9V=jsQuerySelectorAll(_9U,toJSStr([1,_6I,new T(function(){if(!E(_7Q)){var _9W=B(_Y(E(E(_7P)[2])[4],_6K));}else{var _9W=B(_Y(E(E(_7P)[1])[4],_6K));}return _9W;})])),_9X=_9V,_9Y=E(_7S[1])[1];if(_9Y>=0){var _9Z=jsSetAttr(B(_4(_9X,_9Y))[1],toJSStr(E(_72)),toJSStr(E(_75))),_a0=jsQuerySelectorAll(_9U,toJSStr(E(_6Z))),_a1=_a0,_a2=_a1,_a3=function(_a4){var _a5=jsQuerySelectorAll(_a4,toJSStr(E(_6g))),_a6=_a5,_a7=B(A(_6r,[_6T,_6R,_a6,new T(function(){if(!E(_7Q)){var _a8=E(_7R),_a9=_a8[0]==0?E(_6b):B(_1A(_6z,_a8[1]));}else{var _aa=E(_7R);if(!_aa[0]){var _ab=E(_6G);}else{var _ab=B(_1A(_6z,B(_6B(_aa[1],_aa[2]))));}var _a9=_ab;}return _a9;}),_])),_ac=_a7;return _9T;};if(!E(_7Q)){var _ad=E(_a2);if(!_ad[0]){return E(_6b);}else{return new F(function(){return _a3(E(_ad[1])[1]);});}}else{var _ae=E(_a2);if(!_ae[0]){return E(_6G);}else{return new F(function(){return _a3(B(_6B(_ae[1],_ae[2]))[1]);});}}}else{return E(_1);}};break;default:var _8g=E(_76);}var _af=_8g;return _af;}),_C]]]],_1T,_1U);});};},_ag=function(_ah){return new F(function(){return _1f(E(_ah)[1]);});},_ai=[0],_aj=[0,2147483562],_ak=[0,1],_al=[0,_ak,_aj],_am=function(_an){return E(_al);},_ao=function(_ap,_aq){return [0,E(_ap)[1],E(_aq)[1]];},_ar=function(_as,_at){var _au=quot(_at,52774),_av=(imul(40692,_at-(imul(_au,52774)|0)|0)|0)-(imul(_au,3791)|0)|0,_aw=new T(function(){if(_av>=0){var _ax=[0,_av];}else{var _ax=[0,_av+2147483399|0];}var _ay=_ax;return _ay;}),_az=quot(_as,53668),_aA=(imul(40014,_as-(imul(_az,53668)|0)|0)|0)-(imul(_az,12211)|0)|0,_aB=new T(function(){if(_aA>=0){var _aC=[0,_aA];}else{var _aC=[0,_aA+2147483563|0];}var _aD=_aC;return _aD;});return [0,new T(function(){var _aE=E(_aB)[1]-E(_aw)[1]|0;if(_aE>=1){var _aF=[0,_aE];}else{var _aF=[0,_aE+2147483562|0];}var _aG=_aF,_aH=_aG,_aI=_aH,_aJ=_aI;return _aJ;}),new T(function(){return B(_ao(_aB,_aw));})];},_aK=function(_aL){var _aM=E(_aL),_aN=B(_ar(_aM[1],_aM[2]));return [0,_aN[1],_aN[2]];},_aO=function(_aP,_aQ){var _aR=new T(function(){return E(B(_ar(_aP,_aQ))[2]);});return [0,new T(function(){var _aS=E(_aP);if(_aS==2147483562){var _aT=[0,1,E(_aR)[2]];}else{var _aT=[0,_aS+1|0,E(_aR)[2]];}return _aT;}),new T(function(){var _aU=E(_aR)[1],_aV=E(_aQ);if(_aV==1){var _aW=[0,_aU,2147483398];}else{var _aW=[0,_aU,_aV-1|0];}var _aX=_aW;return _aX;})];},_aY=function(_aZ){var _b0=E(_aZ),_b1=B(_aO(_b0[1],_b0[2]));return [0,_b1[1],_b1[2]];},_b2=[0,_aK,_am,_aY],_b3=function(_b4){return [0,[0,_b4],new T(function(){var _b5=E(_b4);if(_b5==13){var _b6=[0];}else{var _b7=B(_b3(_b5+1|0)),_b6=[1,_b7[1],_b7[2]];}return _b6;})];},_b8=new T(function(){var _b9=B(_b3(1));return [1,_b9[1],_b9[2]];}),_ba=function(_bb){return _bb>1?[0,_b8,new T(function(){var _bc=B(_ba(_bb-1|0));return [1,_bc[1],_bc[2]];})]:[0,_b8,_C];},_bd=new T(function(){var _be=B(_ba(2));return B(_6c([1,_be[1],_be[2]]));}),_bf=new T(function(){return [0,B(_2y(_bd,0))];}),_bg=function(_bh){return [0,E(E(_bh))];},_bi=function(_bj){var _bk=E(_bj);if(!_bk[0]){return [0,_C,_C];}else{var _bl=E(_bk[1]),_bm=new T(function(){var _bn=B(_bi(_bk[2]));return [0,_bn[1],_bn[2]];});return [0,[1,_bl[1],new T(function(){return E(E(_bm)[1]);})],[1,_bl[2],new T(function(){return E(E(_bm)[2]);})]];}},_bo=function(_bp,_bq){return [0,imul(E(_bp)[1],E(_bq)[1])|0];},_br=function(_bs,_bt){return [0,E(_bs)[1]-E(_bt)[1]|0];},_bu=function(_bv){var _bw=E(_bv),_bx=_bw[1];return _bx<0?[0, -_bx]:E(_bw);},_by=function(_bz){var _bA=E(_bz);return _bA[0]==0?E(_bA[1]):I_toInt(_bA[1]);},_bB=function(_bC){return [0,B(_by(_bC))];},_bD=function(_bE){return [0, -E(_bE)[1]];},_bF=[0,-1],_bG=[0,0],_bH=[0,1],_bI=function(_bJ){var _bK=E(_bJ)[1];return _bK>=0?E(_bK)==0?E(_bG):E(_bH):E(_bF);},_bL=[0,_4j,_bo,_br,_bD,_bu,_bI,_bB],_bM=[0,1],_bN=new T(function(){return B(unCStr("base"));}),_bO=new T(function(){return B(unCStr("GHC.Exception"));}),_bP=new T(function(){return B(unCStr("ArithException"));}),_bQ=[0,I_fromBits([4194982440,719304104]),I_fromBits([3110813675,1843557400]),_bN,_bO,_bP],_bR=[0,I_fromBits([4194982440,719304104]),I_fromBits([3110813675,1843557400]),_bQ,_C],_bS=function(_bT){return E(_bR);},_bU=function(_bV){return E(E(_bV)[1]);},_bW=function(_bX,_bY,_bZ){var _c0=B(A(_bX,[_])),_c1=B(A(_bY,[_])),_c2=hs_eqWord64(_c0[1],_c1[1]),_c3=_c2;if(!E(_c3)){return [0];}else{var _c4=hs_eqWord64(_c0[2],_c1[2]),_c5=_c4;return E(_c5)==0?[0]:[1,_bZ];}},_c6=function(_c7){var _c8=E(_c7);return new F(function(){return _bW(B(_bU(_c8[1])),_bS,_c8[2]);});},_c9=new T(function(){return B(unCStr("arithmetic underflow"));}),_ca=new T(function(){return B(unCStr("arithmetic overflow"));}),_cb=new T(function(){return B(unCStr("Ratio has zero denominator"));}),_cc=new T(function(){return B(unCStr("denormal"));}),_cd=new T(function(){return B(unCStr("divide by zero"));}),_ce=new T(function(){return B(unCStr("loss of precision"));}),_cf=function(_cg){switch(E(_cg)){case 0:return E(_ca);case 1:return E(_c9);case 2:return E(_ce);case 3:return E(_cd);case 4:return E(_cc);default:return E(_cb);}},_ch=function(_ci){return new F(function(){return _Y(_c9,_ci);});},_cj=function(_ci){return new F(function(){return _Y(_ca,_ci);});},_ck=function(_ci){return new F(function(){return _Y(_cb,_ci);});},_cl=function(_ci){return new F(function(){return _Y(_cc,_ci);});},_cm=function(_ci){return new F(function(){return _Y(_cd,_ci);});},_cn=function(_ci){return new F(function(){return _Y(_ce,_ci);});},_co=function(_cp){switch(E(_cp)){case 0:return E(_cj);case 1:return E(_ch);case 2:return E(_cn);case 3:return E(_cm);case 4:return E(_cl);default:return E(_ck);}},_cq=[0,44],_cr=[0,93],_cs=[0,91],_ct=function(_cu,_cv,_cw){var _cx=E(_cv);return _cx[0]==0?B(unAppCStr("[]",_cw)):[1,_cs,new T(function(){return B(A(_cu,[_cx[1],new T(function(){var _cy=function(_cz){var _cA=E(_cz);return _cA[0]==0?E([1,_cr,_cw]):[1,_cq,new T(function(){return B(A(_cu,[_cA[1],new T(function(){return B(_cy(_cA[2]));})]));})];};return B(_cy(_cx[2]));})]));})];},_cB=function(_cC,_cD){return new F(function(){return _ct(_co,_cC,_cD);});},_cE=function(_cF,_cG){switch(E(_cG)){case 0:return E(_cj);case 1:return E(_ch);case 2:return E(_cn);case 3:return E(_cm);case 4:return E(_cl);default:return E(_ck);}},_cH=[0,_cE,_cf,_cB],_cI=new T(function(){return [0,_bS,_cH,_cJ,_c6];}),_cJ=function(_ci){return [0,_cI,_ci];},_cK=3,_cL=new T(function(){return B(_cJ(_cK));}),_cM=new T(function(){return die(_cL);}),_cN=function(_cO,_cP){var _cQ=E(_cO);if(!_cQ[0]){var _cR=_cQ[1],_cS=E(_cP);return _cS[0]==0?_cR==_cS[1]:I_compareInt(_cS[1],_cR)==0?true:false;}else{var _cT=_cQ[1],_cU=E(_cP);return _cU[0]==0?I_compareInt(_cT,_cU[1])==0?true:false:I_compare(_cT,_cU[1])==0?true:false;}},_cV=function(_cW){return E(E(_cW)[7]);},_cX=function(_cY,_cZ){var _d0=E(_cY);if(!_d0[0]){var _d1=_d0[1],_d2=E(_cZ);return _d2[0]==0?_d1>=_d2[1]:I_compareInt(_d2[1],_d1)<=0;}else{var _d3=_d0[1],_d4=E(_cZ);return _d4[0]==0?I_compareInt(_d3,_d4[1])>=0:I_compare(_d3,_d4[1])>=0;}},_d5=function(_d6){return E(E(_d6)[2]);},_d7=[0,0],_d8=function(_d9,_da){var _db=E(_d9);if(!_db[0]){var _dc=_db[1],_dd=E(_da);return _dd[0]==0?_dc>_dd[1]:I_compareInt(_dd[1],_dc)<0;}else{var _de=_db[1],_df=E(_da);return _df[0]==0?I_compareInt(_de,_df[1])>0:I_compare(_de,_df[1])>0;}},_dg=[0,1000],_dh=function(_di,_dj){while(1){var _dk=E(_di);if(!_dk[0]){var _dl=_dk[1],_dm=E(_dj);if(!_dm[0]){var _dn=_dm[1],_do=subC(_dl,_dn);if(!E(_do[2])){return [0,_do[1]];}else{_di=[1,I_fromInt(_dl)];_dj=[1,I_fromInt(_dn)];continue;}}else{_di=[1,I_fromInt(_dl)];_dj=_dm;continue;}}else{var _dp=E(_dj);if(!_dp[0]){_di=_dk;_dj=[1,I_fromInt(_dp[1])];continue;}else{return [1,I_sub(_dk[1],_dp[1])];}}}},_dq=function(_dr,_ds){var _dt=_dr%_ds;if(_dr<=0){if(_dr>=0){return E(_dt);}else{if(_ds<=0){return E(_dt);}else{var _du=E(_dt);return _du==0?0:_du+_ds|0;}}}else{if(_ds>=0){if(_dr>=0){return E(_dt);}else{if(_ds<=0){return E(_dt);}else{var _dv=E(_dt);return _dv==0?0:_dv+_ds|0;}}}else{var _dw=E(_dt);return _dw==0?0:_dw+_ds|0;}}},_dx=function(_dy,_dz){while(1){var _dA=E(_dy);if(!_dA[0]){var _dB=E(_dA[1]);if(_dB==(-2147483648)){_dy=[1,I_fromInt(-2147483648)];continue;}else{var _dC=E(_dz);if(!_dC[0]){return [0,B(_dq(_dB,_dC[1]))];}else{_dy=[1,I_fromInt(_dB)];_dz=_dC;continue;}}}else{var _dD=_dA[1],_dE=E(_dz);return _dE[0]==0?[0,I_toInt(I_mod(_dD,I_fromInt(_dE[1])))]:[1,I_mod(_dD,_dE[1])];}}},_dF=function(_dG){return E(E(_dG)[1]);},_dH=function(_dI,_dJ){while(1){var _dK=E(_dI);if(!_dK[0]){var _dL=_dK[1],_dM=E(_dJ);if(!_dM[0]){var _dN=_dM[1],_dO=addC(_dL,_dN);if(!E(_dO[2])){return [0,_dO[1]];}else{_dI=[1,I_fromInt(_dL)];_dJ=[1,I_fromInt(_dN)];continue;}}else{_dI=[1,I_fromInt(_dL)];_dJ=_dM;continue;}}else{var _dP=E(_dJ);if(!_dP[0]){_dI=_dK;_dJ=[1,I_fromInt(_dP[1])];continue;}else{return [1,I_add(_dK[1],_dP[1])];}}}},_dQ=function(_dR){return [0,_dR];},_dS=function(_dT,_dU){while(1){var _dV=E(_dT);if(!_dV[0]){var _dW=_dV[1],_dX=E(_dU);if(!_dX[0]){var _dY=_dX[1];if(!(imul(_dW,_dY)|0)){return [0,imul(_dW,_dY)|0];}else{_dT=[1,I_fromInt(_dW)];_dU=[1,I_fromInt(_dY)];continue;}}else{_dT=[1,I_fromInt(_dW)];_dU=_dX;continue;}}else{var _dZ=E(_dU);if(!_dZ[0]){_dT=_dV;_dU=[1,I_fromInt(_dZ[1])];continue;}else{return [1,I_mul(_dV[1],_dZ[1])];}}}},_e0=function(_e1,_e2,_e3,_e4,_e5){while(1){var _e6=(function(_e7,_e8,_e9,_ea,_eb){if(!B(_d8(_e9,_ea))){var _ec=B(_dH(B(_dh(_ea,_e9)),_bM)),_ed=new T(function(){return B(A(_d5,[_e7,_eb]));}),_ee=new T(function(){return E(E(_ed)[1]);}),_ef=new T(function(){return B(_dH(B(_dh(B(_dQ(E(E(_ed)[2])[1])),B(_dQ(E(_ee)[1])))),_bM));}),_eg=B((function(_eh,_ei,_ej){while(1){if(!B(_cX(_eh,B(_dS(_ec,_dg))))){var _ek=B(A(new T(function(){return B(_dF(_e7));}),[_ej])),_el=B(_dS(_eh,_ef)),_em=B(_dH(B(_dS(_ei,_ef)),B(_dh(B(_dQ(E(_ek[1])[1])),new T(function(){return B(_dQ(E(_ee)[1]));})))));_ej=_ek[2];_eh=_el;_ei=_em;continue;}else{return [0,_ei,_ej];}}})(_bM,_d7,_eb));return [0,new T(function(){return B(A(_cV,[_e8,new T(function(){if(!B(_cN(_ec,_d7))){var _en=B(_dH(_e9,B(_dx(_eg[1],_ec))));}else{var _en=E(_cM);}return _en;})]));}),_eg[2]];}else{var _eo=_e7,_ep=_e8,_eq=_ea,_er=_e9,_es=_eb;_e1=_eo;_e2=_ep;_e3=_eq;_e4=_er;_e5=_es;return null;}})(_e1,_e2,_e3,_e4,_e5);if(_e6!=null){return _e6;}}},_et=[0,0],_eu=function(_ev,_ew,_ex){var _ey=E(_ew);if(!_ey){return [0];}else{var _ez=new T(function(){var _eA=B(_e0(_ev,_bL,_et,B(_dQ(_ey)),_ex));return [0,_eA[1],_eA[2]];});return [1,[0,new T(function(){return E(E(_ez)[1]);}),_ex],new T(function(){return B(_eu(_ev,_ey-1|0,new T(function(){return E(E(_ez)[2]);})));})];}},_eB=function(_eC,_eD,_eE){return new F(function(){return _eu(_eC,E(_eD)[1],_eE);});},_eF=new T(function(){return B(unCStr("[extractTree] impossible"));}),_eG=new T(function(){return B(err(_eF));}),_eH=function(_eI,_eJ){var _eK=function(_eL){var _eM=E(_eJ);if(!_eM[0]){return E(_eG);}else{var _eN=_eM[1],_eO=_eM[3],_eP=E(_eM[2]);if(!_eP[0]){var _eQ=new T(function(){var _eR=B(_eH(_eI-1|0,_eO));return [0,_eR[1],_eR[2]];});return [0,new T(function(){return E(E(_eQ)[1]);}),new T(function(){return [1,_eN-1|0,E(_eP),E(E(E(_eQ)[2]))];})];}else{var _eS=_eP[1],_eT=function(_eU){if(_eI>=_eS){var _eV=new T(function(){var _eW=B(_eH(_eI-_eS|0,_eO));return [0,_eW[1],_eW[2]];});return [0,new T(function(){return E(E(_eV)[1]);}),new T(function(){return [1,_eN-1|0,E(_eP),E(E(E(_eV)[2]))];})];}else{var _eX=new T(function(){var _eY=B(_eH(_eI,_eP));return [0,_eY[1],_eY[2]];});return [0,new T(function(){return E(E(_eX)[1]);}),new T(function(){return [1,_eN-1|0,E(E(E(_eX)[2])),E(_eO)];})];}},_eZ=E(_eO);if(!_eZ[0]){return (_eI+1|0)!=_eN?B(_eT(_)):[0,_eZ[1],_eP];}else{return new F(function(){return _eT(_);});}}}};switch(E(_eI)){case 0:var _f0=E(_eJ);if(!_f0[0]){return new F(function(){return _eK(_);});}else{var _f1=E(_f0[2]);return _f1[0]==0?[0,_f1[1],_f0[3]]:B(_eK(_));}break;case 1:var _f2=E(_eJ);if(!_f2[0]){return new F(function(){return _eK(_);});}else{if(E(_f2[1])==2){var _f3=E(_f2[2]);if(!_f3[0]){var _f4=E(_f2[3]);return _f4[0]==0?[0,_f4[1],_f3]:B(_eK(_));}else{return new F(function(){return _eK(_);});}}else{return new F(function(){return _eK(_);});}}break;default:return new F(function(){return _eK(_);});}},_f5=new T(function(){return B(unCStr("[shuffle] called with lists of different lengths"));}),_f6=new T(function(){return B(err(_f5));}),_f7=function(_f8,_f9){var _fa=function(_fb){var _fc=E(_f9);if(!_fc[0]){return E(_f6);}else{var _fd=new T(function(){var _fe=B(_eH(E(_fc[1])[1],_f8));return [0,_fe[1],_fe[2]];});return [1,new T(function(){return E(E(_fd)[1]);}),new T(function(){return B(_f7(E(_fd)[2],_fc[2]));})];}},_ff=E(_f8);return _ff[0]==0?E(_f9)[0]==0?[1,_ff[1],_C]:B(_fa(_)):B(_fa(_));},_fg=function(_fh){var _fi=E(_fh);if(!_fi[0]){return [0];}else{var _fj=_fi[1],_fk=E(_fi[2]);if(!_fk[0]){return [1,_fj,_C];}else{var _fl=E(_fk[1]);return [1,new T(function(){var _fm=E(E(_fj));if(!_fm[0]){var _fn=E(_fl);if(!_fn[0]){var _fo=[1,2,E(_fm),E(_fn)];}else{var _fo=[1,_fn[1]+1|0,E(_fm),E(_fn)];}var _fp=_fo;}else{var _fq=_fm[1],_fr=E(_fl);if(!_fr[0]){var _fs=[1,_fq+1|0,E(_fm),E(_fr)];}else{var _fs=[1,_fq+_fr[1]|0,E(_fm),E(_fr)];}var _fp=_fs;}return _fp;}),new T(function(){return B(_fg(_fk[2]));})];}}},_ft=new T(function(){return B(_fg(_C));}),_fu=new T(function(){return B(_fv(_ft));}),_fv=function(_fw){while(1){var _fx=E(_fw);if(!_fx[0]){return E(_fu);}else{if(!E(_fx[2])[0]){return E(_fx[1]);}else{_fw=B(_fg(_fx));continue;}}}},_fy=function(_fz,_fA,_fB){return function(_fC){return new F(function(){return _f7(new T(function(){return B(_fv(B(_1A(_bg,_fA))));}),B(_bi(B(_eB(_fz,new T(function(){return [0,E(_fB)[1]-1|0];}),_fC))))[1]);});};},_fD=new T(function(){return B(_fy(_b2,_bd,_bf));}),_fE=function(_fF){return _fF>1?[0,_3d,new T(function(){var _fG=B(_fE(_fF-1|0));return [1,_fG[1],_fG[2]];})]:[0,_3d,_C];},_fH=new T(function(){var _fI=B(_fE(3));return [1,_fI[1],_fI[2]];}),_fJ=function(_fK){return _fK>1?[0,_fH,new T(function(){var _fL=B(_fJ(_fK-1|0));return [1,_fL[1],_fL[2]];})]:[0,_fH,_C];},_fM=new T(function(){var _fN=B(_fJ(5));return [1,_fN[1],_fN[2]];}),_fO=[0,63],_fP=[1,_fO,_C],_fQ=function(_fR){return E(_fP);},_fS=new T(function(){return B(unCStr("computers"));}),_fT=new T(function(){return B(unCStr("\u30b3\u30f3\u30d4\u30e5\u30fc\u30bf"));}),_fU=new T(function(){return B(unCStr("yours"));}),_fV=new T(function(){return B(unCStr("\u3042\u306a\u305f"));}),_fW=function(_fX,_fY){var _fZ=E(_fX);if(!_fZ){return [0];}else{var _g0=E(_fY);return _g0[0]==0?[0]:[1,_g0[1],new T(function(){return B(_fW(_fZ-1|0,_g0[2]));})];}},_g1=function(_g2,_g3,_g4){var _g5=new T(function(){return B(A(_fD,[_g3]));}),_g6=new T(function(){return B(A(_fD,[_g2]));});return [0,[0,[0,new T(function(){return B(_fW(3,_g6));}),new T(function(){return B(_3I(3,_g6));}),_fV,_fU,_ag],[0,new T(function(){return B(_fW(3,_g5));}),new T(function(){return B(_3I(3,_g5));}),_fT,_fS,_fQ]],_5M,_ai,_fM,_g4];},_g7=function(_){var _g8=B(A(_22,["(function(){return document.body;})",_])),_g9=_g8;return [0,_g9];},_ga=function(_gb,_){var _gc=B(_g7(_)),_gd=_gc;return new F(function(){return A(_7o,[_gb,_gd,_]);});},_ge=function(_gf,_){var _gg=0,_gh=_gg;if(!E(_gh)){var _gi=B((function(_){var _gj=E(_gf)[1],_gk=takeMVar(_gj),_gl=_gk,_gm=jsCatch(function(_){return new F(function(){return (function(_){return new F(function(){return _ga(_gl,_);});})();});},function(_gn,_){var _=putMVar(_gj,_gl);return new F(function(){return die(_gn);});}),_go=_gm,_=putMVar(_gj,_gl);return _go;})()),_gp=_gi;return _1i;}else{var _gq=E(_gf)[1],_gr=takeMVar(_gq),_gs=_gr,_gt=jsCatch(function(_){return new F(function(){return _ga(_gs,_);});},function(_gu,_){var _=putMVar(_gq,_gs);return new F(function(){return die(_gu);});}),_gv=_gt,_=putMVar(_gq,_gs);return _1i;}},_gw=[0,0],_gx=0,_gy=new T(function(){return B(_cJ(_gx));}),_gz=new T(function(){return die(_gy);}),_gA=function(_gB,_gC){var _gD=E(_gC);if(!_gD){return E(_cM);}else{var _gE=function(_gF){if(_gB<=0){if(_gB>=0){var _gG=quotRemI(_gB,_gD);return [0,[0,_gG[1]],[0,_gG[2]]];}else{if(_gD<=0){var _gH=quotRemI(_gB,_gD);return [0,[0,_gH[1]],[0,_gH[2]]];}else{var _gI=quotRemI(_gB+1|0,_gD);return [0,[0,_gI[1]-1|0],[0,(_gI[2]+_gD|0)-1|0]];}}}else{if(_gD>=0){if(_gB>=0){var _gJ=quotRemI(_gB,_gD);return [0,[0,_gJ[1]],[0,_gJ[2]]];}else{if(_gD<=0){var _gK=quotRemI(_gB,_gD);return [0,[0,_gK[1]],[0,_gK[2]]];}else{var _gL=quotRemI(_gB+1|0,_gD);return [0,[0,_gL[1]-1|0],[0,(_gL[2]+_gD|0)-1|0]];}}}else{var _gM=quotRemI(_gB-1|0,_gD);return [0,[0,_gM[1]-1|0],[0,(_gM[2]+_gD|0)+1|0]];}}};return E(_gD)==(-1)?E(_gB)==(-2147483648)?[0,_gz,_gw]:B(_gE(_)):B(_gE(_));}},_gN=function(_gO){var _gP=B(_gA((_gO>>>0&2147483647>>>0)>>>0&4.294967295e9,2147483562));return [0,E(_gP[2])[1]+1|0,B(_dq(E(_gP[1])[1],2147483398))+1|0];},_gQ=function(_){var _gR=B(A(_22,["(function(){ return Math.floor(Math.random() * Math.pow(2, 53)); })",_])),_gS=_gR;return new T(function(){var _gT=jsTrunc(_gS),_gU=_gT,_gV=B(_gN(_gU));return [0,_gV[1],_gV[2]];});},_gW=[1],_gX=[6,_5M],_gY=[6,_5L],_gZ=new T(function(){return B(unCStr("tail"));}),_h0=new T(function(){return B(_68(_gZ));}),_h1=function(_h2,_h3,_h4,_h5,_h6){var _h7=function(_h8){var _h9=new T(function(){return !E(_h4)?[0,_h2,new T(function(){var _ha=E(_h3);return [0,[1,_h8,_ha[1]],_ha[2],_ha[3],_ha[4],_ha[5]];})]:[0,new T(function(){var _hb=E(_h2);return [0,[1,_h8,_hb[1]],_hb[2],_hb[3],_hb[4],_hb[5]];}),_h3];},1);return [0,new T(function(){if(!E(_h4)){var _hc=E(_h9),_hd=[0,_hc[1],new T(function(){var _he=E(_hc[2]);return [0,_he[1],new T(function(){var _hf=E(_he[2]);return _hf[0]==0?E(_h0):E(_hf[2]);}),_he[3],_he[4],_he[5]];})];}else{var _hg=E(_h9),_hd=[0,new T(function(){var _hh=E(_hg[1]);return [0,_hh[1],new T(function(){var _hi=E(_hh[2]);return _hi[0]==0?E(_h0):E(_hi[2]);}),_hh[3],_hh[4],_hh[5]];}),_hg[2]];}return _hd;}),_h4,_gW,_h5,_h6];};if(!E(_h4)){var _hj=E(_h3),_hk=E(_hj[2]);return _hk[0]==0?[0,[0,_h2,_hj],_5L,_gY,_h5,_h6]:B(_h7(_hk[1]));}else{var _hl=E(_h2),_hm=E(_hl[2]);return _hm[0]==0?[0,[0,_hl,_h3],_5M,_gX,_h5,_h6]:B(_h7(_hm[1]));}},_hn=function(_ho){var _hp=E(_ho),_hq=E(_hp[1]),_hr=B(_h1(_hq[1],_hq[2],_hp[2],_hp[4],_hp[5]));return [0,_hr[1],_hr[2],_hr[3],_hr[4],_hr[5]];},_hs=[8,_],_ht=function(_hu){var _hv=String(_hu),_hw=_hv;return new F(function(){return fromJSStr(_hw);});},_hx=function(_hy,_hz){while(1){var _hA=E(_hy);if(!_hA[0]){return E(_hz)[0]==0?true:false;}else{var _hB=E(_hz);if(!_hB[0]){return false;}else{if(E(_hA[1])[1]!=E(_hB[1])[1]){return false;}else{_hy=_hA[2];_hz=_hB[2];continue;}}}}},_hC=new T(function(){return B(unCStr("LI"));}),_hD=new T(function(){return B(_22("(function(e){ return e.tagName })"));}),_hE=new T(function(){return B(unCStr("wheel"));}),_hF=new T(function(){return B(unCStr("mouseout"));}),_hG=new T(function(){return B(unCStr("mouseover"));}),_hH=new T(function(){return B(unCStr("mousemove"));}),_hI=new T(function(){return B(unCStr("blur"));}),_hJ=new T(function(){return B(unCStr("focus"));}),_hK=new T(function(){return B(unCStr("change"));}),_hL=new T(function(){return B(unCStr("unload"));}),_hM=new T(function(){return B(unCStr("load"));}),_hN=new T(function(){return B(unCStr("submit"));}),_hO=new T(function(){return B(unCStr("keydown"));}),_hP=new T(function(){return B(unCStr("keyup"));}),_hQ=new T(function(){return B(unCStr("keypress"));}),_hR=new T(function(){return B(unCStr("mouseup"));}),_hS=new T(function(){return B(unCStr("mousedown"));}),_hT=new T(function(){return B(unCStr("dblclick"));}),_hU=new T(function(){return B(unCStr("click"));}),_hV=function(_hW){switch(E(_hW)[0]){case 0:return E(_hM);case 1:return E(_hL);case 2:return E(_hK);case 3:return E(_hJ);case 4:return E(_hI);case 5:return E(_hH);case 6:return E(_hG);case 7:return E(_hF);case 8:return E(_hU);case 9:return E(_hT);case 10:return E(_hS);case 11:return E(_hR);case 12:return E(_hQ);case 13:return E(_hP);case 14:return E(_hO);case 15:return E(_hN);default:return E(_hE);}},_hX=new T(function(){return E(0);}),_hY=new T(function(){return B(_22("(function(f) {  return (function() {      return (function(){        var args=Array.prototype.slice.call(arguments,0);        args.push(0);        return E(B(A(f, args)));    });  });})"));}),_hZ=function(_i0,_){return new F(function(){return A(_hY,[E(_i0),_]);});},_i1=function(_i2,_){return new F(function(){return _hZ(_i2,_);});},_i3=new T(function(){return B(_22("(function(node, evtName, f){ node.addEventListener(evtName, (function(e){ f(e.target) }))})"));}),_i4=function(_i5,_i6){return function(_i7,_){var _i8=E(_i7),_i9=B(A(_i3,[E(_i8[1]),E(toJSStr(E(new T(function(){return B(_hV(_i5));})))),E(new T(function(){return B(_1Y(function(_){var _=0;return new F(function(){return _i1(function(_ia){return new F(function(){return _1Y(function(_){var _=0,_ib=B(A(_i6,[[0,_ia],_])),_ic=_ib;return E(_hX);});});},_);});}));})),_])),_id=_i9;return _i8;};},_ie=function(_if){return new F(function(){return _i4(_hs,function(_ig,_){var _ih=E(_ig)[1],_ii=B(A(_hD,[E(_ih),_])),_ij=_ii;if(!B(_hx(B(_ht(_ij)),_hC))){return _1i;}else{var _ik=B(_3z(_6y,_ih,_)),_il=_ik;return new F(function(){return A(_if,[_il,_]);});}});});},_im=new T(function(){return B(unCStr("TD"));}),_in=function(_io){return new F(function(){return _i4(_hs,function(_ip,_){var _iq=E(_ip)[1],_ir=E(_iq),_is=B(A(_hD,[_ir,_])),_it=_is;if(!B(_hx(B(_ht(_it)),_im))){return _1i;}else{var _iu=B(A(_6f,[_ir,_])),_iv=_iu,_iw=B(_3z(_6y,_iv,_)),_ix=_iw,_iy=B(_3z(_6y,_iq,_)),_iz=_iy;return new F(function(){return A(_io,[_ix,_iz,_]);});}});});},_iA=new T(function(){return B(unCStr("#field"));}),_iB=new T(function(){return B(unCStr("#yours ol.hand"));}),_iC=function(_iD,_iE){return new F(function(){return A(_iD,[_iE]);});},_iF=[5],_iG=function(_iH){return [0];},_iI=function(_iJ,_iK,_iL,_iM,_iN){if(!B(_57(_iN,_iJ,_iK,_iL,_iM))){return [0];}else{var _iO=B(_3N(_iJ,_9,function(_iP){return new F(function(){return _3N(_iK,_9,_42,_iP);});},new T(function(){return E(E(_iN)[4]);})));return _iO[0]==0?[0]:[1,new T(function(){var _iQ=E(_iN),_iR=_iQ[1],_iS=_iQ[4],_iT=_iQ[5],_iU=new T(function(){return B(_3N(_iJ,_iC,function(_iV){return new F(function(){return _3N(_iK,_iC,_iG,_iV);});},new T(function(){return B(_3N(_iL,_iC,function(_iW){return new F(function(){return _3N(_iM,_iC,function(_iX){return [1,_iO[1]];},_iW);});},_iS));})));});if(!E(_iQ[2])){var _iY=(E(_iL)[1]+1|0)!=B(_2y(_iS,0))?[0,_iR,_5L,_iF,_iU,_iT]:[0,_iR,_5L,_gY,_iU,_iT];}else{var _iY=E(E(_iL)[1])==0?[0,_iR,_5M,_gX,_iU,_iT]:[0,_iR,_5M,_iF,_iU,_iT];}var _iZ=_iY;return _iZ;})];}},_j0=function(_j1,_j2,_j3){var _j4=B(_3N(_j1,_9,function(_j5){return new F(function(){return _3N(_j2,_9,_42,_j5);});},new T(function(){return E(E(_j3)[4]);})));if(!_j4[0]){return [0];}else{var _j6=_j4[1],_j7=E(_j3),_j8=_j7[2],_j9=_j7[4],_ja=function(_jb){return B(_5w(_j8,_j9,_j1,_j2))[0]==0?[0]:[1,[0,_j7[1],_j8,[2,[0,_j1,_j2]],_j9,_j7[5]]];};return !E(_j8)?!E(E(_j6)[1])?B(_ja(_)):[0]:!E(E(_j6)[1])?[0]:B(_ja(_));}},_jc=[0,114514],_jd=function(_je,_jf){var _jg=new T(function(){var _jh=_je+1|0;return _jh>=0?B(_3I(_jh,_jf)):E(_jf);});if(_je>0){var _ji=function(_jj,_jk){var _jl=E(_jj);if(!_jl[0]){return E(_jg);}else{var _jm=_jl[1];return _jk>1?[1,_jm,new T(function(){return B(_ji(_jl[2],_jk-1|0));})]:[1,_jm,_jg];}};return new F(function(){return _ji(_jf,_je);});}else{return E(_jg);}},_jn=function(_jo,_jp){return new F(function(){return _jd(E(_jo)[1],_jp);});},_jq=function(_jr,_js){var _jt=E(_js);return _jt[0]==0?[0]:[1,_jr,new T(function(){return B(_jq(_jt[1],_jt[2]));})];},_ju=new T(function(){return B(unCStr("init"));}),_jv=new T(function(){return B(_68(_ju));}),_jw=function(_jx,_jy,_jz,_jA){return B(_3N(_jy,_9,_42,new T(function(){var _jB=E(_jA),_jC=_jB[4];if(!E(_jB[2])){var _jD=E(_jC),_jE=_jD[0]==0?E(_6b):E(_jD[1]);}else{var _jF=E(_jC),_jE=_jF[0]==0?E(_6G):B(_6B(_jF[1],_jF[2]));}var _jG=_jE;return _jG;})))[0]==0?[1,new T(function(){var _jH=E(_jz),_jI=_jH[1],_jJ=function(_jK){var _jL=E(_jA),_jM=_jL[1],_jN=_jL[2],_jO=_jL[4],_jP=_jL[5],_jQ=new T(function(){var _jR=new T(function(){return B(_3N(_jy,_iC,function(_jS){return [1,[0,_jN,_jH]];},new T(function(){if(!E(_jN)){var _jT=E(_jO),_jU=_jT[0]==0?E(_6b):E(_jT[1]);}else{var _jV=E(_jO),_jU=_jV[0]==0?E(_6G):B(_6B(_jV[1],_jV[2]));}return _jU;})));});if(!E(_jN)){var _jW=[1,_jR,new T(function(){var _jX=E(_jO);return _jX[0]==0?E(_h0):E(_jX[2]);})];}else{var _jY=E(_jO);if(!_jY[0]){var _jZ=E(_jv);}else{var _jZ=B(_Y(B(_jq(_jY[1],_jY[2])),[1,_jR,_C]));}var _jW=_jZ;}return _jW;}),_k0=new T(function(){if(!E(_jN)){var _k1=E(_jM),_k2=[0,_k1[1],new T(function(){var _k3=E(_k1[2]);return [0,new T(function(){return B(_jn(_jx,_k3[1]));}),_k3[2],_k3[3],_k3[4],_k3[5]];})];}else{var _k4=E(_jM),_k2=[0,new T(function(){var _k5=E(_k4[1]);return [0,new T(function(){return B(_jn(_jx,_k5[1]));}),_k5[2],_k5[3],_k5[4],_k5[5]];}),_k4[2]];}return _k2;});return E(_jK)==0?[0,_k0,_jN,_iF,_jQ,_jP]:[0,_k0,_jN,[3,new T(function(){return _jI<=10?E(_jI)==1?E(_jc):E(_4f):E(_7d);})],_jQ,_jP];};if(_jI<=10){if(E(_jI)==1){var _k6=B(_jJ(114514)),_k7=[0,_k6[1],_k6[2],_k6[3],_k6[4],_k6[5]];}else{var _k8=B(_jJ(0)),_k7=[0,_k8[1],_k8[2],_k8[3],_k8[4],_k8[5]];}var _k9=_k7;}else{var _ka=B(_jJ(2)),_k9=[0,_ka[1],_ka[2],_ka[3],_ka[4],_ka[5]];}var _kb=_k9,_kc=_kb;return _kc;})]:[0];},_kd=function(_ke,_kf,_kg,_kh,_ki,_kj,_kk){var _kl=[0,_kg,_kh,_ki,_kj,_kk],_km=E(_ki);switch(_km[0]){case 1:var _kn=B(_j0(_ke,_kf,_kl));return _kn[0]==0?E(_kl):E(_kn[1]);case 2:var _ko=E(_km[1]),_kp=B(_iI(_ko[1],_ko[2],_ke,_kf,_kl));return _kp[0]==0?E(_kl):E(_kp[1]);case 4:var _kq=_km[1],_kr=function(_ks){var _kt=function(_ku){var _kv=B(_jw(_kq,_kf,new T(function(){var _kw=E(_kq)[1];if(_kw>=0){if(!E(_kh)){var _kx=B(_4(E(E(_kg)[2])[1],_kw));}else{var _kx=B(_4(E(E(_kg)[1])[1],_kw));}var _ky=_kx;}else{var _ky=E(_1);}var _kz=_ky,_kA=_kz;return _kA;},1),_kl));return _kv[0]==0?E(_kl):E(_kv[1]);};if(!E(_kh)){return E(E(_ke)[1])==0?B(_kt(_)):E(_kl);}else{return new F(function(){return _kt(_);});}};if(!E(_kh)){return new F(function(){return _kr(_);});}else{return E(_ke)[1]!=(B(_2y(_kj,0))-1|0)?E(_kl):B(_kr(_));}break;default:return E(_kl);}},_kB=function(_kC,_kD,_kE){var _kF=E(_kE);return new F(function(){return _kd(_kC,_kD,_kF[1],_kF[2],_kF[3],_kF[4],_kF[5]);});},_kG=function(_kH,_kI,_kJ,_kK,_kL,_kM){var _kN=B(_3N(_kH,_9,_42,new T(function(){if(!E(_kK)){var _kO=E(E(_kJ)[1]);}else{var _kO=E(E(_kI)[1]);}return _kO;})))[1];if(!E(_kK)){var _kP=E(_kJ);return !B(_7g(_kN,_kP[1]))?[0]:[1,[0,[0,_kI,_kP],_5L,[4,_kH],_kL,_kM]];}else{var _kQ=E(_kI);return !B(_7g(_kN,_kQ[1]))?[0]:[1,[0,[0,_kQ,_kJ],_5M,[4,_kH],_kL,_kM]];}},_kR=function(_kS,_kT){return E(_kS);},_kU=[0,_iC,_kR],_kV=[0,2147483647],_kW=[0,-2147483648],_kX=[0,2147483562],_kY=function(_kZ,_l0,_l1,_l2){while(1){var _l3=(function(_l4,_l5,_l6,_l7){if(!B(_d8(_l5,_l6))){var _l8=B(_dH(B(_dh(_l6,_l5)),_bM)),_l9=B((function(_la,_lb,_lc){while(1){if(!B(_cX(_la,B(_dS(_l8,_dg))))){var _ld=E(_lc),_le=B(_ar(_ld[1],_ld[2])),_lf=B(_dS(_la,_kX)),_lg=B(_dH(B(_dS(_lb,_kX)),B(_dh(B(_dQ(E(_le[1])[1])),_bM))));_lc=_le[2];_la=_lf;_lb=_lg;continue;}else{return [0,_lb,_lc];}}})(_bM,_d7,_l7));return [0,new T(function(){return B(A(_cV,[_l4,new T(function(){if(!B(_cN(_l8,_d7))){var _lh=B(_dH(_l5,B(_dx(_l9[1],_l8))));}else{var _lh=E(_cM);}return _lh;})]));}),_l9[2]];}else{var _li=_l4,_lj=_l6,_lk=_l5,_ll=_l7;_kZ=_li;_l0=_lj;_l1=_lk;_l2=_ll;return null;}})(_kZ,_l0,_l1,_l2);if(_l3!=null){return _l3;}}},_lm=function(_ln,_lo,_lp){var _lq=B(A(_ln,[_kU,function(_lr){var _ls=E(_lr),_lt=_ls[2];return [0,_ls[1],[1,_lo,new T(function(){return B(_f7(B(_fv(B(_1A(_bg,_lt)))),B(_bi(B(_eu(_b2,B(_2y(_lt,0))-1|0,new T(function(){return E(E(_lp)[5]);})))))[1]));})],_ls[3],_ls[4],_ls[5]];},_lp]));return [0,_lq[1],_lq[2],_lq[3],_lq[4],new T(function(){return E(B(_kY(_bL,_kW,_kV,_lq[5]))[2]);})];},_lu=function(_lv,_lw,_lx){return new F(function(){return A(_lv,[function(_ly){var _lz=E(_lx),_lA=_lz[1],_lB=_lz[2];return [0,new T(function(){if(!E(_lB)){var _lC=[0,E(_lA)[1],_ly];}else{var _lC=[0,_ly,E(_lA)[2]];}return _lC;}),_lB,_lz[3],_lz[4],_lz[5]];},new T(function(){return B(A(_lw,[new T(function(){var _lD=E(_lx),_lE=_lD[1];if(!E(_lD[2])){var _lF=E(E(_lE)[2]);}else{var _lF=E(E(_lE)[1]);}var _lG=_lF;return _lG;})]));})]);});},_lH=function(_lI,_lJ,_lK){return new F(function(){return _lu(E(_lI)[1],_lJ,_lK);});},_lL=function(_lM,_lN,_lO){var _lP=function(_lQ){if(_lM<=_lQ){var _lR=B(_lm(_lH,new T(function(){return B(_3N(_lN,_9,_42,new T(function(){var _lS=E(_lO),_lT=_lS[1];if(!E(_lS[2])){var _lU=E(E(E(_lT)[2])[1]);}else{var _lU=E(E(E(_lT)[1])[1]);}var _lV=_lU;return _lV;})));}),_lO)),_lW=_lR[1],_lX=_lR[2];return [0,new T(function(){if(!E(_lX)){var _lY=E(_lW),_lZ=[0,_lY[1],new T(function(){var _m0=E(_lY[2]);return [0,new T(function(){return B(_jn(_lN,_m0[1]));}),_m0[2],_m0[3],_m0[4],_m0[5]];})];}else{var _m1=E(_lW),_lZ=[0,new T(function(){var _m2=E(_m1[1]);return [0,new T(function(){return B(_jn(_lN,_m2[1]));}),_m2[2],_m2[3],_m2[4],_m2[5]];}),_m1[2]];}return _lZ;}),_lX,_iF,_lR[4],_lR[5]];}else{var _m3=B(_lm(_lH,new T(function(){return B(_3N(_lN,_9,_42,new T(function(){var _m4=E(_lO),_m5=_m4[1];if(!E(_m4[2])){var _m6=E(E(E(_m5)[2])[1]);}else{var _m6=E(E(E(_m5)[1])[1]);}var _m7=_m6;return _m7;})));}),new T(function(){var _m8=E(_lO),_m9=_m8[1],_ma=_m8[2];return [0,_m9,_ma,[3,new T(function(){if(E(B(_3N(_lN,_9,_42,new T(function(){if(!E(_ma)){var _mb=E(E(E(_m9)[2])[1]);}else{var _mb=E(E(E(_m9)[1])[1]);}return _mb;})))[1])==1){var _mc=[0,_lM-2|0];}else{var _mc=[0,_lM-1|0];}var _md=_mc;return _md;})],_m8[4],_m8[5]];}))),_me=_m3[1],_mf=_m3[2];return [0,new T(function(){if(!E(_mf)){var _mg=E(_me),_mh=[0,_mg[1],new T(function(){var _mi=E(_mg[2]);return [0,new T(function(){return B(_jn(_lN,_mi[1]));}),_mi[2],_mi[3],_mi[4],_mi[5]];})];}else{var _mj=E(_me),_mh=[0,new T(function(){var _mk=E(_mj[1]);return [0,new T(function(){return B(_jn(_lN,_mk[1]));}),_mk[2],_mk[3],_mk[4],_mk[5]];}),_mj[2]];}return _mh;}),_mf,_m3[3],_m3[4],_m3[5]];}};return E(B(_3N(_lN,_9,_42,new T(function(){var _ml=E(_lO),_mm=_ml[1];if(!E(_ml[2])){var _mn=E(E(E(_mm)[2])[1]);}else{var _mn=E(E(E(_mm)[1])[1]);}var _mo=_mn;return _mo;})))[1])==1?B(_lP(2)):B(_lP(1));},_mp=function(_mq,_mr,_ms,_mt,_mu,_mv){var _mw=E(_mt);switch(_mw[0]){case 1:var _mx=E(_mr),_my=B(_kG(_mq,_mx[1],_mx[2],_ms,_mu,_mv));return _my[0]==0?[0,_mx,_ms,_gW,_mu,_mv]:E(_my[1]);case 3:var _mz=B(_lL(E(_mw[1])[1],_mq,[0,_mr,_ms,_mw,_mu,_mv]));return [0,_mz[1],_mz[2],_mz[3],_mz[4],_mz[5]];case 4:var _mA=E(_mq);if(_mA[1]!=E(_mw[1])[1]){var _mB=E(_mr),_mC=B(_kG(_mA,_mB[1],_mB[2],_ms,_mu,_mv));return _mC[0]==0?[0,_mB,_ms,_mw,_mu,_mv]:E(_mC[1]);}else{return [0,_mr,_ms,_gW,_mu,_mv];}break;default:return [0,_mr,_ms,_mw,_mu,_mv];}},_mD=function(_mE,_mF){var _mG=E(_mF);return new F(function(){return _mp(_mE,_mG[1],_mG[2],_mG[3],_mG[4],_mG[5]);});},_mH=function(_mI,_mJ,_){var _mK=B(A(_mI,[_])),_mL=_mK,_mM=E(_mJ),_mN=jsSetTimeout(1000,_mM);return _1i;},_mO=function(_){return _1i;},_mP=new T(function(){return B(unCStr("foldr1"));}),_mQ=new T(function(){return B(_68(_mP));}),_mR=function(_mS,_mT){var _mU=E(_mT);if(!_mU[0]){return E(_mQ);}else{var _mV=_mU[1],_mW=E(_mU[2]);if(!_mW[0]){return E(_mV);}else{return new F(function(){return A(_mS,[_mV,new T(function(){return B(_mR(_mS,_mW));})]);});}}},_mX=function(_mY){var _mZ=E(_mY);return [0,_mZ[1],new T(function(){return !E(_mZ[2])?true:false;}),_ai,_mZ[4],_mZ[5]];},_n0=function(_n1){return new F(function(){return _mR(_mH,[1,_mO,[1,function(_){var _n2=0,_n3=_n2;if(!E(_n3)){var _n4=B((function(_){var _n5=E(_n1)[1],_n6=takeMVar(_n5),_n7=_n6,_n8=jsCatch(function(_){return new F(function(){return (function(_){return new T(function(){return B(_mX(_n7));});})();});},function(_n9,_){var _=putMVar(_n5,_n7);return new F(function(){return die(_n9);});}),_na=_n8,_=putMVar(_n5,_na);return _1i;})()),_nb=_n4;return new F(function(){return _ge(_n1,_);});}else{var _nc=E(_n1),_nd=_nc[1],_ne=takeMVar(_nd),_nf=_ne,_ng=jsCatch(function(_){return new T(function(){return B(_mX(_nf));});},function(_nh,_){var _=putMVar(_nd,_nf);return new F(function(){return die(_nh);});}),_ni=_ng,_=putMVar(_nd,_ni);return new F(function(){return _ge(_nc,_);});}},[1,function(_){var _nj=0,_nk=_nj;if(!E(_nk)){var _nl=B((function(_){var _nm=E(_n1)[1],_nn=takeMVar(_nm),_no=_nn,_np=jsCatch(function(_){return new F(function(){return (function(_){return new T(function(){return B(_hn(_no));});})();});},function(_nq,_){var _=putMVar(_nm,_no);return new F(function(){return die(_nq);});}),_nr=_np,_=putMVar(_nm,_nr);return _1i;})()),_ns=_nl;return new F(function(){return _ge(_n1,_);});}else{var _nt=E(_n1),_nu=_nt[1],_nv=takeMVar(_nu),_nw=_nv,_nx=jsCatch(function(_){return new T(function(){return B(_hn(_nw));});},function(_ny,_){var _=putMVar(_nu,_nw);return new F(function(){return die(_ny);});}),_nz=_nx,_=putMVar(_nu,_nz);return new F(function(){return _ge(_nt,_);});}},_C]]]);});},_nA=function(_){var _nB=B(_gQ(_)),_nC=_nB,_nD=B(_gQ(_)),_nE=_nD,_nF=B(_gQ(_)),_nG=_nF,_nH=newMVar(),_nI=_nH,_nJ=new T(function(){var _nK=B(_g1(_nC,_nE,_nG));return [0,_nK[1],_nK[2],_nK[3],_nK[4],_nK[5]];}),_=putMVar(_nI,_nJ),_nL=[0,_nI],_nM=function(_){var _nN=B(_ge(_nL,_)),_nO=_nN,_nP=0,_nQ=_nP;if(!E(_nQ)){var _nR=B((function(_){var _nS=takeMVar(_nI),_nT=_nS,_nU=jsCatch(function(_){return new F(function(){return (function(_){return E(E(_nT)[3])[0]==5?B(A(_n0,[_nL,_])):_1i;})();});},function(_nV,_){var _=putMVar(_nI,_nT);return new F(function(){return die(_nV);});}),_nW=_nU,_=putMVar(_nI,_nT);return _nW;})()),_nX=_nR;return _1i;}else{var _nY=takeMVar(_nI),_nZ=_nY,_o0=jsCatch(function(_){return E(E(_nZ)[3])[0]==5?B(A(_n0,[_nL,_])):_1i;},function(_o1,_){var _=putMVar(_nI,_nZ);return new F(function(){return die(_o1);});}),_o2=_o0,_=putMVar(_nI,_nZ);return _1i;}},_o3=B(_g7(_)),_o4=_o3,_o5=B(_28(_iB,new T(function(){return B(_ie(function(_o6,_){var _o7=0,_o8=_o7;if(!E(_o8)){var _o9=B((function(_){var _oa=takeMVar(_nI),_ob=_oa,_oc=jsCatch(function(_){return new F(function(){return (function(_){return new T(function(){return B(_mD(_o6,_ob));});})();});},function(_od,_){var _=putMVar(_nI,_ob);return new F(function(){return die(_od);});}),_oe=_oc,_=putMVar(_nI,_oe);return _1i;})()),_of=_o9;return new F(function(){return _nM(_);});}else{var _og=takeMVar(_nI),_oh=_og,_oi=jsCatch(function(_){return new T(function(){return B(_mD(_o6,_oh));});},function(_oj,_){var _=putMVar(_nI,_oh);return new F(function(){return die(_oj);});}),_ok=_oi,_=putMVar(_nI,_ok);return new F(function(){return _nM(_);});}}));}),_o4,_)),_ol=_o5,_om=B(_28(_iA,new T(function(){return B(_in(function(_on,_oo,_){var _op=0,_oq=_op;if(!E(_oq)){var _or=B((function(_){var _os=takeMVar(_nI),_ot=_os,_ou=jsCatch(function(_){return new F(function(){return (function(_){return new T(function(){return B(_kB(_on,_oo,_ot));});})();});},function(_ov,_){var _=putMVar(_nI,_ot);return new F(function(){return die(_ov);});}),_ow=_ou,_=putMVar(_nI,_ow);return _1i;})()),_ox=_or;return new F(function(){return _nM(_);});}else{var _oy=takeMVar(_nI),_oz=_oy,_oA=jsCatch(function(_){return new T(function(){return B(_kB(_on,_oo,_oz));});},function(_oB,_){var _=putMVar(_nI,_oz);return new F(function(){return die(_oB);});}),_oC=_oA,_=putMVar(_nI,_oC);return new F(function(){return _nM(_);});}}));}),_o4,_)),_oD=_om,_oE=B(A(_7o,[_nJ,_o4,_])),_oF=_oE,_oG=jsSetTimeout(1000,function(_){var _oH=0,_oI=_oH;if(!E(_oI)){var _oJ=B((function(_){var _oK=takeMVar(_nI),_oL=_oK,_oM=jsCatch(function(_){return new F(function(){return (function(_){return new T(function(){return B(_hn(_oL));});})();});},function(_oN,_){var _=putMVar(_nI,_oL);return new F(function(){return die(_oN);});}),_oO=_oM,_=putMVar(_nI,_oO);return _1i;})()),_oP=_oJ;return new F(function(){return _ge(_nL,_);});}else{var _oQ=takeMVar(_nI),_oR=_oQ,_oS=jsCatch(function(_){return new T(function(){return B(_hn(_oR));});},function(_oT,_){var _=putMVar(_nI,_oR);return new F(function(){return die(_oT);});}),_oU=_oS,_=putMVar(_nI,_oU);return new F(function(){return _ge(_nL,_);});}});return _1i;},_oV=function(_){return new F(function(){return _nA(_);});};
var hasteMain = function() {B(A(_oV, [0]));};window.onload = hasteMain;