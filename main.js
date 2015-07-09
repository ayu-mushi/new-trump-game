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

var _0=[0,2147483562],_1=[0,1],_2=[0,_1,_0],_3=function(_4){return E(_2);},_5=function(_6,_7){return [0,E(_6)[1],E(_7)[1]];},_8=function(_9,_a){var _b=quot(_a,52774),_c=(imul(40692,_a-(imul(_b,52774)|0)|0)|0)-(imul(_b,3791)|0)|0,_d=new T(function(){if(_c>=0){var _e=[0,_c];}else{var _e=[0,_c+2147483399|0];}var _f=_e;return _f;}),_g=quot(_9,53668),_h=(imul(40014,_9-(imul(_g,53668)|0)|0)|0)-(imul(_g,12211)|0)|0,_i=new T(function(){if(_h>=0){var _j=[0,_h];}else{var _j=[0,_h+2147483563|0];}var _k=_j;return _k;});return [0,new T(function(){var _l=E(_i)[1]-E(_d)[1]|0;if(_l>=1){var _m=[0,_l];}else{var _m=[0,_l+2147483562|0];}var _n=_m,_o=_n,_p=_o,_q=_p;return _q;}),new T(function(){return B(_5(_i,_d));})];},_r=function(_s){var _t=E(_s),_u=B(_8(_t[1],_t[2]));return [0,_u[1],_u[2]];},_v=function(_w,_x){var _y=new T(function(){return E(B(_8(_w,_x))[2]);});return [0,new T(function(){var _z=E(_w);if(_z==2147483562){var _A=[0,1,E(_y)[2]];}else{var _A=[0,_z+1|0,E(_y)[2]];}return _A;}),new T(function(){var _B=E(_y)[1],_C=E(_x);if(_C==1){var _D=[0,_B,2147483398];}else{var _D=[0,_B,_C-1|0];}var _E=_D;return _E;})];},_F=function(_G){var _H=E(_G),_I=B(_v(_H[1],_H[2]));return [0,_I[1],_I[2]];},_J=[0,_r,_3,_F],_K=new T(function(){return B(unCStr("Prelude.(!!): negative index\n"));}),_L=new T(function(){return B(err(_K));}),_M=new T(function(){return B(unCStr("Prelude.(!!): index too large\n"));}),_N=new T(function(){return B(err(_M));}),_O=function(_P,_Q){while(1){var _R=E(_P);if(!_R[0]){return E(_N);}else{var _S=E(_Q);if(!_S){return E(_R[1]);}else{_P=_R[2];_Q=_S-1|0;continue;}}}},_T=function(_U,_V,_){var _W=jsCreateTextNode(toJSStr(E(_U))),_X=_W,_Y=jsAppendChild(_X,E(_V)[1]);return [0,_X];},_Z=new T(function(){return B(unCStr("\u624b\u756a\u3092\u4ea4\u4ee3"));}),_10=new T(function(){return B(unCStr("\u53ec\u559a\u3059\u308b\u4f4d\u7f6e\u3092\u9078\u629e"));}),_11=new T(function(){return B(unCStr("\u751f\u8d04\u3092\u9078\u629e"));}),_12=new T(function(){return B(unCStr("\u624b\u672d\u3092\u9078\u629e"));}),_13=new T(function(){return B(unCStr("\u30c9\u30ed\u30fc"));}),_14=function(_15,_16,_){var _17=E(_15);if(!_17[0]){return _16;}else{var _18=B(A(_17[1],[_16,_])),_19=_18,_1a=B(_14(_17[2],_16,_)),_1b=_1a;return _16;}},_1c=[0,119],_1d=[0],_1e=[1,_1c,_1d],_1f=[0,75],_1g=[1,_1f,_1d],_1h=[0,81],_1i=[1,_1h,_1d],_1j=[0,74],_1k=[1,_1j,_1d],_1l=[0,65],_1m=[1,_1l,_1d],_1n=function(_1o,_1p){var _1q=E(_1o);return _1q[0]==0?E(_1p):[1,_1q[1],new T(function(){return B(_1n(_1q[2],_1p));})];},_1r=function(_1s,_1t){var _1u=jsShowI(_1s),_1v=_1u;return new F(function(){return _1n(fromJSStr(_1v),_1t);});},_1w=[0,41],_1x=[0,40],_1y=function(_1z,_1A,_1B){if(_1A>=0){return new F(function(){return _1r(_1A,_1B);});}else{return _1z<=6?B(_1r(_1A,_1B)):[1,_1x,new T(function(){var _1C=jsShowI(_1A),_1D=_1C;return B(_1n(fromJSStr(_1D),[1,_1w,_1B]));})];}},_1E=function(_1F){var _1G=E(_1F);if(!_1G[0]){return E(_1e);}else{var _1H=E(E(_1G[1])[1]);switch(_1H){case 1:return E(_1m);case 11:return E(_1k);case 12:return E(_1i);case 13:return E(_1g);default:return new F(function(){return _1y(0,_1H,_1d);});}}},_1I=new T(function(){return B(unCStr("td"));}),_1J=function(_1K,_1L,_1M,_){var _1N=jsCreateElem(toJSStr(E(_1I))),_1O=_1N,_1P=jsAppendChild(_1O,E(_1M)[1]),_1Q=[0,_1O],_1R=B(A(_1K,[_1L,_1Q,_])),_1S=_1R;return _1Q;},_1T=function(_1U){return function(_1V,_1W){return new F(function(){return _1J(_T,new T(function(){var _1X=E(_1U);return _1X[0]==0?[0]:B(_1E(_1X[1]));}),_1V,_1W);});};},_1Y=function(_1Z,_20){var _21=E(_20);return _21[0]==0?[0]:[1,new T(function(){return B(A(_1Z,[_21[1]]));}),new T(function(){return B(_1Y(_1Z,_21[2]));})];},_22=new T(function(){return B(unCStr("tr"));}),_23=function(_24,_25,_26,_){var _27=jsCreateElem(toJSStr(E(_22))),_28=_27,_29=jsAppendChild(_28,E(_26)[1]),_2a=[0,_28],_2b=B(A(_24,[_25,_2a,_])),_2c=_2b;return _2a;},_2d=function(_2e){return E(_2e);},_2f=function(_2g){return function(_1V,_1W){return new F(function(){return _23(_2d,function(_2h,_){return new F(function(){return _14(new T(function(){return B(_1Y(_1T,_2g));}),_2h,_);});},_1V,_1W);});};},_2i=new T(function(){return B(unCStr("table#field"));}),_2j=new T(function(){return [0,"arr2lst"];}),_2k=function(_2l){var _2m=B(A(_2l,[_])),_2n=_2m;return E(_2n);},_2o=function(_2p){return new F(function(){return _2k(function(_){var _=0;return new F(function(){return eval(_2p);});});});},_2q=function(_2r,_2s){return new F(function(){return _2k(function(_){var _=0;return new F(function(){return A(_2o,[E(_2j)[1],E(_2r),E(_2s),_]);});});});},_2t=new T(function(){return B(_2o("(function(sel){return document.querySelectorAll(sel);})"));}),_2u=function(_2v,_2w,_2x,_){var _2y=B(A(_2t,[E(toJSStr(E(_2v))),_])),_2z=_2y,_2A=function(_2B,_){var _2C=E(_2B);if(!_2C[0]){return _1d;}else{var _2D=B(A(_2w,[[0,_2C[1]],_])),_2E=_2D,_2F=B(_2A(_2C[2],_)),_2G=_2F;return [1,_2E,_2G];}},_2H=B(_2A(B(_2q(_2z,0)),_)),_2I=_2H;return _2x;},_2J=function(_2K){return function(_1V,_1W){return new F(function(){return _2u(_2i,function(_2L,_){var _2M=E(_2L),_2N=jsClearChildren(_2M[1]),_2O=B(_14(new T(function(){return B(_1Y(_2f,_2K));}),_2M,_)),_2P=_2O;return _2M;},_1V,_1W);});};},_2Q=new T(function(){return B(unCStr(" .deck"));}),_2R=new T(function(){return B(unCStr("\u306e\u6b8b\u308a\u5c71\u672d: "));}),_2S=[0,35],_2T=new T(function(){return B(unCStr(" .hand"));}),_2U=function(_2V,_2W){while(1){var _2X=E(_2V);if(!_2X[0]){return E(_2W);}else{_2V=_2X[2];var _2Y=_2W+1|0;_2W=_2Y;continue;}}},_2Z=new T(function(){return B(unCStr("li"));}),_30=function(_31,_32,_33,_){var _34=jsCreateElem(toJSStr(E(_2Z))),_35=_34,_36=jsAppendChild(_35,E(_33)[1]),_37=[0,_35],_38=B(A(_31,[_32,_37,_])),_39=_38;return _37;},_3a=function(_3b){return function(_3c,_){var _3d=B(_2u([1,_2S,new T(function(){return B(_1n(E(_3b)[4],_2Q));})],function(_3e,_){var _3f=E(_3e),_3g=jsClearChildren(_3f[1]),_3h=B(_T(new T(function(){var _3i=E(_3b);return B(_1n(_3i[3],new T(function(){return B(_1n(_2R,new T(function(){return B(_1y(0,B(_2U(_3i[2],0)),_1d));},1)));},1)));}),_3f,_)),_3j=_3h;return _3f;},_3c,_)),_3k=_3d,_3l=B(_2u([1,_2S,new T(function(){return B(_1n(E(_3b)[4],_2T));})],function(_3m,_){var _3n=E(_3m),_3o=jsClearChildren(_3n[1]),_3p=B(_14(new T(function(){var _3q=E(_3b);return B(_1Y(function(_3r){return function(_1V,_1W){return new F(function(){return _30(_T,new T(function(){return B(A(_3q[5],[_3r]));}),_1V,_1W);});};},_3q[1]));}),_3n,_)),_3s=_3p;return _3n;},_3c,_)),_3t=_3l;return _3c;};},_3u=0,_3v=function(_){var _3w=B(A(_2o,["(function(){return document.body;})",_])),_3x=_3w;return [0,_3x];},_3y=new T(function(){return B(unCStr("#yours ol.hand li"));}),_3z=new T(function(){return B(unCStr("id"));}),_3A=new T(function(){return B(unCStr("selected"));}),_3B=function(_3C,_3D,_){var _3E=B(_3v(_)),_3F=_3E,_3G=jsQuerySelectorAll(E(_3F)[1],toJSStr(E(_3y))),_3H=_3G,_3I=E(_3C)[1];if(_3I>=0){var _3J=jsSetAttr(B(_O(_3H,_3I))[1],toJSStr(E(_3z)),toJSStr(E(_3A)));return _3D;}else{return E(_L);}},_3K=new T(function(){return B(unCStr("#status"));}),_3L=new T(function(){return B(unCStr("class"));}),_3M=new T(function(){return B(unCStr("sacrifice"));}),_3N=new T(function(){return B(unCStr("\u306e\u756a\u3067\u3059\u3001"));}),_3O=function(_3P,_){return _3P;},_3Q=function(_3R){var _3S=new T(function(){return E(E(_3R)[1]);});return function(_3T,_){var _3U=B(A(new T(function(){return B(_2J(new T(function(){return E(E(_3R)[4]);},1)));}),[_3T,_])),_3V=_3U,_3W=B(_2u(_3K,function(_3X,_){var _3Y=E(_3X),_3Z=jsClearChildren(_3Y[1]),_40=B(_T(new T(function(){return B(unAppCStr("-- ",new T(function(){var _41=E(_3R),_42=_41[1],_43=new T(function(){return B(_1n(_3N,new T(function(){switch(E(_41[3])[0]){case 0:var _44=E(_13);break;case 1:var _44=E(_12);break;case 2:var _44=E(_11);break;case 3:var _44=E(_10);break;default:var _44=E(_Z);}return _44;},1)));},1);if(!E(_41[2])){var _45=B(_1n(E(E(_42)[2])[3],_43));}else{var _45=B(_1n(E(E(_42)[1])[3],_43));}var _46=_45;return _46;})));}),_3Y,_)),_47=_40;return _3Y;},_3T,_)),_48=_3W,_49=B(A(new T(function(){return B(_3a(new T(function(){return E(E(_3S)[1]);})));}),[_3T,_])),_4a=_49,_4b=B(A(new T(function(){return B(_3a(new T(function(){return E(E(_3S)[2]);})));}),[_3T,_])),_4c=_4b,_4d=B(A(new T(function(){var _4e=E(E(_3R)[3]);switch(_4e[0]){case 2:var _4f=function(_4g,_){var _4h=B(_3B(_4e[1],_4g,_)),_4i=_4h,_4j=E(_4g),_4k=jsQuerySelectorAll(_4j[1],toJSStr(E(_3y))),_4l=_4k,_4m=_4l,_4n=E(_4e[2]);if(!_4n[0]){return _4j;}else{var _4o=E(_4n[1])[1];if(_4o>=0){var _4p=E(_3L),_4q=E(_3M),_4r=jsSetAttr(B(_O(_4m,_4o))[1],toJSStr(_4p),toJSStr(_4q)),_4s=B((function(_4t,_){while(1){var _4u=E(_4t);if(!_4u[0]){return _3u;}else{var _4v=E(_4u[1])[1];if(_4v>=0){var _4w=jsSetAttr(B(_O(_4m,_4v))[1],toJSStr(_4p),toJSStr(_4q));_4t=_4u[2];continue;}else{return E(_L);}}}})(_4n[2],_)),_4x=_4s;return _4j;}else{return E(_L);}}};break;case 3:var _4f=function(_4y,_){var _4z=B(_3B(_4e[1],_4y,_)),_4A=_4z,_4B=E(_4y),_4C=jsQuerySelectorAll(_4B[1],toJSStr(E(_3y))),_4D=_4C,_4E=_4D,_4F=E(_4e[2]);if(!_4F[0]){return _4B;}else{var _4G=E(_4F[1])[1];if(_4G>=0){var _4H=E(_3L),_4I=E(_3M),_4J=jsSetAttr(B(_O(_4E,_4G))[1],toJSStr(_4H),toJSStr(_4I)),_4K=B((function(_4L,_){while(1){var _4M=E(_4L);if(!_4M[0]){return _3u;}else{var _4N=E(_4M[1])[1];if(_4N>=0){var _4O=jsSetAttr(B(_O(_4E,_4N))[1],toJSStr(_4H),toJSStr(_4I));_4L=_4M[2];continue;}else{return E(_L);}}}})(_4F[2],_)),_4P=_4K;return _4B;}else{return E(_L);}}};break;default:var _4f=E(_3O);}var _4Q=_4f;return _4Q;}),[_3T,_])),_4R=_4d;return _3T;};},_4S=[0],_4T=true,_4U=function(_4V,_4W){while(1){var _4X=E(_4V);if(!_4X){return E(_4W);}else{var _4Y=E(_4W);if(!_4Y[0]){return [0];}else{_4V=_4X-1|0;_4W=_4Y[2];continue;}}}},_4Z=function(_50){return E(_50);},_51=[0],_52=function(_53){return [1,[1,[0,_53]],new T(function(){var _54=E(_53);if(_54==13){var _55=[0];}else{var _55=B(_52(_54+1|0));}return _55;})];},_56=new T(function(){return B(_52(1));}),_57=[1,_51,_56],_58=new T(function(){return B(_1Y(_4Z,_57));}),_59=function(_5a){return _5a>1?[0,_58,new T(function(){var _5b=B(_59(_5a-1|0));return [1,_5b[1],_5b[2]];})]:[0,_58,_1d];},_5c=function(_5d){var _5e=E(_5d);if(!_5e[0]){return [0];}else{return new F(function(){return _1n(_5e[1],new T(function(){return B(_5c(_5e[2]));},1));});}},_5f=new T(function(){var _5g=B(_59(4));return B(_5c([1,_5g[1],_5g[2]]));}),_5h=new T(function(){return [0,B(_2U(_5f,0))];}),_5i=function(_5j){return _5j>1?[0,_51,new T(function(){var _5k=B(_5i(_5j-1|0));return [1,_5k[1],_5k[2]];})]:[0,_51,_1d];},_5l=new T(function(){var _5m=B(_5i(3));return [1,_5m[1],_5m[2]];}),_5n=function(_5o){return _5o>1?[0,_5l,new T(function(){var _5p=B(_5n(_5o-1|0));return [1,_5p[1],_5p[2]];})]:[0,_5l,_1d];},_5q=new T(function(){var _5r=B(_5n(5));return [1,_5r[1],_5r[2]];}),_5s=[0,63],_5t=[1,_5s,_1d],_5u=function(_5v){return E(_5t);},_5w=new T(function(){return B(unCStr("computers"));}),_5x=new T(function(){return B(unCStr("\u30b3\u30f3\u30d4\u30e5\u30fc\u30bf"));}),_5y=new T(function(){return B(unCStr("yours"));}),_5z=new T(function(){return B(unCStr("\u3042\u306a\u305f"));}),_5A=function(_5B){return [0,E(E(_5B))];},_5C=function(_5D){var _5E=E(_5D);if(!_5E[0]){return [0,_1d,_1d];}else{var _5F=E(_5E[1]),_5G=new T(function(){var _5H=B(_5C(_5E[2]));return [0,_5H[1],_5H[2]];});return [0,[1,_5F[1],new T(function(){return E(E(_5G)[1]);})],[1,_5F[2],new T(function(){return E(E(_5G)[2]);})]];}},_5I=function(_5J,_5K){return [0,imul(E(_5J)[1],E(_5K)[1])|0];},_5L=function(_5M,_5N){return [0,E(_5M)[1]+E(_5N)[1]|0];},_5O=function(_5P,_5Q){return [0,E(_5P)[1]-E(_5Q)[1]|0];},_5R=function(_5S){var _5T=E(_5S),_5U=_5T[1];return _5U<0?[0, -_5U]:E(_5T);},_5V=function(_5W){var _5X=E(_5W);return _5X[0]==0?E(_5X[1]):I_toInt(_5X[1]);},_5Y=function(_5Z){return [0,B(_5V(_5Z))];},_60=function(_61){return [0, -E(_61)[1]];},_62=[0,-1],_63=[0,0],_64=[0,1],_65=function(_66){var _67=E(_66)[1];return _67>=0?E(_67)==0?E(_63):E(_64):E(_62);},_68=[0,_5L,_5I,_5O,_60,_5R,_65,_5Y],_69=[0,1],_6a=new T(function(){return B(unCStr("base"));}),_6b=new T(function(){return B(unCStr("GHC.Exception"));}),_6c=new T(function(){return B(unCStr("ArithException"));}),_6d=[0,I_fromBits([4194982440,719304104]),I_fromBits([3110813675,1843557400]),_6a,_6b,_6c],_6e=[0,I_fromBits([4194982440,719304104]),I_fromBits([3110813675,1843557400]),_6d,_1d],_6f=function(_6g){return E(_6e);},_6h=function(_6i){return E(E(_6i)[1]);},_6j=function(_6k,_6l,_6m){var _6n=B(A(_6k,[_])),_6o=B(A(_6l,[_])),_6p=hs_eqWord64(_6n[1],_6o[1]),_6q=_6p;if(!E(_6q)){return [0];}else{var _6r=hs_eqWord64(_6n[2],_6o[2]),_6s=_6r;return E(_6s)==0?[0]:[1,_6m];}},_6t=function(_6u){var _6v=E(_6u);return new F(function(){return _6j(B(_6h(_6v[1])),_6f,_6v[2]);});},_6w=new T(function(){return B(unCStr("arithmetic underflow"));}),_6x=new T(function(){return B(unCStr("arithmetic overflow"));}),_6y=new T(function(){return B(unCStr("Ratio has zero denominator"));}),_6z=new T(function(){return B(unCStr("denormal"));}),_6A=new T(function(){return B(unCStr("divide by zero"));}),_6B=new T(function(){return B(unCStr("loss of precision"));}),_6C=function(_6D){switch(E(_6D)){case 0:return E(_6x);case 1:return E(_6w);case 2:return E(_6B);case 3:return E(_6A);case 4:return E(_6z);default:return E(_6y);}},_6E=function(_6F){return new F(function(){return _1n(_6w,_6F);});},_6G=function(_6F){return new F(function(){return _1n(_6x,_6F);});},_6H=function(_6F){return new F(function(){return _1n(_6y,_6F);});},_6I=function(_6F){return new F(function(){return _1n(_6z,_6F);});},_6J=function(_6F){return new F(function(){return _1n(_6A,_6F);});},_6K=function(_6F){return new F(function(){return _1n(_6B,_6F);});},_6L=function(_6M){switch(E(_6M)){case 0:return E(_6G);case 1:return E(_6E);case 2:return E(_6K);case 3:return E(_6J);case 4:return E(_6I);default:return E(_6H);}},_6N=[0,44],_6O=[0,93],_6P=[0,91],_6Q=function(_6R,_6S,_6T){var _6U=E(_6S);return _6U[0]==0?B(unAppCStr("[]",_6T)):[1,_6P,new T(function(){return B(A(_6R,[_6U[1],new T(function(){var _6V=function(_6W){var _6X=E(_6W);return _6X[0]==0?E([1,_6O,_6T]):[1,_6N,new T(function(){return B(A(_6R,[_6X[1],new T(function(){return B(_6V(_6X[2]));})]));})];};return B(_6V(_6U[2]));})]));})];},_6Y=function(_6Z,_70){return new F(function(){return _6Q(_6L,_6Z,_70);});},_71=function(_72,_73){switch(E(_73)){case 0:return E(_6G);case 1:return E(_6E);case 2:return E(_6K);case 3:return E(_6J);case 4:return E(_6I);default:return E(_6H);}},_74=[0,_71,_6C,_6Y],_75=new T(function(){return [0,_6f,_74,_76,_6t];}),_76=function(_6F){return [0,_75,_6F];},_77=3,_78=new T(function(){return B(_76(_77));}),_79=new T(function(){return die(_78);}),_7a=function(_7b,_7c){var _7d=E(_7b);if(!_7d[0]){var _7e=_7d[1],_7f=E(_7c);return _7f[0]==0?_7e==_7f[1]:I_compareInt(_7f[1],_7e)==0?true:false;}else{var _7g=_7d[1],_7h=E(_7c);return _7h[0]==0?I_compareInt(_7g,_7h[1])==0?true:false:I_compare(_7g,_7h[1])==0?true:false;}},_7i=function(_7j){return E(E(_7j)[7]);},_7k=function(_7l,_7m){var _7n=E(_7l);if(!_7n[0]){var _7o=_7n[1],_7p=E(_7m);return _7p[0]==0?_7o>=_7p[1]:I_compareInt(_7p[1],_7o)<=0;}else{var _7q=_7n[1],_7r=E(_7m);return _7r[0]==0?I_compareInt(_7q,_7r[1])>=0:I_compare(_7q,_7r[1])>=0;}},_7s=function(_7t){return E(E(_7t)[2]);},_7u=[0,0],_7v=function(_7w,_7x){var _7y=E(_7w);if(!_7y[0]){var _7z=_7y[1],_7A=E(_7x);return _7A[0]==0?_7z>_7A[1]:I_compareInt(_7A[1],_7z)<0;}else{var _7B=_7y[1],_7C=E(_7x);return _7C[0]==0?I_compareInt(_7B,_7C[1])>0:I_compare(_7B,_7C[1])>0;}},_7D=[0,1000],_7E=function(_7F,_7G){while(1){var _7H=E(_7F);if(!_7H[0]){var _7I=_7H[1],_7J=E(_7G);if(!_7J[0]){var _7K=_7J[1],_7L=subC(_7I,_7K);if(!E(_7L[2])){return [0,_7L[1]];}else{_7F=[1,I_fromInt(_7I)];_7G=[1,I_fromInt(_7K)];continue;}}else{_7F=[1,I_fromInt(_7I)];_7G=_7J;continue;}}else{var _7M=E(_7G);if(!_7M[0]){_7F=_7H;_7G=[1,I_fromInt(_7M[1])];continue;}else{return [1,I_sub(_7H[1],_7M[1])];}}}},_7N=function(_7O,_7P){var _7Q=_7O%_7P;if(_7O<=0){if(_7O>=0){return E(_7Q);}else{if(_7P<=0){return E(_7Q);}else{var _7R=E(_7Q);return _7R==0?0:_7R+_7P|0;}}}else{if(_7P>=0){if(_7O>=0){return E(_7Q);}else{if(_7P<=0){return E(_7Q);}else{var _7S=E(_7Q);return _7S==0?0:_7S+_7P|0;}}}else{var _7T=E(_7Q);return _7T==0?0:_7T+_7P|0;}}},_7U=function(_7V,_7W){while(1){var _7X=E(_7V);if(!_7X[0]){var _7Y=E(_7X[1]);if(_7Y==(-2147483648)){_7V=[1,I_fromInt(-2147483648)];continue;}else{var _7Z=E(_7W);if(!_7Z[0]){return [0,B(_7N(_7Y,_7Z[1]))];}else{_7V=[1,I_fromInt(_7Y)];_7W=_7Z;continue;}}}else{var _80=_7X[1],_81=E(_7W);return _81[0]==0?[0,I_toInt(I_mod(_80,I_fromInt(_81[1])))]:[1,I_mod(_80,_81[1])];}}},_82=function(_83){return E(E(_83)[1]);},_84=function(_85,_86){while(1){var _87=E(_85);if(!_87[0]){var _88=_87[1],_89=E(_86);if(!_89[0]){var _8a=_89[1],_8b=addC(_88,_8a);if(!E(_8b[2])){return [0,_8b[1]];}else{_85=[1,I_fromInt(_88)];_86=[1,I_fromInt(_8a)];continue;}}else{_85=[1,I_fromInt(_88)];_86=_89;continue;}}else{var _8c=E(_86);if(!_8c[0]){_85=_87;_86=[1,I_fromInt(_8c[1])];continue;}else{return [1,I_add(_87[1],_8c[1])];}}}},_8d=function(_8e){return [0,_8e];},_8f=function(_8g,_8h){while(1){var _8i=E(_8g);if(!_8i[0]){var _8j=_8i[1],_8k=E(_8h);if(!_8k[0]){var _8l=_8k[1];if(!(imul(_8j,_8l)|0)){return [0,imul(_8j,_8l)|0];}else{_8g=[1,I_fromInt(_8j)];_8h=[1,I_fromInt(_8l)];continue;}}else{_8g=[1,I_fromInt(_8j)];_8h=_8k;continue;}}else{var _8m=E(_8h);if(!_8m[0]){_8g=_8i;_8h=[1,I_fromInt(_8m[1])];continue;}else{return [1,I_mul(_8i[1],_8m[1])];}}}},_8n=function(_8o,_8p,_8q,_8r,_8s){while(1){var _8t=(function(_8u,_8v,_8w,_8x,_8y){if(!B(_7v(_8w,_8x))){var _8z=B(_84(B(_7E(_8x,_8w)),_69)),_8A=new T(function(){return B(A(_7s,[_8u,_8y]));}),_8B=new T(function(){return E(E(_8A)[1]);}),_8C=new T(function(){return B(_84(B(_7E(B(_8d(E(E(_8A)[2])[1])),B(_8d(E(_8B)[1])))),_69));}),_8D=B((function(_8E,_8F,_8G){while(1){if(!B(_7k(_8E,B(_8f(_8z,_7D))))){var _8H=B(A(new T(function(){return B(_82(_8u));}),[_8G])),_8I=B(_8f(_8E,_8C)),_8J=B(_84(B(_8f(_8F,_8C)),B(_7E(B(_8d(E(_8H[1])[1])),new T(function(){return B(_8d(E(_8B)[1]));})))));_8G=_8H[2];_8E=_8I;_8F=_8J;continue;}else{return [0,_8F,_8G];}}})(_69,_7u,_8y));return [0,new T(function(){return B(A(_7i,[_8v,new T(function(){if(!B(_7a(_8z,_7u))){var _8K=B(_84(_8w,B(_7U(_8D[1],_8z))));}else{var _8K=E(_79);}return _8K;})]));}),_8D[2]];}else{var _8L=_8u,_8M=_8v,_8N=_8x,_8O=_8w,_8P=_8y;_8o=_8L;_8p=_8M;_8q=_8N;_8r=_8O;_8s=_8P;return null;}})(_8o,_8p,_8q,_8r,_8s);if(_8t!=null){return _8t;}}},_8Q=[0,0],_8R=function(_8S,_8T,_8U){var _8V=E(_8T);if(!_8V){return [0];}else{var _8W=new T(function(){var _8X=B(_8n(_8S,_68,_8Q,B(_8d(_8V)),_8U));return [0,_8X[1],_8X[2]];});return [1,[0,new T(function(){return E(E(_8W)[1]);}),_8U],new T(function(){return B(_8R(_8S,_8V-1|0,new T(function(){return E(E(_8W)[2]);})));})];}},_8Y=function(_8Z,_90,_91){return new F(function(){return _8R(_8Z,E(_90)[1],_91);});},_92=new T(function(){return B(unCStr("[extractTree] impossible"));}),_93=new T(function(){return B(err(_92));}),_94=function(_95,_96){var _97=function(_98){var _99=E(_96);if(!_99[0]){return E(_93);}else{var _9a=_99[1],_9b=_99[3],_9c=E(_99[2]);if(!_9c[0]){var _9d=new T(function(){var _9e=B(_94(_95-1|0,_9b));return [0,_9e[1],_9e[2]];});return [0,new T(function(){return E(E(_9d)[1]);}),new T(function(){return [1,_9a-1|0,E(_9c),E(E(E(_9d)[2]))];})];}else{var _9f=_9c[1],_9g=function(_9h){if(_95>=_9f){var _9i=new T(function(){var _9j=B(_94(_95-_9f|0,_9b));return [0,_9j[1],_9j[2]];});return [0,new T(function(){return E(E(_9i)[1]);}),new T(function(){return [1,_9a-1|0,E(_9c),E(E(E(_9i)[2]))];})];}else{var _9k=new T(function(){var _9l=B(_94(_95,_9c));return [0,_9l[1],_9l[2]];});return [0,new T(function(){return E(E(_9k)[1]);}),new T(function(){return [1,_9a-1|0,E(E(E(_9k)[2])),E(_9b)];})];}},_9m=E(_9b);if(!_9m[0]){return (_95+1|0)!=_9a?B(_9g(_)):[0,_9m[1],_9c];}else{return new F(function(){return _9g(_);});}}}};switch(E(_95)){case 0:var _9n=E(_96);if(!_9n[0]){return new F(function(){return _97(_);});}else{var _9o=E(_9n[2]);return _9o[0]==0?[0,_9o[1],_9n[3]]:B(_97(_));}break;case 1:var _9p=E(_96);if(!_9p[0]){return new F(function(){return _97(_);});}else{if(E(_9p[1])==2){var _9q=E(_9p[2]);if(!_9q[0]){var _9r=E(_9p[3]);return _9r[0]==0?[0,_9r[1],_9q]:B(_97(_));}else{return new F(function(){return _97(_);});}}else{return new F(function(){return _97(_);});}}break;default:return new F(function(){return _97(_);});}},_9s=new T(function(){return B(unCStr("[shuffle] called with lists of different lengths"));}),_9t=new T(function(){return B(err(_9s));}),_9u=function(_9v,_9w){var _9x=function(_9y){var _9z=E(_9w);if(!_9z[0]){return E(_9t);}else{var _9A=new T(function(){var _9B=B(_94(E(_9z[1])[1],_9v));return [0,_9B[1],_9B[2]];});return [1,new T(function(){return E(E(_9A)[1]);}),new T(function(){return B(_9u(E(_9A)[2],_9z[2]));})];}},_9C=E(_9v);return _9C[0]==0?E(_9w)[0]==0?[1,_9C[1],_1d]:B(_9x(_)):B(_9x(_));},_9D=function(_9E){var _9F=E(_9E);if(!_9F[0]){return [0];}else{var _9G=_9F[1],_9H=E(_9F[2]);if(!_9H[0]){return [1,_9G,_1d];}else{var _9I=E(_9H[1]);return [1,new T(function(){var _9J=E(E(_9G));if(!_9J[0]){var _9K=E(_9I);if(!_9K[0]){var _9L=[1,2,E(_9J),E(_9K)];}else{var _9L=[1,_9K[1]+1|0,E(_9J),E(_9K)];}var _9M=_9L;}else{var _9N=_9J[1],_9O=E(_9I);if(!_9O[0]){var _9P=[1,_9N+1|0,E(_9J),E(_9O)];}else{var _9P=[1,_9N+_9O[1]|0,E(_9J),E(_9O)];}var _9M=_9P;}return _9M;}),new T(function(){return B(_9D(_9H[2]));})];}}},_9Q=new T(function(){return B(_9D(_1d));}),_9R=new T(function(){return B(_9S(_9Q));}),_9S=function(_9T){while(1){var _9U=E(_9T);if(!_9U[0]){return E(_9R);}else{if(!E(_9U[2])[0]){return E(_9U[1]);}else{_9T=B(_9D(_9U));continue;}}}},_9V=function(_9W,_9X,_9Y){return function(_9Z){return new F(function(){return _9u(new T(function(){return B(_9S(B(_1Y(_5A,_9X))));}),B(_5C(B(_8Y(_9W,new T(function(){return [0,E(_9Y)[1]-1|0];}),_9Z))))[1]);});};},_a0=function(_a1,_a2){var _a3=E(_a1);if(!_a3){return [0];}else{var _a4=E(_a2);return _a4[0]==0?[0]:[1,_a4[1],new T(function(){return B(_a0(_a3-1|0,_a4[2]));})];}},_a5=function(_a6,_a7,_a8){var _a9=new T(function(){return B(A(_9V,[_a6,_5f,_5h,_a8]));}),_aa=new T(function(){return B(A(_9V,[_a6,_5f,_5h,_a7]));});return [0,[0,[0,new T(function(){return B(_a0(3,_aa));}),new T(function(){return B(_4U(3,_aa));}),_5z,_5y,_1E],[0,new T(function(){return B(_a0(3,_a9));}),new T(function(){return B(_4U(3,_a9));}),_5x,_5w,_5u]],_4T,_4S,_5q];},_ab=[0,0],_ac=0,_ad=new T(function(){return B(_76(_ac));}),_ae=new T(function(){return die(_ad);}),_af=function(_ag,_ah){var _ai=E(_ah);if(!_ai){return E(_79);}else{var _aj=function(_ak){if(_ag<=0){if(_ag>=0){var _al=quotRemI(_ag,_ai);return [0,[0,_al[1]],[0,_al[2]]];}else{if(_ai<=0){var _am=quotRemI(_ag,_ai);return [0,[0,_am[1]],[0,_am[2]]];}else{var _an=quotRemI(_ag+1|0,_ai);return [0,[0,_an[1]-1|0],[0,(_an[2]+_ai|0)-1|0]];}}}else{if(_ai>=0){if(_ag>=0){var _ao=quotRemI(_ag,_ai);return [0,[0,_ao[1]],[0,_ao[2]]];}else{if(_ai<=0){var _ap=quotRemI(_ag,_ai);return [0,[0,_ap[1]],[0,_ap[2]]];}else{var _aq=quotRemI(_ag+1|0,_ai);return [0,[0,_aq[1]-1|0],[0,(_aq[2]+_ai|0)-1|0]];}}}else{var _ar=quotRemI(_ag-1|0,_ai);return [0,[0,_ar[1]-1|0],[0,(_ar[2]+_ai|0)+1|0]];}}};return E(_ai)==(-1)?E(_ag)==(-2147483648)?[0,_ae,_ab]:B(_aj(_)):B(_aj(_));}},_as=function(_at){var _au=B(_af((_at>>>0&2147483647>>>0)>>>0&4.294967295e9,2147483562));return [0,E(_au[2])[1]+1|0,B(_7N(E(_au[1])[1],2147483398))+1|0];},_av=function(_){var _aw=B(A(_2o,["(function(){ return Math.floor(Math.random() * Math.pow(2, 53)); })",_])),_ax=_aw;return new T(function(){var _ay=jsTrunc(_ax),_az=_ay,_aA=B(_as(_az));return [0,_aA[1],_aA[2]];});},_aB=function(_){var _aC=B(_av(_)),_aD=_aC,_aE=B(_av(_)),_aF=_aE,_aG=newMVar(),_aH=_aG,_aI=new T(function(){var _aJ=B(_a5(_J,_aD,_aF));return [0,_aJ[1],_aJ[2],_aJ[3],_aJ[4]];}),_=putMVar(_aH,_aI),_aK=B(_3v(_)),_aL=_aK,_aM=B(A(_3Q,[_aI,_aL,_])),_aN=_aM;return _3u;},_aO=function(_){return new F(function(){return _aB(_);});};
var hasteMain = function() {B(A(_aO, [0]));};window.onload = hasteMain;