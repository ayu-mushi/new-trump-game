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

var _0=new T(function(){return B(unCStr("Prelude.(!!): negative index\n"));}),_1=new T(function(){return B(err(_0));}),_2=new T(function(){return B(unCStr("Prelude.(!!): index too large\n"));}),_3=new T(function(){return B(err(_2));}),_4=function(_5,_6){while(1){var _7=E(_5);if(!_7[0]){return E(_3);}else{var _8=E(_6);if(!_8){return E(_7[1]);}else{_5=_7[2];_6=_8-1|0;continue;}}}},_9=function(_a,_b){return E(_b);},_c=function(_d,_e,_){var _f=jsCreateTextNode(toJSStr(E(_d))),_g=_f,_h=jsAppendChild(_g,E(_e)[1]);return [0,_g];},_i=function(_j,_k,_){var _l=E(_j);if(!_l[0]){return _k;}else{var _m=B(A(_l[1],[_k,_])),_n=_m,_o=B(_i(_l[2],_k,_)),_p=_o;return _k;}},_q=new T(function(){return B(unCStr("\u30c9\u30ed\u30fc"));}),_r=new T(function(){return B(unCStr("\u3042\u306a\u305f\u306e\u52dd\u3061\u3067\u3059!"));}),_s=new T(function(){return B(unCStr("\u30b3\u30f3\u30d4\u30e5\u30fc\u30bf\u304c\u52dd\u3061!"));}),_t=new T(function(){return B(unCStr("\u624b\u756a\u3092\u4ea4\u4ee3"));}),_u=new T(function(){return B(unCStr("\u53ec\u559a\u3059\u308b\u4f4d\u7f6e\u3092\u9078\u629e"));}),_v=new T(function(){return B(unCStr("\u751f\u8d04\u3092\u9078\u629e: \u30a8\u30cd\u30eb\u30ae\u30fc\u304c\u3042\u3068"));}),_w=new T(function(){return B(unCStr("\u5fc5\u8981"));}),_x=new T(function(){return B(unCStr("\u79fb\u52d5\u3059\u308b\u4f4d\u7f6e\u3092\u9078\u629e"));}),_y=new T(function(){return B(unCStr("\u884c\u52d5\u3092\u9078\u629e"));}),_z=new T(function(){return B(unCStr(" .deck"));}),_A=new T(function(){return B(unCStr("\u306e\u6b8b\u308a\u5c71\u672d: "));}),_B=[0,35],_C=new T(function(){return B(unCStr(" .hand"));}),_D=function(_E,_F){while(1){var _G=E(_E);if(!_G[0]){return E(_F);}else{_E=_G[2];var _H=_F+1|0;_F=_H;continue;}}},_I=function(_J,_K){var _L=E(_J);return _L[0]==0?E(_K):[1,_L[1],new T(function(){return B(_I(_L[2],_K));})];},_M=function(_N,_O){var _P=jsShowI(_N),_Q=_P;return new F(function(){return _I(fromJSStr(_Q),_O);});},_R=[0,41],_S=[0,40],_T=function(_U,_V,_W){if(_V>=0){return new F(function(){return _M(_V,_W);});}else{return _U<=6?B(_M(_V,_W)):[1,_S,new T(function(){var _X=jsShowI(_V),_Y=_X;return B(_I(fromJSStr(_Y),[1,_R,_W]));})];}},_Z=[0],_10=new T(function(){return [0,"arr2lst"];}),_11=function(_12){var _13=B(A(_12,[_])),_14=_13;return E(_14);},_15=function(_16){return new F(function(){return _11(function(_){var _=0;return new F(function(){return eval(_16);});});});},_17=function(_18,_19){return new F(function(){return _11(function(_){var _=0;return new F(function(){return A(_15,[E(_10)[1],E(_18),E(_19),_]);});});});},_1a=new T(function(){return B(_15("(function(sel){return document.querySelectorAll(sel);})"));}),_1b=function(_1c,_1d,_1e,_){var _1f=B(A(_1a,[E(toJSStr(E(_1c))),_])),_1g=_1f,_1h=function(_1i,_){var _1j=E(_1i);if(!_1j[0]){return _Z;}else{var _1k=B(A(_1d,[[0,_1j[1]],_])),_1l=_1k,_1m=B(_1h(_1j[2],_)),_1n=_1m;return [1,_1l,_1n];}},_1o=B(_1h(B(_17(_1g,0)),_)),_1p=_1o;return _1e;},_1q=new T(function(){return B(unCStr("li"));}),_1r=function(_1s,_1t,_1u,_){var _1v=jsCreateElem(toJSStr(E(_1q))),_1w=_1v,_1x=jsAppendChild(_1w,E(_1u)[1]),_1y=[0,_1w],_1z=B(A(_1s,[_1t,_1y,_])),_1A=_1z;return _1y;},_1B=function(_1C,_1D){var _1E=E(_1D);return _1E[0]==0?[0]:[1,new T(function(){return B(A(_1C,[_1E[1]]));}),new T(function(){return B(_1B(_1C,_1E[2]));})];},_1F=function(_1G){return function(_1H,_){var _1I=B(_1b([1,_B,new T(function(){return B(_I(E(_1G)[4],_z));})],function(_1J,_){var _1K=E(_1J),_1L=jsClearChildren(_1K[1]),_1M=B(_c(new T(function(){var _1N=E(_1G);return B(_I(_1N[3],new T(function(){return B(_I(_A,new T(function(){return B(_T(0,B(_D(_1N[2],0)),_Z));},1)));},1)));}),_1K,_)),_1O=_1M;return _1K;},_1H,_)),_1P=_1I,_1Q=B(_1b([1,_B,new T(function(){return B(_I(E(_1G)[4],_C));})],function(_1R,_){var _1S=E(_1R),_1T=jsClearChildren(_1S[1]),_1U=B(_i(new T(function(){var _1V=E(_1G);return B(_1B(function(_1W){return function(_1X,_1Y){return new F(function(){return _1r(_c,new T(function(){return B(A(_1V[5],[_1W]));}),_1X,_1Y);});};},_1V[1]));}),_1S,_)),_1Z=_1U;return _1S;},_1H,_)),_20=_1Q;return _1H;};},_21=new T(function(){return B(unCStr("Prelude.Enum.succ{Int}: tried to take `succ\' of maxBound"));}),_22=new T(function(){return B(err(_21));}),_23=function(_24){var _25=E(E(_24)[1]);return _25==2147483647?E(_22):[0,_25+1|0];},_26=[0],_27=new T(function(){return [0,"(function(e){ return e.previousSibling })"];}),_28=new T(function(){return B(_15("(function(x) {return x === null})"));}),_29=new T(function(){return B(_15("(function(node) {return node.nodeType === 1})"));}),_2a=function(_2b,_){var _2c=E(_27)[1],_2d=B(A(_15,[_2c,E(_2b),_])),_2e=_2d,_2f=E(_2e),_2g=B(A(_28,[_2f,_])),_2h=_2g;if(_2h<=0){var _2i=B(A(_29,[_2f,_])),_2j=_2i;if(_2j<=0){return new F(function(){return (function(_2k,_){while(1){var _2l=B(A(_15,[_2c,E(_2k),_])),_2m=_2l,_2n=E(_2m),_2o=B(A(_28,[_2n,_])),_2p=_2o;if(_2p<=0){var _2q=B(A(_29,[_2n,_])),_2r=_2q;if(_2r<=0){_2k=_2m;continue;}else{return [1,[0,_2n]];}}else{return _26;}}})(_2e,_);});}else{return [1,[0,_2f]];}}else{return _26;}},_2s=function(_2t,_2u,_){while(1){var _2v=(function(_2w,_2x,_){var _2y=B(_2a(_2x,_)),_2z=_2y,_2A=E(_2z);if(!_2A[0]){return _2w;}else{_2t=new T(function(){return B(_23(_2w));});_2u=E(_2A[1])[1];return null;}})(_2t,_2u,_);if(_2v!=null){return _2v;}}},_2B=function(_2C,_2D){while(1){var _2E=E(_2C);if(!_2E){return E(_2D);}else{var _2F=E(_2D);if(!_2F[0]){return [0];}else{_2C=_2E-1|0;_2D=_2F[2];continue;}}}},_2G=function(_2H,_2I,_2J,_2K){return new F(function(){return A(_2I,[function(_2L){var _2M=E(_2H)[1],_2N=[1,_2L,new T(function(){var _2O=E(_2H)[1]+1|0;return _2O>=0?B(_2B(_2O,_2K)):E(_2K);})];if(_2M>0){var _2P=function(_2Q,_2R){var _2S=E(_2Q);if(!_2S[0]){return E(_2N);}else{var _2T=_2S[1];return _2R>1?[1,_2T,new T(function(){return B(_2P(_2S[2],_2R-1|0));})]:[1,_2T,_2N];}};return new F(function(){return _2P(_2K,_2M);});}else{return E(_2N);}},new T(function(){return B(A(_2J,[new T(function(){var _2U=E(_2H)[1];return _2U>=0?B(_4(_2K,_2U)):E(_1);})]));})]);});},_2V=function(_2W){return E(_2W);},_2X=function(_2Y){return E(E(_2Y)[1]);},_2Z=function(_30,_31,_32){while(1){var _33=E(_32);if(!_33[0]){return false;}else{if(!B(A(_2X,[_30,_31,_33[1]]))){_32=_33[2];continue;}else{return true;}}}},_34=function(_35){var _36=E(_35);return [0,new T(function(){return [0,-1+E(_36[1])[1]|0];}),_36[2]];},_37=[1,_34,_Z],_38=[0,0],_39=[0,1],_3a=[0,_39,_38],_3b=[1,_3a,_Z],_3c=function(_3d,_3e){return [0,E(_3d)[1]+E(_3e)[1]|0];},_3f=function(_3g,_3h){var _3i=E(_3g),_3j=E(_3h);return [0,new T(function(){return B(_3c(_3i[1],_3j[1]));}),new T(function(){return B(_3c(_3i[2],_3j[2]));})];},_3k=new T(function(){return B(_1B(_3f,_3b));}),_3l=function(_3m,_3n,_3o,_3p,_3q,_3r){return !B(A(_3m,[_3o,_3q]))?true:!B(A(_2X,[_3n,_3p,_3r]))?true:false;},_3s=function(_3t,_3u,_3v,_3w){var _3x=E(_3v),_3y=E(_3w);return new F(function(){return _3l(E(_3t)[1],_3u,_3x[1],_3x[2],_3y[1],_3y[2]);});},_3z=function(_3A,_3B,_3C,_3D,_3E,_3F){return !B(A(_3A,[_3C,_3E]))?false:B(A(_2X,[_3B,_3D,_3F]));},_3G=function(_3H,_3I,_3J,_3K){var _3L=E(_3J),_3M=E(_3K);return new F(function(){return _3z(E(_3H)[1],_3I,_3L[1],_3L[2],_3M[1],_3M[2]);});},_3N=function(_3O,_3P){return [0,function(_3Q,_3R){return new F(function(){return _3G(_3O,_3P,_3Q,_3R);});},function(_3Q,_3R){return new F(function(){return _3s(_3O,_3P,_3Q,_3R);});}];},_3S=function(_3T,_3U){return E(_3T)[1]==E(_3U)[1];},_3V=function(_3W,_3X){return E(_3W)[1]!=E(_3X)[1];},_3Y=[0,_3S,_3V],_3Z=new T(function(){return B(_3N(_3Y,_3Y));}),_40=function(_41,_42,_43,_44,_45){var _46=B(_2G(_42,_9,function(_47){return new F(function(){return _2G(_43,_9,_2V,_47);});},new T(function(){return E(E(_41)[4]);})));if(!_46[0]){return false;}else{var _48=E(_41),_49=function(_4a){return new F(function(){return A(_4a,[[0,_42,_43]]);});},_4b=function(_4c){var _4d=B(_2G(_44,_9,function(_4e){return new F(function(){return _2G(_45,_9,_2V,_4e);});},_48[4]));if(!_4d[0]){return true;}else{var _4f=E(_46[1]),_4g=_4f[1],_4h=E(_4f[2])[1],_4i=E(_4d[1]),_4j=E(_4i[2])[1];return _4h>=_4j?_4h!=_4j?!E(_4i[1])?E(_4g):!E(_4g)?true:false:false:false;}};return !E(_48[2])?!B(_2Z(_3Z,[0,_44,_45],B(_1B(_49,_3k))))?false:B(_4b(_)):!B(_2Z(_3Z,[0,_44,_45],B(_1B(_49,_37))))?false:B(_4b(_));}},_4k=function(_4l){return new F(function(){return err(B(unAppCStr("Oops!  Entered absent arg ",new T(function(){return B(unCStr(_4l));}))));});},_4m=new T(function(){return B(_4k("ww_s8m7{v} [lid] random-1.1:System.Random.StdGen{tc r15D}"));}),_4n=new T(function(){return B(_4k("ww_s8m5{v} [lid] main:Game.BoardTrump.GameState.Phase{tc r1WT}"));}),_4o=new T(function(){return B(_4k("ww_s8m3{v} [lid] (main:Game.BoardTrump.Player.Player{tc r1IO},\n                  main:Game.BoardTrump.Player.Player{tc r1IO})"));}),_4p=function(_4q,_4r,_4s,_4t){var _4u=[0,_4o,_4q,_4n,_4r,_4m],_4v=function(_4w){while(1){var _4x=(function(_4y){var _4z=E(_4y);if(!_4z[0]){return [0];}else{var _4A=_4z[2],_4B=new T(function(){return B(A(_4z[1],[[0,_4s,_4t]]));});if(!B(_40(_4u,_4s,_4t,new T(function(){return E(E(_4B)[1]);}),new T(function(){return E(E(_4B)[2]);})))){_4w=_4A;return null;}else{return [1,_4B,new T(function(){return B(_4v(_4A));})];}}})(_4w);if(_4x!=null){return _4x;}}};return !E(_4q)?B(_4v(_3k)):B((function(_4C){var _4D=new T(function(){return [0,-1+E(_4s)[1]|0];});return !B(_40(_4u,_4s,_4t,_4D,_4t))?B(_4v(_4C)):[1,[0,_4D,_4t],new T(function(){return B(_4v(_4C));})];})(_Z));},_4E=function(_4F,_4G){while(1){var _4H=E(_4G);if(!_4H[0]){return E(_4F);}else{var _4I=_4F+E(_4H[1])[1]|0;_4G=_4H[2];_4F=_4I;continue;}}},_4J=[0,2],_4K=function(_4L){return E(E(_4L)[1])==1?E(_4J):E(_39);},_4M=function(_4N,_4O){var _4P=function(_4Q){return E(_4N)==1?_4Q<=B(_4E(-2,B(_1B(_4K,_4O)))):_4Q<=B(_4E(-1,B(_1B(_4K,_4O))));};return _4N<=10?E(_4N)==1?B(_4P(114514)):B(_4P(0)):B(_4P(2));},_4R=0,_4S=false,_4T=true,_4U=[0,75],_4V=[1,_4U,_Z],_4W=[0,81],_4X=[1,_4W,_Z],_4Y=[0,74],_4Z=[1,_4Y,_Z],_50=[0,65],_51=[1,_50,_Z],_52=function(_53){var _54=E(_53);switch(_54){case 1:return E(_51);case 11:return E(_4Z);case 12:return E(_4X);case 13:return E(_4V);default:return new F(function(){return _T(0,_54,_Z);});}},_55=function(_56,_57,_58,_59){return new F(function(){return A(_56,[function(_){var _5a=jsSetAttr(E(_57)[1],toJSStr(E(_58)),toJSStr(E(_59)));return _4R;}]);});},_5b=new T(function(){return B(unCStr("td"));}),_5c=function(_5d,_5e,_5f,_){var _5g=jsCreateElem(toJSStr(E(_5b))),_5h=_5g,_5i=jsAppendChild(_5h,E(_5f)[1]),_5j=[0,_5h],_5k=B(A(_5d,[_5e,_5j,_])),_5l=_5k;return _5j;},_5m=function(_5n,_){return new F(function(){return _5c(_c,_Z,_5n,_);});},_5o=function(_5p){return E(_5p);},_5q=new T(function(){return B(unCStr("class"));}),_5r=new T(function(){return B(unCStr("computers-card"));}),_5s=new T(function(){return B(unCStr("your-card"));}),_5t=function(_5u){var _5v=E(_5u);if(!_5v[0]){return E(_5m);}else{var _5w=E(_5v[1]);return function(_5x,_){var _5y=B(_5c(_c,new T(function(){return B(_52(E(_5w[2])[1]));}),_5x,_)),_5z=_5y,_5A=B(A(_55,[_5o,_5z,_5q,new T(function(){return !E(_5w[1])?E(_5r):E(_5s);}),_])),_5B=_5A;return _5z;};}},_5C=new T(function(){return B(unCStr("tr"));}),_5D=function(_5E,_5F,_5G,_){var _5H=jsCreateElem(toJSStr(E(_5C))),_5I=_5H,_5J=jsAppendChild(_5I,E(_5G)[1]),_5K=[0,_5I],_5L=B(A(_5E,[_5F,_5K,_])),_5M=_5L;return _5K;},_5N=function(_5O){return E(_5O);},_5P=function(_5Q){return function(_1X,_1Y){return new F(function(){return _5D(_5N,function(_5R,_){return new F(function(){return _i(new T(function(){return B(_1B(_5t,_5Q));}),_5R,_);});},_1X,_1Y);});};},_5S=function(_5T,_){return _4R;},_5U=new T(function(){return B(unCStr("selectable-hand"));}),_5V=new T(function(){return B(_15("(function(e,c,x){x?e.classList.add(c):e.classList.remove(c);})"));}),_5W=function(_){var _=0;return new F(function(){return A(_15,["false",_]);});},_5X=new T(function(){return B(_11(_5W));}),_5Y=function(_){var _=0;return new F(function(){return A(_15,["true",_]);});},_5Z=new T(function(){return B(_11(_5Y));}),_60=function(_61){return function(_62){return function(_63,_){var _64=B(A(new T(function(){return B(A(new T(function(){return B(A(_5V,[E(E(_61)[1])]));}),[E(toJSStr(E(_62)))]));}),[!E(_63)?E(_5X):E(_5Z),_])),_65=_64;return _4R;};};},_66=function(_67,_){while(1){var _68=E(_67);if(!_68[0]){return _4R;}else{var _69=B(A(_60,[_68[1],_5U,_4T,_])),_6a=_69;_67=_68[2];continue;}}},_6b=new T(function(){return B(unCStr(": empty list"));}),_6c=new T(function(){return B(unCStr("Prelude."));}),_6d=function(_6e){return new F(function(){return err(B(_I(_6c,new T(function(){return B(_I(_6e,_6b));},1))));});},_6f=new T(function(){return B(unCStr("head"));}),_6g=new T(function(){return B(_6d(_6f));}),_6h=function(_6i){var _6j=E(_6i);if(!_6j[0]){return [0];}else{return new F(function(){return _I(_6j[1],new T(function(){return B(_6h(_6j[2]));},1));});}},_6k=new T(function(){return B(_15("(function(e){return e.parentNode;})"));}),_6l=new T(function(){return B(unCStr("td"));}),_6m=function(_6n,_6o){while(1){var _6p=(function(_6q,_6r){var _6s=E(_6r);if(!_6s[0]){return [0];}else{var _6t=_6s[1],_6u=_6s[2];if(!B(A(_6q,[_6t]))){var _6v=_6q;_6o=_6u;_6n=_6v;return null;}else{return [1,_6t,new T(function(){return B(_6m(_6q,_6u));})];}}})(_6n,_6o);if(_6p!=null){return _6p;}}},_6w=function(_6x,_6y,_6z,_6A){var _6B=E(_6z);if(!_6B[0]){return E(_6y);}else{var _6C=E(_6A);if(!_6C[0]){return E(_6y);}else{return new F(function(){return A(_6x,[_6B[1],_6C[1],new T(function(){return B(_6w(_6x,_6y,_6B[2],_6C[2]));})]);});}}},_6D=[0,0],_6E=function(_6F){return E(_6F)[0]==0?true:false;},_6G=function(_6H,_6I){while(1){var _6J=E(_6I);if(!_6J[0]){return E(_6H);}else{_6H=_6J[1];_6I=_6J[2];continue;}}},_6K=new T(function(){return B(unCStr("last"));}),_6L=new T(function(){return B(_6d(_6K));}),_6M=new T(function(){return B(unCStr("table#field"));}),_6N=new T(function(){return B(unCStr("movable-card"));}),_6O=new T(function(){return B(unCStr("id"));}),_6P=new T(function(){return B(unCStr("moving-subject"));}),_6Q=new T(function(){return B(unCStr("motion-scope"));}),_6R=new T(function(){return B(unCStr("obj-of-summon"));}),_6S=new T(function(){return B(unCStr("\u306e\u756a\u3067\u3059\u3001"));}),_6T=new T(function(){return B(unCStr(" ol.hand li"));}),_6U=function(_6V,_6W,_6X,_){if(!E(_6W)){return new F(function(){return A(_6X,[_]);});}else{var _6Y=B(A(_60,[_6V,_5U,_4T,_])),_6Z=_6Y;return new F(function(){return A(_6X,[_]);});}},_70=function(_){return _4R;},_71=new T(function(){return B(unCStr("summonable-zone"));}),_72=function(_73,_74,_75,_){if(!E(_74)){return new F(function(){return A(_75,[_]);});}else{var _76=B(A(_55,[_5o,_73,_5q,_71,_])),_77=_76;return new F(function(){return A(_75,[_]);});}},_78=new T(function(){return B(unCStr("#status"));}),_79=[0,35],_7a=new T(function(){return B(unCStr("#field tr"));}),_7b=[0,0],_7c=function(_7d,_){return _7d;},_7e=function(_7f){return function(_1X,_1Y){return new F(function(){return _i([1,function(_5n,_){return new F(function(){return _1b(_6M,function(_7g,_){var _7h=E(_7g),_7i=jsClearChildren(_7h[1]),_7j=B(_i(new T(function(){return B(_1B(_5P,E(_7f)[4]));}),_7h,_)),_7k=_7j;return _7h;},_5n,_);});},[1,function(_5n,_){return new F(function(){return _1b(_78,function(_7l,_){var _7m=E(_7l),_7n=jsClearChildren(_7m[1]),_7o=B(A(new T(function(){var _7p=E(_7f),_7q=_7p[1],_7r=E(_7p[3]);if(_7r[0]==6){var _7s=function(_1X,_1Y){return new F(function(){return _c(new T(function(){return !E(_7r[1])?E(_s):E(_r);}),_1X,_1Y);});};}else{var _7s=function(_1X,_1Y){return new F(function(){return _c(new T(function(){return B(unAppCStr("-- ",new T(function(){var _7t=new T(function(){return B(_I(_6S,new T(function(){var _7u=E(_7r);switch(_7u[0]){case 0:var _7v=E(_q);break;case 1:var _7v=E(_y);break;case 2:var _7v=E(_x);break;case 3:var _7w=function(_7x){var _7y=E(_7x);return _7y[0]==0?E(new T(function(){return B(_I(B(_T(0,E(_7u[1])[1],_Z)),_w));})):[1,_7y[1],new T(function(){return B(_7w(_7y[2]));})];},_7v=B(_7w(_v));break;case 4:var _7v=E(_u);break;default:var _7v=E(_t);}return _7v;},1)));},1);if(!E(_7p[2])){var _7z=B(_I(E(E(_7q)[2])[3],_7t));}else{var _7z=B(_I(E(E(_7q)[1])[3],_7t));}return _7z;})));}),_1X,_1Y);});};}var _7A=_7s;return _7A;}),[_7m,_])),_7B=_7o;return _7m;},_5n,_);});},[1,function(_7C,_){var _7D=E(new T(function(){var _7E=E(E(_7f)[1]);return [0,new T(function(){return B(_1F(_7E[1]));}),new T(function(){return B(_1F(_7E[2]));})];})),_7F=B(A(_7D[1],[_7C,_])),_7G=_7F,_7H=B(A(_7D[2],[_7C,_])),_7I=_7H;return _7C;},[1,new T(function(){var _7J=E(_7f),_7K=_7J[1],_7L=_7J[2],_7M=_7J[4],_7N=E(_7J[3]);switch(_7N[0]){case 1:var _7O=function(_7P){var _7Q=E(_7P);if(!_7Q[0]){return E(_5S);}else{var _7R=function(_7S){var _7T=E(_7S);if(!_7T[0]){return E(new T(function(){return B(_7O(_7Q[2]));}));}else{var _7U=new T(function(){return B(_7R(_7T[2]));});return function(_7V,_){var _7W=E(_7V);if(!_7W[0]){return _4R;}else{var _7X=_7W[2],_7Y=E(_7W[1]);if(!_7Y[0]){return new F(function(){return A(_7U,[_7X,_]);});}else{var _7Z=E(_7T[1]),_80=_7Z[1],_81=B(A(_6k,[E(_80),_])),_82=_81,_83=B(_2s(_6D,_82,_)),_84=_83,_85=B(_2s(_7b,_80,_)),_86=_85;if(!E(E(_7Y[1])[1])){if(!E(_7L)){if(!B(_4p(_4S,_7M,_84,_86))[0]){return new F(function(){return A(_7U,[_7X,_]);});}else{var _87=B(A(_60,[_7Z,_6N,_4T,_])),_88=_87;return new F(function(){return A(_7U,[_7X,_]);});}}else{return new F(function(){return A(_7U,[_7X,_]);});}}else{if(!E(_7L)){return new F(function(){return A(_7U,[_7X,_]);});}else{if(!B(_4p(_4T,_7M,_84,_86))[0]){return new F(function(){return A(_7U,[_7X,_]);});}else{var _89=B(A(_60,[_7Z,_6N,_4T,_])),_8a=_89;return new F(function(){return A(_7U,[_7X,_]);});}}}}}};}};return new F(function(){return _7R(_7Q[1]);});}},_8b=function(_8c,_){var _8d=E(_8c),_8e=_8d[1],_8f=jsQuerySelectorAll(_8e,toJSStr([1,_79,new T(function(){if(!E(_7L)){var _8g=B(_I(E(E(_7K)[2])[4],_6T));}else{var _8g=B(_I(E(E(_7K)[1])[4],_6T));}return _8g;})])),_8h=_8f,_8i=B(A(_6w,[_6U,_70,_8h,new T(function(){var _8j=function(_8k){return new F(function(){return (function(_8l){if(!E(_7L)){return new F(function(){return _4M(_8l,E(E(_7K)[2])[1]);});}else{return new F(function(){return _4M(_8l,E(E(_7K)[1])[1]);});}})(E(_8k)[1]);});};if(!E(_7L)){var _8m=B(_1B(_8j,E(E(_7K)[2])[1]));}else{var _8m=B(_1B(_8j,E(E(_7K)[1])[1]));}return _8m;}),_])),_8n=_8i,_8o=jsQuerySelectorAll(_8e,toJSStr(E(_7a))),_8p=_8o,_8q=E(_8p);if(!_8q[0]){return _8d;}else{var _8r=E(_6l),_8s=jsQuerySelectorAll(E(_8q[1])[1],toJSStr(_8r)),_8t=_8s,_8u=function(_8v,_){var _8w=E(_8v);if(!_8w[0]){return _Z;}else{var _8x=jsQuerySelectorAll(E(_8w[1])[1],toJSStr(_8r)),_8y=_8x,_8z=B(_8u(_8w[2],_)),_8A=_8z;return [1,_8y,_8A];}},_8B=B(_8u(_8q[2],_)),_8C=_8B,_8D=B(A(function(_8E,_8F){var _8G=function(_8H){var _8I=E(_8H);if(!_8I[0]){return E(new T(function(){return B(_7O(_8F));}));}else{var _8J=new T(function(){return B(_8G(_8I[2]));});return function(_8K,_){var _8L=E(_8K);if(!_8L[0]){return _4R;}else{var _8M=_8L[2],_8N=E(_8L[1]);if(!_8N[0]){return new F(function(){return A(_8J,[_8M,_]);});}else{var _8O=E(_8I[1]),_8P=_8O[1],_8Q=B(A(_6k,[E(_8P),_])),_8R=_8Q,_8S=B(_2s(_6D,_8R,_)),_8T=_8S,_8U=B(_2s(_7b,_8P,_)),_8V=_8U;if(!E(E(_8N[1])[1])){if(!E(_7L)){if(!B(_4p(_4S,_7M,_8T,_8V))[0]){return new F(function(){return A(_8J,[_8M,_]);});}else{var _8W=B(A(_60,[_8O,_6N,_4T,_])),_8X=_8W;return new F(function(){return A(_8J,[_8M,_]);});}}else{return new F(function(){return A(_8J,[_8M,_]);});}}else{if(!E(_7L)){return new F(function(){return A(_8J,[_8M,_]);});}else{if(!B(_4p(_4T,_7M,_8T,_8V))[0]){return new F(function(){return A(_8J,[_8M,_]);});}else{var _8Y=B(A(_60,[_8O,_6N,_4T,_])),_8Z=_8Y;return new F(function(){return A(_8J,[_8M,_]);});}}}}}};}};return new F(function(){return _8G(_8E);});},[_8t,_8C,new T(function(){return B(_6h(_7M));}),_])),_90=_8D;return _8d;}};break;case 2:var _91=_7N[1],_8b=function(_92,_){var _93=E(_92),_94=jsQuerySelectorAll(_93[1],toJSStr(E(_7a))),_95=_94,_96=_95,_97=E(_91),_98=E(_97[1])[1];if(_98>=0){var _99=E(_6l),_9a=jsQuerySelectorAll(B(_4(_96,_98))[1],toJSStr(_99)),_9b=_9a,_9c=E(_97[2])[1];if(_9c>=0){var _9d=jsSetAttr(B(_4(_9b,_9c))[1],toJSStr(E(_6O)),toJSStr(E(_6P))),_9e=function(_,_9f){var _9g=B((function(_9h,_){while(1){var _9i=(function(_9j,_){var _9k=E(_9j);if(!_9k[0]){return _4R;}else{var _9l=_9k[1],_9m=B(A(_60,[new T(function(){return B(_2G(new T(function(){return E(B(A(_9l,[_97]))[1]);}),_9,function(_9n){return new F(function(){return _2G(new T(function(){return E(B(A(_9l,[_97]))[2]);}),_9,_2V,_9n);});},_9f));},1),_6Q,_4T,_])),_9o=_9m;_9h=_9k[2];return null;}})(_9h,_);if(_9i!=null){return _9i;}}})(new T(function(){var _9p=function(_9q){var _9r=new T(function(){return B(A(_9q,[_91]));});return new F(function(){return _40(_7J,new T(function(){return E(E(_91)[1]);}),new T(function(){return E(E(_91)[2]);}),new T(function(){return E(E(_9r)[1]);}),new T(function(){return E(E(_9r)[2]);}));});};return !E(_7L)?B(_6m(_9p,_3k)):B(_6m(_9p,_37));}),_)),_9s=_9g;return _93;},_9t=E(_96);if(!_9t[0]){return new F(function(){return _9e(_,_Z);});}else{var _9u=jsQuerySelectorAll(E(_9t[1])[1],toJSStr(_99)),_9v=_9u,_9w=function(_9x,_){var _9y=E(_9x);if(!_9y[0]){return _Z;}else{var _9z=jsQuerySelectorAll(E(_9y[1])[1],toJSStr(_99)),_9A=_9z,_9B=B(_9w(_9y[2],_)),_9C=_9B;return [1,_9A,_9C];}},_9D=B(_9w(_9t[2],_)),_9E=_9D;return new F(function(){return _9e(_,[1,_9v,_9E]);});}}else{return E(_1);}}else{return E(_1);}};break;case 3:var _8b=function(_9F,_){var _9G=E(_9F),_9H=jsQuerySelectorAll(_9G[1],toJSStr([1,_79,new T(function(){if(!E(_7L)){var _9I=B(_I(E(E(_7K)[2])[4],_6T));}else{var _9I=B(_I(E(E(_7K)[1])[4],_6T));}return _9I;})])),_9J=_9H,_9K=B(_66(_9J,_)),_9L=_9K;return _9G;};break;case 4:var _8b=function(_9M,_){var _9N=E(_9M),_9O=_9N[1],_9P=jsQuerySelectorAll(_9O,toJSStr([1,_79,new T(function(){if(!E(_7L)){var _9Q=B(_I(E(E(_7K)[2])[4],_6T));}else{var _9Q=B(_I(E(E(_7K)[1])[4],_6T));}return _9Q;})])),_9R=_9P,_9S=E(_7N[1])[1];if(_9S>=0){var _9T=jsSetAttr(B(_4(_9R,_9S))[1],toJSStr(E(_6O)),toJSStr(E(_6R))),_9U=jsQuerySelectorAll(_9O,toJSStr(E(_7a))),_9V=_9U,_9W=_9V,_9X=function(_9Y){var _9Z=jsQuerySelectorAll(_9Y,toJSStr(E(_6l))),_a0=_9Z,_a1=B(A(_6w,[_72,_70,_a0,new T(function(){if(!E(_7L)){var _a2=E(_7M),_a3=_a2[0]==0?E(_6g):B(_1B(_6E,_a2[1]));}else{var _a4=E(_7M);if(!_a4[0]){var _a5=E(_6L);}else{var _a5=B(_1B(_6E,B(_6G(_a4[1],_a4[2]))));}var _a3=_a5;}return _a3;}),_])),_a6=_a1;return _9N;};if(!E(_7L)){var _a7=E(_9W);if(!_a7[0]){return E(_6g);}else{return new F(function(){return _9X(E(_a7[1])[1]);});}}else{var _a8=E(_9W);if(!_a8[0]){return E(_6L);}else{return new F(function(){return _9X(B(_6G(_a8[1],_a8[2]))[1]);});}}}else{return E(_1);}};break;default:var _8b=E(_7c);}var _a9=_8b;return _a9;}),_Z]]]],_1X,_1Y);});};},_aa=new T(function(){return B(unCStr("Prelude.Enum.pred{Int}: tried to take `pred\' of minBound"));}),_ab=new T(function(){return B(err(_aa));}),_ac=function(_ad,_ae){return [0,imul(E(_ad)[1],E(_ae)[1])|0];},_af=function(_ag,_ah){return [0,E(_ag)[1]-E(_ah)[1]|0];},_ai=function(_aj){var _ak=E(_aj),_al=_ak[1];return _al<0?[0, -_al]:E(_ak);},_am=function(_an){var _ao=E(_an);return _ao[0]==0?E(_ao[1]):I_toInt(_ao[1]);},_ap=function(_aq){return [0,B(_am(_aq))];},_ar=function(_as){return [0, -E(_as)[1]];},_at=[0,-1],_au=[0,0],_av=[0,1],_aw=function(_ax){var _ay=E(_ax)[1];return _ay>=0?E(_ay)==0?E(_au):E(_av):E(_at);},_az=[0,_3c,_ac,_af,_ar,_ai,_aw,_ap],_aA=[0,1],_aB=function(_aC,_aD){return [0,E(_aC)[1],E(_aD)[1]];},_aE=function(_aF,_aG){var _aH=quot(_aG,52774),_aI=(imul(40692,_aG-(imul(_aH,52774)|0)|0)|0)-(imul(_aH,3791)|0)|0,_aJ=new T(function(){if(_aI>=0){var _aK=[0,_aI];}else{var _aK=[0,_aI+2147483399|0];}var _aL=_aK;return _aL;}),_aM=quot(_aF,53668),_aN=(imul(40014,_aF-(imul(_aM,53668)|0)|0)|0)-(imul(_aM,12211)|0)|0,_aO=new T(function(){if(_aN>=0){var _aP=[0,_aN];}else{var _aP=[0,_aN+2147483563|0];}var _aQ=_aP;return _aQ;});return [0,new T(function(){var _aR=E(_aO)[1]-E(_aJ)[1]|0;if(_aR>=1){var _aS=[0,_aR];}else{var _aS=[0,_aR+2147483562|0];}var _aT=_aS,_aU=_aT,_aV=_aU,_aW=_aV;return _aW;}),new T(function(){return B(_aB(_aO,_aJ));})];},_aX=[0,2147483562],_aY=new T(function(){return B(unCStr("base"));}),_aZ=new T(function(){return B(unCStr("GHC.Exception"));}),_b0=new T(function(){return B(unCStr("ArithException"));}),_b1=[0,I_fromBits([4194982440,719304104]),I_fromBits([3110813675,1843557400]),_aY,_aZ,_b0],_b2=[0,I_fromBits([4194982440,719304104]),I_fromBits([3110813675,1843557400]),_b1,_Z],_b3=function(_b4){return E(_b2);},_b5=function(_b6){return E(E(_b6)[1]);},_b7=function(_b8,_b9,_ba){var _bb=B(A(_b8,[_])),_bc=B(A(_b9,[_])),_bd=hs_eqWord64(_bb[1],_bc[1]),_be=_bd;if(!E(_be)){return [0];}else{var _bf=hs_eqWord64(_bb[2],_bc[2]),_bg=_bf;return E(_bg)==0?[0]:[1,_ba];}},_bh=function(_bi){var _bj=E(_bi);return new F(function(){return _b7(B(_b5(_bj[1])),_b3,_bj[2]);});},_bk=new T(function(){return B(unCStr("arithmetic underflow"));}),_bl=new T(function(){return B(unCStr("arithmetic overflow"));}),_bm=new T(function(){return B(unCStr("Ratio has zero denominator"));}),_bn=new T(function(){return B(unCStr("denormal"));}),_bo=new T(function(){return B(unCStr("divide by zero"));}),_bp=new T(function(){return B(unCStr("loss of precision"));}),_bq=function(_br){switch(E(_br)){case 0:return E(_bl);case 1:return E(_bk);case 2:return E(_bp);case 3:return E(_bo);case 4:return E(_bn);default:return E(_bm);}},_bs=function(_bt){return new F(function(){return _I(_bk,_bt);});},_bu=function(_bt){return new F(function(){return _I(_bl,_bt);});},_bv=function(_bt){return new F(function(){return _I(_bm,_bt);});},_bw=function(_bt){return new F(function(){return _I(_bn,_bt);});},_bx=function(_bt){return new F(function(){return _I(_bo,_bt);});},_by=function(_bt){return new F(function(){return _I(_bp,_bt);});},_bz=function(_bA){switch(E(_bA)){case 0:return E(_bu);case 1:return E(_bs);case 2:return E(_by);case 3:return E(_bx);case 4:return E(_bw);default:return E(_bv);}},_bB=[0,44],_bC=[0,93],_bD=[0,91],_bE=function(_bF,_bG,_bH){var _bI=E(_bG);return _bI[0]==0?B(unAppCStr("[]",_bH)):[1,_bD,new T(function(){return B(A(_bF,[_bI[1],new T(function(){var _bJ=function(_bK){var _bL=E(_bK);return _bL[0]==0?E([1,_bC,_bH]):[1,_bB,new T(function(){return B(A(_bF,[_bL[1],new T(function(){return B(_bJ(_bL[2]));})]));})];};return B(_bJ(_bI[2]));})]));})];},_bM=function(_bN,_bO){return new F(function(){return _bE(_bz,_bN,_bO);});},_bP=function(_bQ,_bR){switch(E(_bR)){case 0:return E(_bu);case 1:return E(_bs);case 2:return E(_by);case 3:return E(_bx);case 4:return E(_bw);default:return E(_bv);}},_bS=[0,_bP,_bq,_bM],_bT=new T(function(){return [0,_b3,_bS,_bU,_bh];}),_bU=function(_bt){return [0,_bT,_bt];},_bV=3,_bW=new T(function(){return B(_bU(_bV));}),_bX=new T(function(){return die(_bW);}),_bY=function(_bZ,_c0){var _c1=E(_bZ);if(!_c1[0]){var _c2=_c1[1],_c3=E(_c0);return _c3[0]==0?_c2==_c3[1]:I_compareInt(_c3[1],_c2)==0?true:false;}else{var _c4=_c1[1],_c5=E(_c0);return _c5[0]==0?I_compareInt(_c4,_c5[1])==0?true:false:I_compare(_c4,_c5[1])==0?true:false;}},_c6=function(_c7){return E(E(_c7)[7]);},_c8=function(_c9,_ca){var _cb=E(_c9);if(!_cb[0]){var _cc=_cb[1],_cd=E(_ca);return _cd[0]==0?_cc>=_cd[1]:I_compareInt(_cd[1],_cc)<=0;}else{var _ce=_cb[1],_cf=E(_ca);return _cf[0]==0?I_compareInt(_ce,_cf[1])>=0:I_compare(_ce,_cf[1])>=0;}},_cg=[0,0],_ch=function(_ci,_cj){var _ck=E(_ci);if(!_ck[0]){var _cl=_ck[1],_cm=E(_cj);return _cm[0]==0?_cl>_cm[1]:I_compareInt(_cm[1],_cl)<0;}else{var _cn=_ck[1],_co=E(_cj);return _co[0]==0?I_compareInt(_cn,_co[1])>0:I_compare(_cn,_co[1])>0;}},_cp=[0,1000],_cq=function(_cr,_cs){while(1){var _ct=E(_cr);if(!_ct[0]){var _cu=_ct[1],_cv=E(_cs);if(!_cv[0]){var _cw=_cv[1],_cx=subC(_cu,_cw);if(!E(_cx[2])){return [0,_cx[1]];}else{_cr=[1,I_fromInt(_cu)];_cs=[1,I_fromInt(_cw)];continue;}}else{_cr=[1,I_fromInt(_cu)];_cs=_cv;continue;}}else{var _cy=E(_cs);if(!_cy[0]){_cr=_ct;_cs=[1,I_fromInt(_cy[1])];continue;}else{return [1,I_sub(_ct[1],_cy[1])];}}}},_cz=function(_cA,_cB){var _cC=_cA%_cB;if(_cA<=0){if(_cA>=0){return E(_cC);}else{if(_cB<=0){return E(_cC);}else{var _cD=E(_cC);return _cD==0?0:_cD+_cB|0;}}}else{if(_cB>=0){if(_cA>=0){return E(_cC);}else{if(_cB<=0){return E(_cC);}else{var _cE=E(_cC);return _cE==0?0:_cE+_cB|0;}}}else{var _cF=E(_cC);return _cF==0?0:_cF+_cB|0;}}},_cG=function(_cH,_cI){while(1){var _cJ=E(_cH);if(!_cJ[0]){var _cK=E(_cJ[1]);if(_cK==(-2147483648)){_cH=[1,I_fromInt(-2147483648)];continue;}else{var _cL=E(_cI);if(!_cL[0]){return [0,B(_cz(_cK,_cL[1]))];}else{_cH=[1,I_fromInt(_cK)];_cI=_cL;continue;}}}else{var _cM=_cJ[1],_cN=E(_cI);return _cN[0]==0?[0,I_toInt(I_mod(_cM,I_fromInt(_cN[1])))]:[1,I_mod(_cM,_cN[1])];}}},_cO=function(_cP,_cQ){while(1){var _cR=E(_cP);if(!_cR[0]){var _cS=_cR[1],_cT=E(_cQ);if(!_cT[0]){var _cU=_cT[1],_cV=addC(_cS,_cU);if(!E(_cV[2])){return [0,_cV[1]];}else{_cP=[1,I_fromInt(_cS)];_cQ=[1,I_fromInt(_cU)];continue;}}else{_cP=[1,I_fromInt(_cS)];_cQ=_cT;continue;}}else{var _cW=E(_cQ);if(!_cW[0]){_cP=_cR;_cQ=[1,I_fromInt(_cW[1])];continue;}else{return [1,I_add(_cR[1],_cW[1])];}}}},_cX=function(_cY){return [0,_cY];},_cZ=function(_d0,_d1){while(1){var _d2=E(_d0);if(!_d2[0]){var _d3=_d2[1],_d4=E(_d1);if(!_d4[0]){var _d5=_d4[1];if(!(imul(_d3,_d5)|0)){return [0,imul(_d3,_d5)|0];}else{_d0=[1,I_fromInt(_d3)];_d1=[1,I_fromInt(_d5)];continue;}}else{_d0=[1,I_fromInt(_d3)];_d1=_d4;continue;}}else{var _d6=E(_d1);if(!_d6[0]){_d0=_d2;_d1=[1,I_fromInt(_d6[1])];continue;}else{return [1,I_mul(_d2[1],_d6[1])];}}}},_d7=function(_d8,_d9,_da,_db){while(1){var _dc=(function(_dd,_de,_df,_dg){if(!B(_ch(_de,_df))){var _dh=B(_cO(B(_cq(_df,_de)),_aA)),_di=B((function(_dj,_dk,_dl){while(1){if(!B(_c8(_dj,B(_cZ(_dh,_cp))))){var _dm=E(_dl),_dn=B(_aE(_dm[1],_dm[2])),_do=B(_cZ(_dj,_aX)),_dp=B(_cO(B(_cZ(_dk,_aX)),B(_cq(B(_cX(E(_dn[1])[1])),_aA))));_dl=_dn[2];_dj=_do;_dk=_dp;continue;}else{return [0,_dk,_dl];}}})(_aA,_cg,_dg));return [0,new T(function(){return B(A(_c6,[_dd,new T(function(){if(!B(_bY(_dh,_cg))){var _dq=B(_cO(_de,B(_cG(_di[1],_dh))));}else{var _dq=E(_bX);}return _dq;})]));}),_di[2]];}else{var _dr=_dd,_ds=_df,_dt=_de,_du=_dg;_d8=_dr;_d9=_ds;_da=_dt;_db=_du;return null;}})(_d8,_d9,_da,_db);if(_dc!=null){return _dc;}}},_dv=function(_dw){var _dx=E(_dw);if(_dx==(-2147483648)){return E(_ab);}else{var _dy=_dx-1|0;if(0<=_dy){var _dz=function(_dA){return [1,[0,[0,_dA]],new T(function(){if(_dA!=_dy){var _dB=B(_dz(_dA+1|0));}else{var _dB=[0];}var _dC=_dB;return _dC;})];};return new F(function(){return _dz(0);});}else{return [0];}}},_dD=function(_dE,_dF){var _dG=E(_dF);return _dG[0]==0?[0]:[1,_dE,new T(function(){return B(_dD(_dG[1],_dG[2]));})];},_dH=new T(function(){return B(unCStr("init"));}),_dI=new T(function(){return B(_6d(_dH));}),_dJ=new T(function(){return B(unCStr("tail"));}),_dK=new T(function(){return B(_6d(_dJ));}),_dL=function(_dM,_dN,_dO,_dP){return new F(function(){return A(_dN,[function(_dQ){if(!E(_dM)){return [1,_dQ,new T(function(){var _dR=E(_dP);return _dR[0]==0?E(_dK):E(_dR[2]);})];}else{var _dS=E(_dP);if(!_dS[0]){return E(_dI);}else{return new F(function(){return _I(B(_dD(_dS[1],_dS[2])),[1,_dQ,_Z]);});}}},new T(function(){return B(A(_dO,[new T(function(){if(!E(_dM)){var _dT=E(_dP),_dU=_dT[0]==0?E(_6g):E(_dT[1]);}else{var _dV=E(_dP),_dU=_dV[0]==0?E(_6L):B(_6G(_dV[1],_dV[2]));}return _dU;})]));})]);});},_dW=function(_dX){return [0,_dX];},_dY=function(_dX){return [1,_dX];},_dZ=function(_e0){while(1){var _e1=(function(_e2){var _e3=E(_e2);if(!_e3[0]){return [0];}else{var _e4=_e3[2],_e5=E(_e3[1]);if(!_e5[0]){_e0=_e4;return null;}else{return [1,_e5[1],new T(function(){return B(_dZ(_e4));})];}}})(_e0);if(_e1!=null){return _e1;}}},_e6=function(_e7,_e8){if(_e7<=_e8){var _e9=function(_ea){return [1,[0,_ea],new T(function(){if(_ea!=_e8){var _eb=B(_e9(_ea+1|0));}else{var _eb=[0];}var _ec=_eb;return _ec;})];};return new F(function(){return _e9(_e7);});}else{return [0];}},_ed=function(_ee,_ef){var _eg=E(_ee);if(!_eg[0]){return E(_6g);}else{var _eh=B(_D(_eg[1],0));if(_eh==(-2147483648)){return E(_ab);}else{var _ei=_eh-1|0;if(0<=_ei){var _ej=function(_ek){return [1,new T(function(){var _el=[0,_ek],_em=function(_en){var _eo=E(_en);if(!_eo[0]){return [0];}else{var _ep=_eo[1];return [1,new T(function(){return B(A(_ef,[_ep,_el,new T(function(){return B(_2G(_ep,_9,function(_eq){return new F(function(){return _2G(_el,_9,_2V,_eq);});},_eg));})]));}),new T(function(){return B(_em(_eo[2]));})];}};return B(_em(new T(function(){var _er=B(_D(_eg,0));if(_er==(-2147483648)){var _es=E(_ab);}else{var _es=B(_e6(0,_er-1|0));}return _es;})));}),new T(function(){if(_ek!=_ei){var _et=B(_ej(_ek+1|0));}else{var _et=[0];}var _eu=_et;return _eu;})];};return new F(function(){return _ej(0);});}else{return [0];}}}},_ev=function(_ew){var _ex=E(_ew);return !E(_ex[2])?[0]:[1,_ex[1]];},_ey=new T(function(){return B(_e6(0,2147483647));}),_ez=[0,0],_eA=function(_eB){var _eC=E(_eB);return E(_eC[2])[0]==0?[1,_eC[1]]:[0];},_eD=function(_eE,_eF){while(1){var _eG=(function(_eH,_eI){var _eJ=E(_eI);if(!_eJ[0]){return [0];}else{var _eK=_eJ[2],_eL=B(A(_eH,[_eJ[1]]));if(!_eL[0]){var _eM=_eH;_eF=_eK;_eE=_eM;return null;}else{return [1,_eL[1],new T(function(){return B(_eD(_eH,_eK));})];}}})(_eE,_eF);if(_eG!=null){return _eG;}}},_eN=function(_eO,_eP){var _eQ=E(_eO);if(!_eQ[0]){return [0];}else{var _eR=E(_eP);return _eR[0]==0?[0]:[1,[0,_eQ[1],_eR[1]],new T(function(){return B(_eN(_eQ[2],_eR[2]));})];}},_eS=function(_eT,_eU,_eV,_eW){var _eX=E(_eV);switch(_eX[0]){case 1:var _eY=function(_eZ){var _f0=E(_eZ);return _f0[0]==0?E(new T(function(){return B(_1B(_dW,B(_eD(_ev,B(_eN(_ey,new T(function(){if(!E(_eU)){var _f1=E(E(_eT)[2])[1],_f2=B(_1B(function(_f3){return new F(function(){return _4M(E(_f3)[1],_f1);});},_f1));}else{var _f4=E(E(_eT)[1])[1],_f2=B(_1B(function(_f5){return new F(function(){return _4M(E(_f5)[1],_f4);});},_f4));}return _f2;},1)))))));})):[1,[1,_f0[1]],new T(function(){return B(_eY(_f0[2]));})];};return new F(function(){return _eY(B(_dZ(B(_6h(B(_ed(_eW,function(_f6,_f7,_f8){var _f9=E(_f8);if(!_f9[0]){return [0];}else{var _fa=function(_fb){return B(_4p(_eU,_eW,_f6,_f7))[0]==0?[0]:[1,[0,_f6,_f7]];};return !E(E(_f9[1])[1])?!E(_eU)?B(_fa(_)):[0]:!E(_eU)?[0]:B(_fa(_));}})))))));});break;case 2:var _fc=E(_eX[1]);return new F(function(){return _1B(_dY,B(_4p(_eU,_eW,_fc[1],_fc[2])));});break;case 3:if(!E(_eU)){return new F(function(){return _dv(B(_D(E(E(_eT)[2])[1],0)));});}else{return new F(function(){return _dv(B(_D(E(E(_eT)[1])[1],0)));});}break;case 4:return new F(function(){return _1B(function(_fd){return [1,[0,new T(function(){if(!E(_eU)){var _fe=E(_ez);}else{var _ff=B(_D(_eW,0));if(_ff==(-2147483648)){var _fg=E(_ab);}else{var _fg=[0,_ff-1|0];}var _fe=_fg;}return _fe;}),_fd]];},B(_eD(_eA,B(_eN(_ey,new T(function(){return B(_dL(_eU,_9,_2V,_eW));},1))))));});break;default:return [0];}},_fh=function(_fi){var _fj=E(_fi);return new F(function(){return _eS(_fj[1],_fj[2],_fj[3],_fj[4]);});},_fk=[0,0],_fl=function(_fm){var _fn=new T(function(){var _fo=B(_D(B(_fh(_fm)),0));if(_fo==(-2147483648)){var _fp=E(_ab);}else{var _fq=B(_d7(_az,_fk,B(_cX(_fo-1|0)),new T(function(){return E(E(_fm)[5]);}))),_fp=[0,_fq[1],_fq[2]];}var _fr=_fp;return _fr;});return [0,new T(function(){return B(_2G(new T(function(){return E(E(_fn)[1]);}),_9,_2V,new T(function(){return B(_fh(_fm));})));}),new T(function(){return E(E(_fn)[2]);})];},_fs=[1],_ft=function(_fu,_fv){return new F(function(){return A(_fu,[_fv]);});},_fw=[5],_fx=[6,_4S],_fy=[6,_4T],_fz=function(_fA){return [0];},_fB=function(_fC,_fD,_fE,_fF,_fG){if(!B(_40(_fG,_fC,_fD,_fE,_fF))){return [0];}else{var _fH=B(_2G(_fC,_9,function(_fI){return new F(function(){return _2G(_fD,_9,_2V,_fI);});},new T(function(){return E(E(_fG)[4]);})));return _fH[0]==0?[0]:[1,new T(function(){var _fJ=E(_fG),_fK=_fJ[1],_fL=_fJ[4],_fM=_fJ[5],_fN=new T(function(){return B(_2G(_fC,_ft,function(_fO){return new F(function(){return _2G(_fD,_ft,_fz,_fO);});},new T(function(){return B(_2G(_fE,_ft,function(_fP){return new F(function(){return _2G(_fF,_ft,function(_fQ){return [1,_fH[1]];},_fP);});},_fL));})));});if(!E(_fJ[2])){var _fR=(E(_fE)[1]+1|0)!=B(_D(_fL,0))?[0,_fK,_4S,_fw,_fN,_fM]:[0,_fK,_4S,_fx,_fN,_fM];}else{var _fR=E(E(_fE)[1])==0?[0,_fK,_4T,_fy,_fN,_fM]:[0,_fK,_4T,_fw,_fN,_fM];}var _fS=_fR;return _fS;})];}},_fT=function(_fU,_fV,_fW){var _fX=B(_2G(_fU,_9,function(_fY){return new F(function(){return _2G(_fV,_9,_2V,_fY);});},new T(function(){return E(E(_fW)[4]);})));if(!_fX[0]){return [0];}else{var _fZ=_fX[1],_g0=E(_fW),_g1=_g0[2],_g2=_g0[4],_g3=function(_g4){return B(_4p(_g1,_g2,_fU,_fV))[0]==0?[0]:[1,[0,_g0[1],_g1,[2,[0,_fU,_fV]],_g2,_g0[5]]];};return !E(_g1)?!E(E(_fZ)[1])?B(_g3(_)):[0]:!E(E(_fZ)[1])?[0]:B(_g3(_));}},_g5=[0,114514],_g6=function(_g7,_g8){var _g9=new T(function(){var _ga=_g7+1|0;return _ga>=0?B(_2B(_ga,_g8)):E(_g8);});if(_g7>0){var _gb=function(_gc,_gd){var _ge=E(_gc);if(!_ge[0]){return E(_g9);}else{var _gf=_ge[1];return _gd>1?[1,_gf,new T(function(){return B(_gb(_ge[2],_gd-1|0));})]:[1,_gf,_g9];}};return new F(function(){return _gb(_g8,_g7);});}else{return E(_g9);}},_gg=function(_gh,_gi){return new F(function(){return _g6(E(_gh)[1],_gi);});},_gj=function(_gk,_gl,_gm,_gn){return B(_2G(_gl,_9,_2V,new T(function(){var _go=E(_gn),_gp=_go[4];if(!E(_go[2])){var _gq=E(_gp),_gr=_gq[0]==0?E(_6g):E(_gq[1]);}else{var _gs=E(_gp),_gr=_gs[0]==0?E(_6L):B(_6G(_gs[1],_gs[2]));}var _gt=_gr;return _gt;})))[0]==0?[1,new T(function(){var _gu=E(_gm),_gv=_gu[1],_gw=function(_gx){var _gy=E(_gn),_gz=_gy[1],_gA=_gy[2],_gB=_gy[4],_gC=_gy[5],_gD=new T(function(){var _gE=new T(function(){return B(_2G(_gl,_ft,function(_gF){return E([1,[0,_gA,_gu]]);},new T(function(){if(!E(_gA)){var _gG=E(_gB),_gH=_gG[0]==0?E(_6g):E(_gG[1]);}else{var _gI=E(_gB),_gH=_gI[0]==0?E(_6L):B(_6G(_gI[1],_gI[2]));}return _gH;})));});if(!E(_gA)){var _gJ=[1,_gE,new T(function(){var _gK=E(_gB);return _gK[0]==0?E(_dK):E(_gK[2]);})];}else{var _gL=E(_gB);if(!_gL[0]){var _gM=E(_dI);}else{var _gM=B(_I(B(_dD(_gL[1],_gL[2])),[1,_gE,_Z]));}var _gJ=_gM;}return _gJ;}),_gN=new T(function(){if(!E(_gA)){var _gO=E(_gz),_gP=[0,_gO[1],new T(function(){var _gQ=E(_gO[2]);return [0,new T(function(){return B(_gg(_gk,_gQ[1]));}),_gQ[2],_gQ[3],_gQ[4],_gQ[5]];})];}else{var _gR=E(_gz),_gP=[0,new T(function(){var _gS=E(_gR[1]);return [0,new T(function(){return B(_gg(_gk,_gS[1]));}),_gS[2],_gS[3],_gS[4],_gS[5]];}),_gR[2]];}return _gP;});return E(_gx)==0?[0,_gN,_gA,_fw,_gD,_gC]:[0,_gN,_gA,[3,new T(function(){return _gv<=10?E(_gv)==1?E(_g5):E(_38):E(_4J);})],_gD,_gC];};if(_gv<=10){if(E(_gv)==1){var _gT=B(_gw(114514)),_gU=[0,_gT[1],_gT[2],_gT[3],_gT[4],_gT[5]];}else{var _gV=B(_gw(0)),_gU=[0,_gV[1],_gV[2],_gV[3],_gV[4],_gV[5]];}var _gW=_gU;}else{var _gX=B(_gw(2)),_gW=[0,_gX[1],_gX[2],_gX[3],_gX[4],_gX[5]];}var _gY=_gW,_gZ=_gY;return _gZ;})]:[0];},_h0=function(_h1,_h2,_h3,_h4,_h5,_h6,_h7){var _h8=[0,_h3,_h4,_h5,_h6,_h7],_h9=E(_h5);switch(_h9[0]){case 1:var _ha=B(_fT(_h1,_h2,_h8));return _ha[0]==0?E(_h8):E(_ha[1]);case 2:var _hb=E(_h9[1]),_hc=_hb[2],_hd=E(_hb[1]),_he=E(_h1);if(_hd[1]!=_he[1]){var _hf=B(_fB(_hd,_hc,_he,_h2,_h8));return _hf[0]==0?E(_h8):E(_hf[1]);}else{var _hg=E(_hc),_hh=E(_h2);if(_hg[1]!=_hh[1]){var _hi=B(_fB(_hd,_hg,_he,_hh,_h8));return _hi[0]==0?E(_h8):E(_hi[1]);}else{return [0,_h3,_h4,_fs,_h6,_h7];}}break;case 4:var _hj=_h9[1],_hk=function(_hl){var _hm=function(_hn){var _ho=B(_gj(_hj,_h2,new T(function(){var _hp=E(_hj)[1];if(_hp>=0){if(!E(_h4)){var _hq=B(_4(E(E(_h3)[2])[1],_hp));}else{var _hq=B(_4(E(E(_h3)[1])[1],_hp));}var _hr=_hq;}else{var _hr=E(_1);}var _hs=_hr,_ht=_hs;return _ht;},1),_h8));return _ho[0]==0?E(_h8):E(_ho[1]);};if(!E(_h4)){return E(E(_h1)[1])==0?B(_hm(_)):E(_h8);}else{return new F(function(){return _hm(_);});}};if(!E(_h4)){return new F(function(){return _hk(_);});}else{return E(_h1)[1]!=(B(_D(_h6,0))-1|0)?E(_h8):B(_hk(_));}break;default:return E(_h8);}},_hu=function(_hv,_hw,_hx,_hy,_hz,_hA){var _hB=B(_2G(_hv,_9,_2V,new T(function(){if(!E(_hy)){var _hC=E(E(_hx)[1]);}else{var _hC=E(E(_hw)[1]);}return _hC;})))[1];if(!E(_hy)){var _hD=E(_hx);return !B(_4M(_hB,_hD[1]))?[0]:[1,[0,[0,_hw,_hD],_4S,[4,_hv],_hz,_hA]];}else{var _hE=E(_hw);return !B(_4M(_hB,_hE[1]))?[0]:[1,[0,[0,_hE,_hx],_4T,[4,_hv],_hz,_hA]];}},_hF=function(_hG){return [0,E(E(_hG))];},_hH=function(_hI,_hJ){return E(_hI);},_hK=[0,_ft,_hH],_hL=[0,2147483647],_hM=[0,-2147483648],_hN=[0,2147483562],_hO=[0,1],_hP=[0,_hO,_hN],_hQ=function(_hR){return E(_hP);},_hS=function(_hT){var _hU=E(_hT),_hV=B(_aE(_hU[1],_hU[2]));return [0,_hV[1],_hV[2]];},_hW=function(_hX,_hY){var _hZ=new T(function(){return E(B(_aE(_hX,_hY))[2]);});return [0,new T(function(){var _i0=E(_hX);if(_i0==2147483562){var _i1=[0,1,E(_hZ)[2]];}else{var _i1=[0,_i0+1|0,E(_hZ)[2]];}return _i1;}),new T(function(){var _i2=E(_hZ)[1],_i3=E(_hY);if(_i3==1){var _i4=[0,_i2,2147483398];}else{var _i4=[0,_i2,_i3-1|0];}var _i5=_i4;return _i5;})];},_i6=function(_i7){var _i8=E(_i7),_i9=B(_hW(_i8[1],_i8[2]));return [0,_i9[1],_i9[2]];},_ia=[0,_hS,_hQ,_i6],_ib=function(_ic){return E(E(_ic)[2]);},_id=function(_ie){return E(E(_ie)[1]);},_if=function(_ig,_ih,_ii,_ij,_ik){while(1){var _il=(function(_im,_in,_io,_ip,_iq){if(!B(_ch(_io,_ip))){var _ir=B(_cO(B(_cq(_ip,_io)),_aA)),_is=new T(function(){return B(A(_ib,[_im,_iq]));}),_it=new T(function(){return E(E(_is)[1]);}),_iu=new T(function(){return B(_cO(B(_cq(B(_cX(E(E(_is)[2])[1])),B(_cX(E(_it)[1])))),_aA));}),_iv=B((function(_iw,_ix,_iy){while(1){if(!B(_c8(_iw,B(_cZ(_ir,_cp))))){var _iz=B(A(new T(function(){return B(_id(_im));}),[_iy])),_iA=B(_cZ(_iw,_iu)),_iB=B(_cO(B(_cZ(_ix,_iu)),B(_cq(B(_cX(E(_iz[1])[1])),new T(function(){return B(_cX(E(_it)[1]));})))));_iy=_iz[2];_iw=_iA;_ix=_iB;continue;}else{return [0,_ix,_iy];}}})(_aA,_cg,_iq));return [0,new T(function(){return B(A(_c6,[_in,new T(function(){if(!B(_bY(_ir,_cg))){var _iC=B(_cO(_io,B(_cG(_iv[1],_ir))));}else{var _iC=E(_bX);}return _iC;})]));}),_iv[2]];}else{var _iD=_im,_iE=_in,_iF=_ip,_iG=_io,_iH=_iq;_ig=_iD;_ih=_iE;_ii=_iF;_ij=_iG;_ik=_iH;return null;}})(_ig,_ih,_ii,_ij,_ik);if(_il!=null){return _il;}}},_iI=[0,0],_iJ=function(_iK,_iL,_iM){var _iN=E(_iL);if(!_iN){return [0];}else{var _iO=new T(function(){var _iP=B(_if(_iK,_az,_iI,B(_cX(_iN)),_iM));return [0,_iP[1],_iP[2]];});return [1,[0,new T(function(){return E(E(_iO)[1]);}),_iM],new T(function(){return B(_iJ(_iK,_iN-1|0,new T(function(){return E(E(_iO)[2]);})));})];}},_iQ=function(_iR){var _iS=E(_iR);if(!_iS[0]){return [0,_Z,_Z];}else{var _iT=E(_iS[1]),_iU=new T(function(){var _iV=B(_iQ(_iS[2]));return [0,_iV[1],_iV[2]];});return [0,[1,_iT[1],new T(function(){return E(E(_iU)[1]);})],[1,_iT[2],new T(function(){return E(E(_iU)[2]);})]];}},_iW=new T(function(){return B(unCStr("[extractTree] impossible"));}),_iX=new T(function(){return B(err(_iW));}),_iY=function(_iZ,_j0){var _j1=function(_j2){var _j3=E(_j0);if(!_j3[0]){return E(_iX);}else{var _j4=_j3[1],_j5=_j3[3],_j6=E(_j3[2]);if(!_j6[0]){var _j7=new T(function(){var _j8=B(_iY(_iZ-1|0,_j5));return [0,_j8[1],_j8[2]];});return [0,new T(function(){return E(E(_j7)[1]);}),new T(function(){return [1,_j4-1|0,E(_j6),E(E(E(_j7)[2]))];})];}else{var _j9=_j6[1],_ja=function(_jb){if(_iZ>=_j9){var _jc=new T(function(){var _jd=B(_iY(_iZ-_j9|0,_j5));return [0,_jd[1],_jd[2]];});return [0,new T(function(){return E(E(_jc)[1]);}),new T(function(){return [1,_j4-1|0,E(_j6),E(E(E(_jc)[2]))];})];}else{var _je=new T(function(){var _jf=B(_iY(_iZ,_j6));return [0,_jf[1],_jf[2]];});return [0,new T(function(){return E(E(_je)[1]);}),new T(function(){return [1,_j4-1|0,E(E(E(_je)[2])),E(_j5)];})];}},_jg=E(_j5);if(!_jg[0]){return (_iZ+1|0)!=_j4?B(_ja(_)):[0,_jg[1],_j6];}else{return new F(function(){return _ja(_);});}}}};switch(E(_iZ)){case 0:var _jh=E(_j0);if(!_jh[0]){return new F(function(){return _j1(_);});}else{var _ji=E(_jh[2]);return _ji[0]==0?[0,_ji[1],_jh[3]]:B(_j1(_));}break;case 1:var _jj=E(_j0);if(!_jj[0]){return new F(function(){return _j1(_);});}else{if(E(_jj[1])==2){var _jk=E(_jj[2]);if(!_jk[0]){var _jl=E(_jj[3]);return _jl[0]==0?[0,_jl[1],_jk]:B(_j1(_));}else{return new F(function(){return _j1(_);});}}else{return new F(function(){return _j1(_);});}}break;default:return new F(function(){return _j1(_);});}},_jm=new T(function(){return B(unCStr("[shuffle] called with lists of different lengths"));}),_jn=new T(function(){return B(err(_jm));}),_jo=function(_jp,_jq){var _jr=function(_js){var _jt=E(_jq);if(!_jt[0]){return E(_jn);}else{var _ju=new T(function(){var _jv=B(_iY(E(_jt[1])[1],_jp));return [0,_jv[1],_jv[2]];});return [1,new T(function(){return E(E(_ju)[1]);}),new T(function(){return B(_jo(E(_ju)[2],_jt[2]));})];}},_jw=E(_jp);return _jw[0]==0?E(_jq)[0]==0?[1,_jw[1],_Z]:B(_jr(_)):B(_jr(_));},_jx=function(_jy){var _jz=E(_jy);if(!_jz[0]){return [0];}else{var _jA=_jz[1],_jB=E(_jz[2]);if(!_jB[0]){return [1,_jA,_Z];}else{var _jC=E(_jB[1]);return [1,new T(function(){var _jD=E(E(_jA));if(!_jD[0]){var _jE=E(_jC);if(!_jE[0]){var _jF=[1,2,E(_jD),E(_jE)];}else{var _jF=[1,_jE[1]+1|0,E(_jD),E(_jE)];}var _jG=_jF;}else{var _jH=_jD[1],_jI=E(_jC);if(!_jI[0]){var _jJ=[1,_jH+1|0,E(_jD),E(_jI)];}else{var _jJ=[1,_jH+_jI[1]|0,E(_jD),E(_jI)];}var _jG=_jJ;}return _jG;}),new T(function(){return B(_jx(_jB[2]));})];}}},_jK=new T(function(){return B(_jx(_Z));}),_jL=new T(function(){return B(_jM(_jK));}),_jM=function(_jN){while(1){var _jO=E(_jN);if(!_jO[0]){return E(_jL);}else{if(!E(_jO[2])[0]){return E(_jO[1]);}else{_jN=B(_jx(_jO));continue;}}}},_jP=function(_jQ,_jR,_jS){var _jT=B(A(_jQ,[_hK,function(_jU){var _jV=E(_jU),_jW=_jV[2];return [0,_jV[1],[1,_jR,new T(function(){return B(_jo(B(_jM(B(_1B(_hF,_jW)))),B(_iQ(B(_iJ(_ia,B(_D(_jW,0))-1|0,new T(function(){return E(E(_jS)[5]);})))))[1]));})],_jV[3],_jV[4],_jV[5]];},_jS]));return [0,_jT[1],_jT[2],_jT[3],_jT[4],new T(function(){return E(B(_d7(_az,_hM,_hL,_jT[5]))[2]);})];},_jX=function(_jY,_jZ,_k0){return new F(function(){return A(_jY,[function(_k1){var _k2=E(_k0),_k3=_k2[1],_k4=_k2[2];return [0,new T(function(){if(!E(_k4)){var _k5=[0,E(_k3)[1],_k1];}else{var _k5=[0,_k1,E(_k3)[2]];}return _k5;}),_k4,_k2[3],_k2[4],_k2[5]];},new T(function(){return B(A(_jZ,[new T(function(){var _k6=E(_k0),_k7=_k6[1];if(!E(_k6[2])){var _k8=E(E(_k7)[2]);}else{var _k8=E(E(_k7)[1]);}var _k9=_k8;return _k9;})]));})]);});},_ka=function(_kb,_kc,_kd){return new F(function(){return _jX(E(_kb)[1],_kc,_kd);});},_ke=function(_kf,_kg,_kh){var _ki=function(_kj){if(_kf<=_kj){var _kk=B(_jP(_ka,new T(function(){return B(_2G(_kg,_9,_2V,new T(function(){var _kl=E(_kh),_km=_kl[1];if(!E(_kl[2])){var _kn=E(E(E(_km)[2])[1]);}else{var _kn=E(E(E(_km)[1])[1]);}var _ko=_kn;return _ko;})));}),_kh)),_kp=_kk[1],_kq=_kk[2];return [0,new T(function(){if(!E(_kq)){var _kr=E(_kp),_ks=[0,_kr[1],new T(function(){var _kt=E(_kr[2]);return [0,new T(function(){return B(_gg(_kg,_kt[1]));}),_kt[2],_kt[3],_kt[4],_kt[5]];})];}else{var _ku=E(_kp),_ks=[0,new T(function(){var _kv=E(_ku[1]);return [0,new T(function(){return B(_gg(_kg,_kv[1]));}),_kv[2],_kv[3],_kv[4],_kv[5]];}),_ku[2]];}return _ks;}),_kq,_fw,_kk[4],_kk[5]];}else{var _kw=B(_jP(_ka,new T(function(){return B(_2G(_kg,_9,_2V,new T(function(){var _kx=E(_kh),_ky=_kx[1];if(!E(_kx[2])){var _kz=E(E(E(_ky)[2])[1]);}else{var _kz=E(E(E(_ky)[1])[1]);}var _kA=_kz;return _kA;})));}),new T(function(){var _kB=E(_kh),_kC=_kB[1],_kD=_kB[2];return [0,_kC,_kD,[3,new T(function(){if(E(B(_2G(_kg,_9,_2V,new T(function(){if(!E(_kD)){var _kE=E(E(E(_kC)[2])[1]);}else{var _kE=E(E(E(_kC)[1])[1]);}return _kE;})))[1])==1){var _kF=[0,_kf-2|0];}else{var _kF=[0,_kf-1|0];}var _kG=_kF;return _kG;})],_kB[4],_kB[5]];}))),_kH=_kw[1],_kI=_kw[2];return [0,new T(function(){if(!E(_kI)){var _kJ=E(_kH),_kK=[0,_kJ[1],new T(function(){var _kL=E(_kJ[2]);return [0,new T(function(){return B(_gg(_kg,_kL[1]));}),_kL[2],_kL[3],_kL[4],_kL[5]];})];}else{var _kM=E(_kH),_kK=[0,new T(function(){var _kN=E(_kM[1]);return [0,new T(function(){return B(_gg(_kg,_kN[1]));}),_kN[2],_kN[3],_kN[4],_kN[5]];}),_kM[2]];}return _kK;}),_kI,_kw[3],_kw[4],_kw[5]];}};return E(B(_2G(_kg,_9,_2V,new T(function(){var _kO=E(_kh),_kP=_kO[1];if(!E(_kO[2])){var _kQ=E(E(E(_kP)[2])[1]);}else{var _kQ=E(E(E(_kP)[1])[1]);}var _kR=_kQ;return _kR;})))[1])==1?B(_ki(2)):B(_ki(1));},_kS=function(_kT,_kU,_kV,_kW,_kX,_kY){var _kZ=E(_kW);switch(_kZ[0]){case 1:var _l0=E(_kU),_l1=B(_hu(_kT,_l0[1],_l0[2],_kV,_kX,_kY));return _l1[0]==0?[0,_l0,_kV,_fs,_kX,_kY]:E(_l1[1]);case 3:var _l2=B(_ke(E(_kZ[1])[1],_kT,[0,_kU,_kV,_kZ,_kX,_kY]));return [0,_l2[1],_l2[2],_l2[3],_l2[4],_l2[5]];case 4:var _l3=E(_kT);if(_l3[1]!=E(_kZ[1])[1]){var _l4=E(_kU),_l5=B(_hu(_l3,_l4[1],_l4[2],_kV,_kX,_kY));return _l5[0]==0?[0,_l4,_kV,_kZ,_kX,_kY]:E(_l5[1]);}else{return [0,_kU,_kV,_fs,_kX,_kY];}break;default:return [0,_kU,_kV,_kZ,_kX,_kY];}},_l6=function(_l7,_l8,_l9,_la,_lb,_lc){var _ld=E(_l7);if(!_ld[0]){return new F(function(){return _kS(_ld[1],_l8,_l9,_la,_lb,_lc);});}else{var _le=E(_ld[1]);return new F(function(){return _h0(_le[1],_le[2],_l8,_l9,_la,_lb,_lc);});}},_lf=function(_){return _4R;},_lg=function(_){var _lh=B(A(_15,["(function(){return document.body;})",_])),_li=_lh;return [0,_li];},_lj=[0],_lk=function(_ll,_lm,_){var _ln=B(A(_ll,[_])),_lo=_ln,_lp=E(_lm),_lq=jsSetTimeout(1000,_lp);return _4R;},_lr=function(_ls,_lt,_lu,_lv,_lw){var _lx=function(_ly){var _lz=new T(function(){return !E(_lu)?[0,_ls,new T(function(){var _lA=E(_lt);return [0,[1,_ly,_lA[1]],_lA[2],_lA[3],_lA[4],_lA[5]];})]:[0,new T(function(){var _lB=E(_ls);return [0,[1,_ly,_lB[1]],_lB[2],_lB[3],_lB[4],_lB[5]];}),_lt];},1);return [0,new T(function(){if(!E(_lu)){var _lC=E(_lz),_lD=[0,_lC[1],new T(function(){var _lE=E(_lC[2]);return [0,_lE[1],new T(function(){var _lF=E(_lE[2]);return _lF[0]==0?E(_dK):E(_lF[2]);}),_lE[3],_lE[4],_lE[5]];})];}else{var _lG=E(_lz),_lD=[0,new T(function(){var _lH=E(_lG[1]);return [0,_lH[1],new T(function(){var _lI=E(_lH[2]);return _lI[0]==0?E(_dK):E(_lI[2]);}),_lH[3],_lH[4],_lH[5]];}),_lG[2]];}return _lD;}),_lu,_fs,_lv,_lw];};if(!E(_lu)){var _lJ=E(_lt),_lK=E(_lJ[2]);return _lK[0]==0?[0,[0,_ls,_lJ],_4S,_fy,_lv,_lw]:B(_lx(_lK[1]));}else{var _lL=E(_ls),_lM=E(_lL[2]);return _lM[0]==0?[0,[0,_lL,_lt],_4T,_fx,_lv,_lw]:B(_lx(_lM[1]));}},_lN=function(_lO){var _lP=E(_lO),_lQ=E(_lP[1]),_lR=B(_lr(_lQ[1],_lQ[2],_lP[2],_lP[4],_lP[5]));return [0,_lR[1],_lR[2],_lR[3],_lR[4],_lR[5]];},_lS=new T(function(){return B(unCStr("foldr1"));}),_lT=new T(function(){return B(_6d(_lS));}),_lU=function(_lV,_lW){var _lX=E(_lW);if(!_lX[0]){return E(_lT);}else{var _lY=_lX[1],_lZ=E(_lX[2]);if(!_lZ[0]){return E(_lY);}else{return new F(function(){return A(_lV,[_lY,new T(function(){return B(_lU(_lV,_lZ));})]);});}}},_m0=function(_m1,_m2){return new F(function(){return _lU(_lk,[1,_lf,[1,function(_){var _m3=E(_m1)[1],_m4=rMV(_m3),_m5=_m4,_=wMV(_m3,new T(function(){var _m6=E(_m5);return [0,_m6[1],new T(function(){return !E(_m6[2])?true:false;}),_lj,_m6[4],_m6[5]];})),_m7=rMV(_m3),_m8=_m7,_m9=B(_lg(_)),_ma=_m9,_mb=B(A(_7e,[_m8,_ma,_])),_mc=_mb;return _4R;},[1,function(_){var _md=E(_m1)[1],_me=rMV(_md),_mf=_me,_=wMV(_md,new T(function(){return B(_lN(_mf));})),_mg=rMV(_md),_mh=_mg,_mi=B(_lg(_)),_mj=_mi,_mk=B(A(_7e,[_mh,_mj,_])),_ml=_mk;return _4R;},[1,_m2,_Z]]]]);});},_mm=function(_mn,_){var _mo=rMV(_mn),_mp=_mo,_mq=E(_mp);if(E(_mq[3])[0]==5){return new F(function(){return A(_m0,[[0,_mn],_lf,_]);});}else{var _mr=rMV(_mn),_ms=_mr,_=wMV(_mn,new T(function(){var _mt=B(_fl(_mq)),_mu=E(_ms),_mv=B(_l6(_mt[1],_mu[1],_mu[2],_mu[3],_mu[4],_mu[5]));return [0,_mv[1],_mv[2],_mv[3],_mv[4],_mt[2]];})),_mw=rMV(_mn),_mx=_mw,_my=B(_lg(_)),_mz=_my,_mA=B(A(_7e,[_mx,_mz,_])),_mB=_mA,_mC=jsSetTimeout(1000,function(_){return new F(function(){return _mm(_mn,_);});});return _4R;}},_mD=function(_mE){return new F(function(){return _52(E(_mE)[1]);});},_mF=function(_mG){return [0,[0,_mG],new T(function(){var _mH=E(_mG);if(_mH==13){var _mI=[0];}else{var _mJ=B(_mF(_mH+1|0)),_mI=[1,_mJ[1],_mJ[2]];}return _mI;})];},_mK=new T(function(){var _mL=B(_mF(1));return [1,_mL[1],_mL[2]];}),_mM=function(_mN){return _mN>1?[0,_mK,new T(function(){var _mO=B(_mM(_mN-1|0));return [1,_mO[1],_mO[2]];})]:[0,_mK,_Z];},_mP=new T(function(){var _mQ=B(_mM(2));return B(_6h([1,_mQ[1],_mQ[2]]));}),_mR=new T(function(){return [0,B(_D(_mP,0))];}),_mS=function(_mT,_mU,_mV){return new F(function(){return _iJ(_mT,E(_mU)[1],_mV);});},_mW=function(_mX,_mY,_mZ){return function(_n0){return new F(function(){return _jo(new T(function(){return B(_jM(B(_1B(_hF,_mY))));}),B(_iQ(B(_mS(_mX,new T(function(){return [0,E(_mZ)[1]-1|0];}),_n0))))[1]);});};},_n1=new T(function(){return B(_mW(_ia,_mP,_mR));}),_n2=function(_n3){return _n3>1?[0,_26,new T(function(){var _n4=B(_n2(_n3-1|0));return [1,_n4[1],_n4[2]];})]:[0,_26,_Z];},_n5=new T(function(){var _n6=B(_n2(3));return [1,_n6[1],_n6[2]];}),_n7=function(_n8){return _n8>1?[0,_n5,new T(function(){var _n9=B(_n7(_n8-1|0));return [1,_n9[1],_n9[2]];})]:[0,_n5,_Z];},_na=new T(function(){var _nb=B(_n7(5));return [1,_nb[1],_nb[2]];}),_nc=[0,63],_nd=[1,_nc,_Z],_ne=function(_nf){return E(_nd);},_ng=new T(function(){return B(unCStr("computers"));}),_nh=new T(function(){return B(unCStr("\u30b3\u30f3\u30d4\u30e5\u30fc\u30bf"));}),_ni=new T(function(){return B(unCStr("yours"));}),_nj=new T(function(){return B(unCStr("\u3042\u306a\u305f"));}),_nk=function(_nl,_nm){var _nn=E(_nl);if(!_nn){return [0];}else{var _no=E(_nm);return _no[0]==0?[0]:[1,_no[1],new T(function(){return B(_nk(_nn-1|0,_no[2]));})];}},_np=function(_nq,_nr,_ns){var _nt=new T(function(){return B(A(_n1,[_nr]));}),_nu=new T(function(){return B(A(_n1,[_nq]));});return [0,[0,[0,new T(function(){return B(_nk(3,_nu));}),new T(function(){return B(_2B(3,_nu));}),_nj,_ni,_mD],[0,new T(function(){return B(_nk(3,_nt));}),new T(function(){return B(_2B(3,_nt));}),_nh,_ng,_ne]],_4T,_lj,_na,_ns];},_nv=[0,0],_nw=0,_nx=new T(function(){return B(_bU(_nw));}),_ny=new T(function(){return die(_nx);}),_nz=function(_nA,_nB){var _nC=E(_nB);if(!_nC){return E(_bX);}else{var _nD=function(_nE){if(_nA<=0){if(_nA>=0){var _nF=quotRemI(_nA,_nC);return [0,[0,_nF[1]],[0,_nF[2]]];}else{if(_nC<=0){var _nG=quotRemI(_nA,_nC);return [0,[0,_nG[1]],[0,_nG[2]]];}else{var _nH=quotRemI(_nA+1|0,_nC);return [0,[0,_nH[1]-1|0],[0,(_nH[2]+_nC|0)-1|0]];}}}else{if(_nC>=0){if(_nA>=0){var _nI=quotRemI(_nA,_nC);return [0,[0,_nI[1]],[0,_nI[2]]];}else{if(_nC<=0){var _nJ=quotRemI(_nA,_nC);return [0,[0,_nJ[1]],[0,_nJ[2]]];}else{var _nK=quotRemI(_nA+1|0,_nC);return [0,[0,_nK[1]-1|0],[0,(_nK[2]+_nC|0)-1|0]];}}}else{var _nL=quotRemI(_nA-1|0,_nC);return [0,[0,_nL[1]-1|0],[0,(_nL[2]+_nC|0)+1|0]];}}};return E(_nC)==(-1)?E(_nA)==(-2147483648)?[0,_ny,_nv]:B(_nD(_)):B(_nD(_));}},_nM=function(_nN){var _nO=B(_nz((_nN>>>0&2147483647>>>0)>>>0&4.294967295e9,2147483562));return [0,E(_nO[2])[1]+1|0,B(_cz(E(_nO[1])[1],2147483398))+1|0];},_nP=function(_){var _nQ=B(A(_15,["(function(){ return Math.floor(Math.random() * Math.pow(2, 53)); })",_])),_nR=_nQ;return new T(function(){var _nS=jsTrunc(_nR),_nT=_nS,_nU=B(_nM(_nT));return [0,_nU[1],_nU[2]];});},_nV=[8,_],_nW=function(_nX){var _nY=String(_nX),_nZ=_nY;return new F(function(){return fromJSStr(_nZ);});},_o0=function(_o1,_o2){while(1){var _o3=E(_o1);if(!_o3[0]){return E(_o2)[0]==0?true:false;}else{var _o4=E(_o2);if(!_o4[0]){return false;}else{if(E(_o3[1])[1]!=E(_o4[1])[1]){return false;}else{_o1=_o3[2];_o2=_o4[2];continue;}}}}},_o5=new T(function(){return B(unCStr("LI"));}),_o6=new T(function(){return B(_15("(function(e){ return e.tagName })"));}),_o7=new T(function(){return B(unCStr("wheel"));}),_o8=new T(function(){return B(unCStr("mouseout"));}),_o9=new T(function(){return B(unCStr("mouseover"));}),_oa=new T(function(){return B(unCStr("mousemove"));}),_ob=new T(function(){return B(unCStr("blur"));}),_oc=new T(function(){return B(unCStr("focus"));}),_od=new T(function(){return B(unCStr("change"));}),_oe=new T(function(){return B(unCStr("unload"));}),_of=new T(function(){return B(unCStr("load"));}),_og=new T(function(){return B(unCStr("submit"));}),_oh=new T(function(){return B(unCStr("keydown"));}),_oi=new T(function(){return B(unCStr("keyup"));}),_oj=new T(function(){return B(unCStr("keypress"));}),_ok=new T(function(){return B(unCStr("mouseup"));}),_ol=new T(function(){return B(unCStr("mousedown"));}),_om=new T(function(){return B(unCStr("dblclick"));}),_on=new T(function(){return B(unCStr("click"));}),_oo=function(_op){switch(E(_op)[0]){case 0:return E(_of);case 1:return E(_oe);case 2:return E(_od);case 3:return E(_oc);case 4:return E(_ob);case 5:return E(_oa);case 6:return E(_o9);case 7:return E(_o8);case 8:return E(_on);case 9:return E(_om);case 10:return E(_ol);case 11:return E(_ok);case 12:return E(_oj);case 13:return E(_oi);case 14:return E(_oh);case 15:return E(_og);default:return E(_o7);}},_oq=new T(function(){return E(0);}),_or=new T(function(){return B(_15("(function(f) {  return (function() {      return (function(){        var args=Array.prototype.slice.call(arguments,0);        args.push(0);        return E(B(A(f, args)));    });  });})"));}),_os=function(_ot,_){return new F(function(){return A(_or,[E(_ot),_]);});},_ou=function(_ov,_){return new F(function(){return _os(_ov,_);});},_ow=new T(function(){return B(_15("(function(node, evtName, f){ node.addEventListener(evtName, (function(e){ f(e.target) }))})"));}),_ox=function(_oy,_oz){return function(_oA,_){var _oB=E(_oA),_oC=B(A(_ow,[E(_oB[1]),E(toJSStr(E(new T(function(){return B(_oo(_oy));})))),E(new T(function(){return B(_11(function(_){var _=0;return new F(function(){return _ou(function(_oD){return new F(function(){return _11(function(_){var _=0,_oE=B(A(_oz,[[0,_oD],_])),_oF=_oE;return E(_oq);});});},_);});}));})),_])),_oG=_oC;return _oB;};},_oH=function(_oI){return new F(function(){return _ox(_nV,function(_oJ,_){var _oK=E(_oJ)[1],_oL=B(A(_o6,[E(_oK),_])),_oM=_oL;if(!B(_o0(B(_nW(_oM)),_o5))){return _4R;}else{var _oN=B(_2s(_6D,_oK,_)),_oO=_oN;return new F(function(){return A(_oI,[_oO,_]);});}});});},_oP=new T(function(){return B(unCStr("TD"));}),_oQ=function(_oR){return new F(function(){return _ox(_nV,function(_oS,_){var _oT=E(_oS)[1],_oU=E(_oT),_oV=B(A(_o6,[_oU,_])),_oW=_oV;if(!B(_o0(B(_nW(_oW)),_oP))){return _4R;}else{var _oX=B(A(_6k,[_oU,_])),_oY=_oX,_oZ=B(_2s(_6D,_oY,_)),_p0=_oZ,_p1=B(_2s(_6D,_oT,_)),_p2=_p1;return new F(function(){return A(_oR,[_p0,_p2,_]);});}});});},_p3=new T(function(){return B(unCStr("#field"));}),_p4=new T(function(){return B(unCStr("#yours ol.hand"));}),_p5=function(_p6,_p7,_p8){var _p9=E(_p8);return new F(function(){return _h0(_p6,_p7,_p9[1],_p9[2],_p9[3],_p9[4],_p9[5]);});},_pa=function(_pb,_pc){var _pd=E(_pc);return new F(function(){return _kS(_pb,_pd[1],_pd[2],_pd[3],_pd[4],_pd[5]);});},_pe=function(_){var _pf=B(_nP(_)),_pg=_pf,_ph=B(_nP(_)),_pi=_ph,_pj=B(_nP(_)),_pk=_pj,_pl=new T(function(){var _pm=B(_np(_pg,_pi,_pk));return [0,_pm[1],_pm[2],_pm[3],_pm[4],_pm[5]];}),_pn=nMV(_pl),_po=_pn,_pp=function(_){return new F(function(){return _mm(_po,_);});},_pq=B(_lg(_)),_pr=_pq,_ps=[0,_po],_pt=B(_1b(_p4,new T(function(){return B(_oH(function(_pu,_){var _pv=rMV(_po),_pw=_pv;if(!E(E(_pw)[2])){return _4R;}else{var _px=rMV(_po),_py=_px,_=wMV(_po,new T(function(){return B(_pa(_pu,_py));})),_pz=rMV(_po),_pA=_pz,_pB=B(_lg(_)),_pC=_pB,_pD=B(A(_7e,[_pA,_pC,_])),_pE=_pD,_pF=rMV(_po),_pG=_pF;if(E(E(_pG)[3])[0]==5){var _pH=B(A(_m0,[_ps,_pp,_])),_pI=_pH;return _4R;}else{return _4R;}}}));}),_pr,_)),_pJ=_pt,_pK=B(_1b(_p3,new T(function(){return B(_oQ(function(_pL,_pM,_){var _pN=rMV(_po),_pO=_pN,_=wMV(_po,new T(function(){return B(_p5(_pL,_pM,_pO));})),_pP=rMV(_po),_pQ=_pP,_pR=B(_lg(_)),_pS=_pR,_pT=B(A(_7e,[_pQ,_pS,_])),_pU=_pT,_pV=rMV(_po),_pW=_pV;if(E(E(_pW)[3])[0]==5){var _pX=B(A(_m0,[_ps,_pp,_])),_pY=_pX;return _4R;}else{return _4R;}}));}),_pr,_)),_pZ=_pK,_q0=B(A(_7e,[_pl,_pr,_])),_q1=_q0,_q2=jsSetTimeout(1000,function(_){var _q3=rMV(_po),_q4=_q3,_=wMV(_po,new T(function(){return B(_lN(_q4));})),_q5=rMV(_po),_q6=_q5,_q7=B(_lg(_)),_q8=_q7,_q9=B(A(_7e,[_q6,_q8,_])),_qa=_q9;return _4R;});return _4R;},_qb=function(_){return new F(function(){return _pe(_);});};
var hasteMain = function() {B(A(_qb, [0]));};window.onload = hasteMain;