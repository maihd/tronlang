// Code generated from Tron Language
"use strict";
(function(){function $16$tron$00$symbol(value){return (this.value = value);};
function $16$tron$00$keyword(value){return (this.value = value);};
function number$04$(x){return (("number" == typeof(x)));};
function string$04$(x){return (("string" == typeof(x)));};
function symbol$04$(x){return ((x instanceof $16$tron$00$symbol));};
function keyword$04$(x){return ((x instanceof $16$tron$00$keyword));};
function string$00$$10$symbol(x){return (new $16$tron$00$symbol(x));};
function string$00$$10$keyword(x){return (new $16$tron$00$keyword(x));};
function symbol$00$$10$string(x){return (new $16$tron$00$symbol(x));};
function keyword$00$$10$string(x){return (new $16$tron$00$keyword(x));};
function print(x){return ((console.log)(x));};
function object$04$(x){return ((typeof(x) == "object"));};
function $16$point(x,y){return (this.x = x,this.y = y);};
$16$point.prototype = {to$00$string:function (){return (("point(" + this.x + ", " + this.y + ")"));}};
(function (p){return ((print)(p),(((object$04$)(p))?((print)("This is an object")):(null)),(console.log)((p.to$00$string)()));})(new $16$point(1,2));
})();