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
const http = require('http');;
function http$00$server(callback){return ((http.createServer)(callback));};
(function (server){return ((server.on)("clientError",function (err,socket){return ((socket.end)("HTTP/1.1 400 Bad Request\n\r\n"));}),(server.listen)(8000));})((http$00$server)(function (req,res){return ((res.end)());}));
})();