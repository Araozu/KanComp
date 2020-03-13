module Gramatica

open Parser


let operadores = [
    '+'
    '-'
    '='
    '*'
    '!'
    '\\'
    '/'
    '\''
    '|'
    '@'
    '#'
    '·'
    '$'
    '~'
    '%'
    '¦'
    '&'
    '?'
    '¿'
    '¡'
    '<'
    '>'
    '€'
    '^'
    '-'
    '.'
    ':'
    ','
    ';'
]

let private charListToStr (caracteres: char list) =
    let rec inner acc = function
        | [] -> acc
        | c::cs -> inner (acc + c.ToString()) cs

    inner "" caracteres


let private parseOperador = cualquier operadores
let parseOperadores = parseVarios1 parseOperador |>> charListToStr



