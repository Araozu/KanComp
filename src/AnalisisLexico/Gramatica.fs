module Gramatica

open Parser


let parseOperadores = [
    '+'
    '-'
    '='
    '*'
    '!'
    '\\'
    '/'
    '\''
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

let private parseOperador = cualquier parseOperadores


