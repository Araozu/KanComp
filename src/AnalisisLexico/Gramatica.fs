module Gramatica

open Parser


let operadores = [ '+'; '-'; '='; '*'; '!'; '\\'; '/'; '''; '|'; '@'; '#'; '·'; '$'; '~'; '%'; '¦'; '&'; '?'; '¿'; '¡'; '<'; '>'; '€'; '^'; '-'; '.'; ':'; ','; ';' ]
let digitos = [ '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9' ]
let mayusculas = [ 'A'; 'B'; 'C'; 'D'; 'E'; 'F'; 'G'; 'H'; 'I'; 'J'; 'K'; 'L'; 'M'; 'N'; 'O'; 'P'; 'Q'; 'R'; 'S'; 'T'; 'U'; 'V'; 'W'; 'X'; 'Y'; 'Z'; 'Ñ' ]
let minusculas = [ 'a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'; 'i'; 'j'; 'k'; 'l'; 'm'; 'n'; 'o'; 'p'; 'q'; 'r'; 's'; 't'; 'u'; 'v'; 'w'; 'x'; 'y'; 'z'; 'ñ' ]
let signosAgrupacion = [ '('; ')'; '{'; '}'; '['; ']' ]

let parseDigito = cualquier digitos
let parseMayuscula = cualquier mayusculas
let parseMinuscula = cualquier minusculas
let parseGuionBajo = parseCaracter '_'
let parseComillaSimple = parseCaracter '''

let private charListToStr (caracteres: char list) =
    let rec inner acc =
        function
        | [] -> acc
        | c :: cs -> inner (acc + c.ToString()) cs

    inner "" caracteres


let private parseOperador = cualquier operadores
let parseOperadores = parseVarios1 parseOperador |>> charListToStr


let parseNumero =
    let parseNumeros = parseVarios1 parseDigito |>> charListToStr
    let parsePunto = parseCaracter '.'

    let parseParteDecimal =
        parsePunto .>>. parseNumeros |>> fun (p, n) -> p.ToString() + n.ToString()

    parseNumeros <?> parseParteDecimal |>> fun (num, decimal) ->
        num.ToString() + match decimal with
                         | None -> ""
                         | Some s -> s
        |> double


let parseTexto =
    let parseComilla = parseCaracter '"'
    let parseResto = (parseVarios <| parseCualquierMenos '"') |>> charListToStr

    between parseComilla parseResto parseComilla


let parseComentario =
    let parseBarra = parseCaracter '/'
    let parseInicio = parseBarra .>>. parseBarra |>> fun (x1, x2) -> x1.ToString() + x2.ToString()

    let parseResto = parseVarios <| parseCualquierMenos '\n' |>> charListToStr

    parseInicio >>. parseResto


let parseRestoIdentificador =
    let pTest = parseDigito <|> parseMayuscula <|> parseMinuscula <|> parseGuionBajo <|> parseComillaSimple
    parseVarios pTest |>> charListToStr


let parseGenerico =
    let tuplaAStr ((c1, c2), s) = c1.ToString() + c2.ToString() + s
    parseComillaSimple .>>. parseMayuscula .>>. parseRestoIdentificador |>> tuplaAStr


let parseIdentificador =
    parseGuionBajo <|> parseMinuscula .>>. parseRestoIdentificador |>> fun (c, s) -> c.ToString() + s


let parseIdentificadorTipo =
    parseMayuscula .>>. parseRestoIdentificador |>> fun (c, s) -> c.ToString() + s
