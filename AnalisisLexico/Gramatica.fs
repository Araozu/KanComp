module AnalisisLexico.Gramatica

open Parser


let internal operadores = [ '+'; '-'; '='; '*'; '!'; '\\'; '/'; '''; '|'; '@'; '#'; '·'; '$'; '~'; '%'; '¦'; '&'; '?'; '¿'; '¡'; '<'; '>'; '€'; '^'; '-'; '.'; ':'; ','; ';' ]
let internal digitos = [ '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9' ]
let internal mayusculas = [ 'A'; 'B'; 'C'; 'D'; 'E'; 'F'; 'G'; 'H'; 'I'; 'J'; 'K'; 'L'; 'M'; 'N'; 'O'; 'P'; 'Q'; 'R'; 'S'; 'T'; 'U'; 'V'; 'W'; 'X'; 'Y'; 'Z'; 'Ñ' ]
let internal minusculas = [ 'a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'; 'i'; 'j'; 'k'; 'l'; 'm'; 'n'; 'o'; 'p'; 'q'; 'r'; 's'; 't'; 'u'; 'v'; 'w'; 'x'; 'y'; 'z'; 'ñ' ]
let internal signosAgrupacion = [ '('; ')'; '{'; '}'; '['; ']' ]

let internal parseDigito = cualquier digitos
let internal parseMayuscula = cualquier mayusculas
let internal parseMinuscula = cualquier minusculas
let internal parseGuionBajo = parseCaracter '_'
let internal parseComillaSimple = parseCaracter '''

let private charListToStr (caracteres: char list) =
    let rec inner acc =
        function
        | [] -> acc
        | c :: cs -> inner (acc + c.ToString()) cs

    inner "" caracteres


let private parseOperador = cualquier operadores
let internal parseOperadores = parseVarios1 parseOperador |>> charListToStr


let internal parseNumero =
    let parseNumeros = parseVarios1 parseDigito |>> charListToStr
    let parsePunto = parseCaracter '.'

    let parseParteDecimal =
        parsePunto .>>. parseNumeros |>> fun (p, n) -> p.ToString() + n.ToString()

    parseNumeros <?> parseParteDecimal |>> fun (num, decimal) ->
        num.ToString() + match decimal with
                         | None -> ""
                         | Some s -> s



let internal parseTexto =
    let parseComilla = parseCaracter '"'
    let parseResto = (parseVarios <| parseCualquierMenos '"') |>> charListToStr

    between parseComilla parseResto parseComilla


let internal parseComentario =
    let parseBarra = parseCaracter '/'
    let parseInicio = parseBarra .>>. parseBarra |>> fun (x1, x2) -> x1.ToString() + x2.ToString()

    let parseResto = parseVarios <| parseCualquierMenos '\n' |>> charListToStr

    parseInicio >>. parseResto


let private parseRestoIdentificador =
    let pTest = parseDigito <|> parseMayuscula <|> parseMinuscula <|> parseGuionBajo <|> parseComillaSimple
    parseVarios pTest |>> charListToStr


let internal parseGenerico =
    let tuplaAStr ((c1, c2), s) = c1.ToString() + c2.ToString() + s
    parseComillaSimple .>>. parseMayuscula .>>. parseRestoIdentificador |>> tuplaAStr


let internal parseIdentificador =
    parseGuionBajo <|> parseMinuscula .>>. parseRestoIdentificador |>> fun (c, s) -> c.ToString() + s


let internal parseIdentificadorTipo =
    parseMayuscula .>>. parseRestoIdentificador |>> fun (c, s) -> c.ToString() + s


let internal parseNuevaLinea = parseCaracter '\n' |>> fun c -> c.ToString()



// Esta fun. asume que se encuentra al inicio de linea.
let internal parseIndentacion =
    let pEB = parseCaracter ' '
    let parseIdEspBlanco = parseVarios1 pEB |>> charListToStr

    let pTab = parseCaracter '\t' |>> fun c -> c.ToString()
    parseIdEspBlanco <|> pTab


let internal parseParenAb = parseCaracter '('
let internal parseParenCer = parseCaracter ')'

let internal parseLlaveAb = parseCaracter '{'
let internal parseLlaveCer = parseCaracter '}'

let internal parseCorcheteAb = parseCaracter '['
let internal parseCorcheteCer = parseCaracter ']'


let internal parseSignoAgrupacionAb =
    escoger [parseParenAb; parseLlaveAb; parseCorcheteAb] |>> fun x -> x.ToString()


let internal parseSignoAgrupacionCer =
    escoger [parseParenCer; parseLlaveCer; parseCorcheteCer] |>> fun x -> x.ToString()


let internal parserGeneral = parseVariasOpciones [
    mapTipo parseIndentacion Indentacion
    mapTipo parseNuevaLinea NuevaLinea
    mapTipo parseIdentificadorTipo IdentificadorTipo
    mapTipo parseIdentificador Identificador
    mapTipo parseGenerico Generico
    mapTipo parseComentario Comentario
    mapTipo parseNumero Numero
    mapTipo parseTexto Texto
    mapTipo parseOperadores Operadores
    mapTipo parseSignoAgrupacionAb AgrupacionAb
    mapTipo parseSignoAgrupacionCer AgrupacionCer
]



// TODO: Diferenciar entre errores normales y errores por EOF
let generarParser entrada =
    let mutable esInicioDeLinea = true
    let mutable posActual = 0
    let mutable identacionSobrante: Resultado<string> list = []


    let rec sigTokenLuegoDeIdentacion posActual =
        let sigToken = run parserGeneral entrada posActual
        match sigToken with
        | Error _ -> (Nada, -1)
        | Exito ex ->
            match ex.tipo with
            | Indentacion ->
                identacionSobrante <- sigToken :: identacionSobrante
                sigTokenLuegoDeIdentacion ex.posFinal
            | _ -> (ex.tipo, posActual)


    let rec extraerToken () =
        let resultado = run parserGeneral entrada posActual

        match resultado with
        | Error err -> Error err
        | Exito ex ->
            match ex.tipo with
            | Nada -> Error "Se encontró un token huerfano"

            | Indentacion when not esInicioDeLinea ->
                printfn "Encontrada indentacion en medio"
                // Se encontró espacios blancos o un Tab en medio de una linea.
                posActual <- ex.posFinal
                extraerToken ()

            | Indentacion ->

                let (tipo, sigPos) = sigTokenLuegoDeIdentacion ex.posFinal
                match tipo with
                | Nada _ ->
                    posActual <- ex.posFinal
                    resultado
                | NuevaLinea ->
                    identacionSobrante <- []
                    posActual <- sigPos
                    extraerToken ()
                | _ ->
                    posActual <- ex.posFinal
                    posActual <- sigPos
                    resultado

            | NuevaLinea ->
                posActual <- ex.posFinal
                esInicioDeLinea <- true
                resultado

            | _ ->
                esInicioDeLinea <- false
                posActual <- ex.posFinal
                resultado


    let extraerTokenHelper () =
        match identacionSobrante with
        | token::resto ->
            identacionSobrante <- resto
            token
        | [] -> extraerToken ()


    extraerTokenHelper



