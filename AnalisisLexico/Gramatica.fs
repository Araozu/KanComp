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
let internal parseIdentacion =
    let tuplaAStr (((c1,c2),c3),c4) =
        c1.ToString() + c2.ToString() + c3.ToString() + c4.ToString()

    let pEsp = parseCaracter ' '
    let parseIdEspBlanco = pEsp .>>. pEsp .>>. pEsp .>>. pEsp |>> tuplaAStr

    let pTab = parseCaracter '\t' |>> fun c -> c.ToString()
    parseIdEspBlanco <|> pTab


let internal parseVariasOpciones parsers =
    let inner entrada pos =

        let rec inner2 parsers =
            match parsers with
            | p::ps ->
                let resultado = run p entrada pos

                match resultado with
                | Exito ex ->
                    Exito {
                        res = ex.res
                        posInicio = ex.posInicio
                        posFinal = ex.posFinal
                        tipo = ex.tipo
                    }
                | _ -> inner2 ps
            | [] -> Error "Ningun parser se adapta a la entrada."


        inner2 parsers

    Parser inner


let internal parseEspBlanco =
    let pEB = parseCaracter ' '
    parseVarios1 pEB |>> charListToStr



let internal parserGeneral = parseVariasOpciones [
    mapTipo parseIdentacion Identacion
    mapTipo parseEspBlanco EspBlanco
    mapTipo parseNuevaLinea NuevaLinea
    mapTipo parseIdentificadorTipo IdentificadorTipo
    mapTipo parseIdentificador Identificador
    mapTipo parseGenerico Generico
    mapTipo parseComentario Comentario
    mapTipo parseNumero Numero
    mapTipo parseTexto Texto
    mapTipo parseOperadores Operadores
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
            | Identacion ->
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

            | EspBlanco when esInicioDeLinea ->

                let sigToken = run parserGeneral entrada ex.posFinal
                match sigToken with
                | Error _ ->
                    posActual <- ex.posFinal
                    extraerToken ()
                | Exito ex2 ->
                    if ex2.tipo = NuevaLinea then
                        posActual <- ex.posFinal
                        extraerToken ()
                    else
                        Error "Error de identación."

            | EspBlanco ->
                posActual <- ex.posFinal
                esInicioDeLinea <- false
                extraerToken ()

            | NuevaLinea ->
                posActual <- ex.posFinal
                esInicioDeLinea <- true
                extraerToken ()

            | Identacion when not esInicioDeLinea ->
                // Se encontró 4 espacios blancos o un Tab en medio de una linea.
                posActual <- ex.posFinal
                extraerToken ()

            | Identacion ->
                
                let (tipo, sigPos) = sigTokenLuegoDeIdentacion ex.posFinal
                match tipo with
                | Nada _ ->
                    posActual <- ex.posFinal
                    resultado
                | NuevaLinea | EspBlanco ->
                    identacionSobrante <- []
                    posActual <- sigPos
                    extraerToken ()
                | _ ->
                    posActual <- ex.posFinal
                    posActual <- sigPos
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



