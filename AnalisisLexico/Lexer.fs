module AnalisisLexico.Lexer

open System

// ===================================
//  Estructuras
// ===================================

type Token =
    | Indentacion
    | NuevaLinea
    | IdentificadorTipo
    | Identificador
    | Generico
    | Comentario
    | Numero
    | Texto
    | Operadores
    | AgrupacionAb
    | AgrupacionCer
    | Nada


type Exito<'A> = {
    res: 'A
    posInicio: int
    posFinal: int
    tipo: Token
}


type InfoToken<'A> = {
    valor:       'A
    inicio:      int
    final:       int
}


type Token2 =
    | TNuevaLinea of InfoToken<unit>
    | TIdentificador of InfoToken<string>
    | TGenerico of InfoToken<string>
    | TComentario of InfoToken<string>
    | TNumero of InfoToken<float>
    | TTexto of InfoToken<string>
    | TBool of InfoToken<bool>
    | TOperador of InfoToken<string>
    | TParenAb of InfoToken<string>  // Parentesis abierto
    | TParenCer of InfoToken<string> // Parentesis cerrado
    | TAgrupAb of InfoToken<string>
    | TAgrupCer of InfoToken<string>
    | PC_SEA of InfoToken<string>
    | PC_MUT of InfoToken<string>


type Resultado<'A> =
    | Exito of Exito<'A>
    | Error of string


type internal Parser<'A> = Parser of (string -> int -> Resultado<'A>)


// ===================================
//  Utilidades
// ===================================


let internal run (parser: Parser<'A>) entrada inicio =
    let (Parser p) = parser
    p entrada inicio


/// "bindP" takes a parser-producing function f, and a parser p
/// and passes the output of p into f, to create a new parser
/// TODO: Remover uso de bindP porque pierde el state de los parsers.
let internal bindP f p =
    let innerFn entrada inicio =
        let result1 = run p entrada inicio
        match result1 with
        | Error err -> Error err
        | Exito ex ->
            let (resultado, posSiguiente) = (ex.res, ex.posFinal)
            let p2 = f resultado
            run p2 entrada posSiguiente

    Parser innerFn

let internal ( >>= ) p f = bindP f p


/// Lift a value to a Parser
let internal returnP x =
    let innerFn _ inicio =
        Exito {
            res = x
            posInicio = inicio
            posFinal = inicio
            tipo = Nada
        }

    Parser innerFn


/// Aplica una función al resultado de un Parser
let internal mapP f p =
    let inner entrada inicio =
        let res = run p entrada inicio
        match res with
        | Error err -> Error err
        | Exito ex ->
            Exito {
                res = f ex.res
                posInicio = ex.posInicio
                posFinal = ex.posFinal
                tipo = ex.tipo
            }

    Parser inner

let internal ( <!> ) = mapP
let internal ( |>> ) x f = mapP f x


/// apply a wrapped function to a wrapped value
let internal applyP fP xP =
    fP >>= (fun f ->
        xP >>= (fun x ->
            returnP (f x)
        )
    )

let internal ( <*> ) = applyP


/// lift a two parameter function to Parser World
let internal lift2 f xP yP =
    returnP f <*> xP <*> yP


// ===================================
//  Parsers
// ===================================


let internal parseCaracter caracter =
    let inner entrada inicio = 
        if String.IsNullOrEmpty entrada || inicio >= entrada.Length then
            Error "Entrada terminada"
        else
            let c = entrada.[inicio]
            if c = caracter then
                Exito {
                    res = c
                    posInicio = inicio
                    posFinal = inicio + 1
                    tipo = Nada
                }
            else
                Error <| sprintf "Se esperaba '%c', pero se obtuvo '%c'." caracter c

    Parser inner


/// Aplica p1 y luego p2
let internal parseLuego p1 p2 =
    let inner entrada inicio =
        let res1 = run p1 entrada inicio

        match res1 with
        | Error err -> Error err
        | Exito ex1 ->
            let res2 = run p2 entrada ex1.posFinal

            match res2 with
            | Error err -> Error err
            | Exito ex2 ->
                Exito {
                    res = (ex1.res, ex2.res)
                    posInicio = inicio
                    posFinal = ex2.posFinal
                    tipo = Nada
                }

    Parser inner


let internal ( .>>. ) = parseLuego


/// Intenta aplicar p1 y si falla aplica p2
let internal parseOtro p1 p2 =
    let innerFn entrada inicio =
        let result1 = run p1 entrada inicio

        match result1 with
        | Exito _ -> result1
        | Error _ -> run p2 entrada inicio

    Parser innerFn

let internal ( <|> ) = parseOtro


/// Escoge desde una lista de parsers
let internal escoger listOfParsers =
    List.reduce ( <|> ) listOfParsers


/// Escoge desde una lista de caracteres
let internal cualquier listOfChars =
    listOfChars
    |> List.map parseCaracter
    |> escoger


/// Convierte una lista de Parsers a un Parser de listas
let rec internal sequence parserList =
    
    let cons head tail = head::tail

    let consP = lift2 cons

    // process the list of parsers recursively
    match parserList with
    | [] ->
        returnP []
    | head::tail ->
        consP head (sequence tail)



let rec internal parseVariosHelper parser entrada inicio =

    let resultado = run parser entrada inicio
    
    match resultado with
    | Error _ -> ([], inicio)
    | Exito ex ->
        let (resultado, posSig) = (ex.res, ex.posFinal)
        let (valores, posFinal) = parseVariosHelper parser entrada posSig

        (resultado::valores, posFinal)


let internal parseVarios parser =
    let inner entrada inicio =
        let (datos, posFinal) = parseVariosHelper parser entrada inicio
        Exito {
            res = datos
            posInicio = inicio
            posFinal = posFinal
            tipo = Nada
        }

    Parser inner


let internal parseVarios1 parser =
    let inner entrada inicio =
        let (datos, posFinal) = parseVariosHelper parser entrada inicio

        match datos with
        | [] -> Error ""
        | _ -> Exito {
            res = datos
            posInicio = inicio
            posFinal = posFinal
            tipo = Nada
        }

    Parser inner


let internal parseSegundoOpcional p1 p2 =
    let inner entrada inicio =
        let res1 = run p1 entrada inicio
        
        match res1 with
        | Error err -> Error err
        | Exito ex1 ->
            let res2 = run p2 entrada ex1.posFinal
            
            match res2 with
            | Exito ex2 ->
                Exito {
                    res = (ex1.res, Some ex2.res)
                    posInicio = inicio
                    posFinal = ex2.posFinal
                    tipo = Nada
                }
            | Error _ ->
                Exito {
                    res = (ex1.res, None)
                    posInicio = inicio
                    posFinal = ex1.posFinal
                    tipo = Nada
                }
    
    Parser inner

let internal (<?>) = parseSegundoOpcional


let internal parseCualquierMenos caracter =
    let inner entrada inicio =
        if String.IsNullOrEmpty entrada || inicio >= entrada.Length then
            Error "Entrada terminada"
        else
            let c = entrada.[inicio]
            if caracter = c then
                Error "Se encontró el caracter a no parsear."
            else
                Exito {
                    res = c
                    posInicio = inicio
                    posFinal = inicio + 1
                    tipo = Nada
                }
        
        
    Parser inner


/// Parsea una ocurrencia opcional de p y lo devuelve en option
let internal pOpc p =
    let some = p |>> Some
    let none = returnP None
    some <|> none


/// Ignora el resultado del parser derecho
let internal (.>>) p1 p2 = p1 .>>. p2 |>> fun (a,_) -> a

/// Ignora el resultado del parser izq
let internal (>>.) p1 p2 = p1 .>>. p2 |>> fun (_,b) -> b

/// Ignora el resultado de los parsers de los costados
let internal between p1 p2 p3 =
    p1 >>. p2 .>> p3


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


let internal mapTipo parser nuevoTipo =
    let inner entrada inicio =
        let res = run parser entrada inicio
        
        match res with
        | Error err -> Error err
        | Exito ex -> Exito {
            res = ex.res
            posInicio = ex.posInicio
            posFinal = ex.posFinal
            tipo = nuevoTipo
        }

    Parser inner
