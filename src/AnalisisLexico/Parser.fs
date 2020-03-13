module Parser

open System

// ===================================
//  Estructuras
// ===================================

type Exito<'A> = {
    res: 'A;
    posInicio: int;
    posFinal: int
    tipo: string option
}


type Resultado<'A> =
    | Exito of Exito<'A>
    | Error of string


type Parser<'A> = Parser of (string -> int -> Resultado<'A>)


// ===================================
//  Utilidades
// ===================================


let run (parser: Parser<'A>) entrada inicio =
    let (Parser p) = parser
    p entrada inicio


/// "bindP" takes a parser-producing function f, and a parser p
/// and passes the output of p into f, to create a new parser
let bindP f p =
    let innerFn entrada inicio =
        let result1 = run p entrada inicio
        match result1 with
        | Error err -> Error err
        | Exito ex ->
            let (resultado, posSiguiente) = (ex.res, ex.posFinal)
            let p2 = f resultado
            run p2 entrada posSiguiente

    Parser innerFn

let ( >>= ) p f = bindP f p


/// Lift a value to a Parser
let returnP x =
    let innerFn _ inicio =
        Exito {
            res = x
            posInicio = inicio
            posFinal = inicio
            tipo = None
        }

    Parser innerFn


/// apply a function to the value inside a parser
let mapP f =
    bindP (f >> returnP)

let ( <!> ) = mapP
let ( |>> ) x f = mapP f x


/// apply a wrapped function to a wrapped value
let applyP fP xP =
    fP >>= (fun f ->
        xP >>= (fun x ->
            returnP (f x)
        )
    )

let ( <*> ) = applyP


/// lift a two parameter function to Parser World
let lift2 f xP yP =
    returnP f <*> xP <*> yP


// ===================================
//  Parsers
// ===================================


let parseCaracter caracter =
    let inner entrada inicio = 
        if String.IsNullOrEmpty entrada then
            Error "Entrada terminada"
        else
            let c = entrada.[inicio]
            if c = caracter then
                Exito {
                    res = c
                    posInicio = inicio
                    posFinal = inicio + 1
                    tipo = None
                }
            else
                Error <| sprintf "Se esperaba '%c', pero se obtuvo '%c'." caracter c

    Parser inner


/// Aplica p1 y luego p2
let parseLuego p1 p2 =
    p1 >>= (fun p1Result ->
        p2 >>= (fun p2Result ->
            returnP (p1Result, p2Result)
        )
    )

let ( .>>. ) = parseLuego


/// Intenta aplicar p1 y si falla aplica p2
let parseOtro p1 p2 =
    let innerFn input entrada =
        let result1 = run p1 input entrada

        match result1 with
        | Exito _ -> result1
        | Error _ -> run p2 input entrada

    Parser innerFn

let ( <|> ) = parseOtro


/// Escoge desde una lista de parsers
let escoger listOfParsers =
    List.reduce ( <|> ) listOfParsers


/// Escoge desde una lista de caracteres
let cualquier listOfChars =
    listOfChars
    |> List.map parseCaracter
    |> escoger


/// Convierte una lista de Parsers a un Parser de listas
let rec sequence parserList =
    
    let cons head tail = head::tail

    let consP = lift2 cons

    // process the list of parsers recursively
    match parserList with
    | [] ->
        returnP []
    | head::tail ->
        consP head (sequence tail)



let rec private parseVariosHelper parser entrada inicio =

    let resultado = run parser entrada inicio
    
    match resultado with
    | Error _ -> ([], inicio)
    | Exito ex ->
        let (resultado, posSig) = (ex.res, ex.posFinal)
        let (valores, posFinal) = parseVariosHelper parser entrada posSig
        let valores = resultado::valores
        (valores, posFinal)


let parseVarios parser =
    let inner entrada inicio =
        let (datos, posFinal) = parseVariosHelper parser entrada inicio
        Exito {
            res = datos
            posInicio = inicio
            posFinal = posFinal
            tipo = None
        }

    Parser inner


let parseVarios1 parser =
    let inner entrada inicio =
        let (datos, posFinal) = parseVariosHelper parser entrada inicio

        match datos with
        | [] -> Error ""
        | _ -> Exito {
            res = datos
            posInicio = inicio
            posFinal = posFinal
            tipo = None
        }

    Parser inner


let parseSegundoOpcional p1 p2 =
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
                    tipo = None
                }
            | Error _ ->
                Exito {
                    res = (ex1.res, None)
                    posInicio = inicio
                    posFinal = ex1.posFinal
                    tipo = None
                }
    
    Parser inner


let parseCualquierMenos caracter =
    let inner entrada inicio =
        if String.IsNullOrEmpty entrada then
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
                    tipo = None
                }
        
        
    Parser inner


/// Parsea una ocurrencia opcional de p y lo devuelve en option
let pOpc p =
    let some = p |>> Some
    let none = returnP None
    some <|> none


/// Ignora el resultado del parser derecho
let (.>>) p1 p2 =
    p1 .>>. p2
    |> mapP (fun (a,_) -> a)

/// Ignora el resultado del parser izq
let (>>.) p1 p2 =
    p1 .>>. p2
    |> mapP (fun (_,b) -> b)

/// Ignora el resultado de los parsers de los costados
let between p1 p2 p3 =
    p1 >>. p2 .>> p3

