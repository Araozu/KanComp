module AnalisisSintactico.Parser

open AnalisisLexico.Lexer
open AnalisisLexico.Gramatica


// ===================================
//  Expresion
// ===================================

type Signatura =
    | Indefinida
    | Simple of string
    | Array of Signatura
    | Tupla of Signatura list
    | Funcion of Signatura * Signatura

type EIdentificador = {
    signatura: Signatura
    valor: InfoToken<string>
}

and EOperador = {
    signatura: Signatura
    valor: InfoToken<string>
}

and EOperadorApl = {
    op: EOperador
    izq: Expresion
    der: Expresion
}

and EFuncion = {
    signatura: Signatura
    fn: Expresion
    param: Expresion
}

and EDeclaracion = {
    mut: bool
    id: EIdentificador
    valor: Expresion
}


and Expresion =
    | EIdentificador of EIdentificador
    | EUnidad
    | ENumero of InfoToken<float>
    | ETexto of InfoToken<string>
    | EBool of InfoToken<bool>
    | EOperador of InfoToken<string>
    | EOperadorApl of EOperadorApl
    | EFuncion of EFuncion
    | EDeclaracion of EDeclaracion
    | EModulo of Expresion list


type ExprRes =
    | ER_Exito of Expresion
    | ER_Error of string
    | ER_EOF

type ResParser =
    | ExitoParser of Expresion
    | ErrorParser of string


let parseTokens (lexer: Lexer) =

    let rec sigExpresion nivel =

        let resultado = lexer.SigToken ()
        match resultado with
        | EOF -> ER_EOF
        | ErrorLexer err -> ER_Error err
        | Token token ->
            match token with
            (*
            | Identificador when token.res = "sea" ->
                sigExprDeclaracion nivel
            *
            | Identificador ->
                sigExprIdentificador nivel token
            *)
            | TNumero infoNumero ->
                ER_Exito (ENumero infoNumero)
            | _ ->
                ER_Error "No implementado :c"


    try
        let mutable expresiones = []
        while lexer.HayTokens () do
            let expr' = sigExpresion 0
            match expr' with
            | ER_Error err -> failwith err
            | ER_Exito expr ->
                expresiones <- expresiones @ [expr]
            | ER_EOF -> ()

        ExitoParser <| EModulo expresiones
    with
    | Failure err -> ErrorParser err
