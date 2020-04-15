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
    | EBloque of Expresion list


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

        let sigExprActual = 
            match resultado with
            | EOF -> ER_EOF
            | ErrorLexer err -> ER_Error err
            | Token (token, identacion) ->
                match token with
                    (*
                    | Identificador when token.res = "sea" ->
                        sigExprDeclaracion nivel
                    *
                    | Identificador ->
                        sigExprIdentificador nivel token
                    *)
                    | TComentario _ -> sigExpresion nivel
                    | TNumero infoNumero ->
                        ER_Exito (ENumero infoNumero)
                    | TTexto infoTexto ->
                        ER_Exito (ETexto infoTexto)
                    | TBool infoBool ->
                        ER_Exito (EBool infoBool)
                    | TParenAb infoParen ->
                        let sigToken = sigExpresion nivel
                        match sigToken with
                        | ER_Error _ -> sigToken
                        | ER_EOF ->
                            ER_Error <| sprintf "El parentesis abierto en %i no está cerrado." infoParen.inicio
                        | ER_Exito sigToken' ->
                            let ultimoToken = lexer.SigToken ()
                            match ultimoToken with
                            | EOF ->
                                ER_Error <| sprintf "El parentesis abierto en %i contiene una expresion, pero no está cerrado." infoParen.inicio
                            | ErrorLexer error ->
                                ER_Error <| sprintf "El parentesis abierto en %i no está cerrado debido a un error léxico: %s" infoParen.inicio error
                            | Token (ultimoToken', indentacion2) ->
                                match ultimoToken' with
                                | TParenCer _ -> ER_Exito sigToken'
                                | _ ->
                                    ER_Error <| sprintf "Se esperaba un cierre de parentesis."
                    | _ ->
                        ER_Error "No implementado :c"

        match (sigExprActual, lexer.LookAhead ()) with
        | (ER_EOF, _) -> sigExprActual
        | (ER_Error _, _) -> sigExprActual
        | (ER_Exito _, EOF) -> ER_EOF
        | (ER_Exito _, ErrorLexer err) -> ER_Error err
        | (ER_Exito exprAct, Token (token, indentacion2)) ->
            if indentacion2 = nivel then
                let sigExprTop = sigExpresion nivel
                match sigExprTop with
                | ER_Error err -> ER_Error err
                | ER_EOF -> sigExprActual
                | ER_Exito expr ->
                    ER_Exito <| match expr with
                                | EBloque exprs ->
                                    EBloque <| exprAct :: exprs
                                | _ ->
                                    EBloque <| [exprAct; expr]
            else
                sigExprActual
            


    try
        let mutable expresiones = []
        while lexer.HayTokens () do
            let expr' = sigExpresion 0
            match expr' with
            | ER_Error err -> failwith err
            | ER_Exito expr ->
                expresiones <- expresiones @ [expr]
            | ER_EOF -> ()

        ExitoParser <| EBloque expresiones
    with
    | Failure err -> ErrorParser err
