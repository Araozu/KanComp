module AnalisisSintactico.Parser

open AnalisisLexico.Lexer
open AnalisisLexico.Gramatica
open AnalisisSintactico


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

        let sigExprDeclaracion nivel =
            try
                let mutable esMut = false
                let token2 = lexer.SigToken ()
                let mutable preTokenId = token2
                
                try
                    let infoTokenMut = Expect.PC_MUT token2 None ""
                    esMut <- true
                    preTokenId <- lexer.SigToken()
                    ()
                with
                | _ -> ()

                let infoTokenId = Expect.TIdentificador preTokenId None "Se esperaba un identificador"
                let infoTokenOpAsign = Expect.TOperador (lexer.SigToken ()) (Some "=") "Se esperaba el operador de asignación '=' luego del indentificador."

                match sigExpresion nivel with
                | ER_EOF -> ER_Error "Se esperaba una expresión luego de la asignacion."
                | ER_Error err -> ER_Error err
                | ER_Exito exprFinal ->
                    ER_Exito <| EDeclaracion {
                        mut = esMut
                        id = {
                            signatura = Indefinida
                            valor = infoTokenId
                        }
                        valor = exprFinal
                    }
                
            with
            | Failure err -> ER_Error err

        let resultado = lexer.SigToken ()

        let sigExprActual = 
            match resultado with
            | EOF -> ER_EOF
            | ErrorLexer err -> ER_Error err
            | Token (token, identacion) ->
                match token with
                    | PC_SEA infoToken ->
                        sigExprDeclaracion nivel
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
        | (ER_Exito _, EOF) -> sigExprActual
        | (ER_Exito _, ErrorLexer err) -> ER_Error err
        | (ER_Exito exprAct, Token (_, indentacion2)) ->
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



    let expr' = sigExpresion 0
    match expr' with
    | ER_Error err -> ErrorParser err
    | ER_Exito expr -> ExitoParser expr
    | ER_EOF -> ErrorParser "EOF sin tratar en el parser."

