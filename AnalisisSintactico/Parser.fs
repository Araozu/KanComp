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
    | EUnidad of InfoToken<unit>
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


let obtSigIndentacion (lexer: Lexer) msgError fnErrorLexer fnEOF =
    let mutable hayNuevaLinea = false
    try
        while true do
            ignore <| Expect.TNuevaLinea (lexer.LookAhead ()) None ""
            hayNuevaLinea <- true
            ignore (lexer.SigToken ())
        (-1, true)
    with
    | _ ->
        let (_, nuevaIndentacion) = Expect.Any (lexer.LookAhead ()) msgError fnErrorLexer fnEOF
        (nuevaIndentacion, hayNuevaLinea)


let parseTokens (lexer: Lexer) =

    let rec sigExpresion nivel aceptarExprMismoNivel =

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

                let (nuevoNivel, hayNuevaLinea) = obtSigIndentacion lexer "Se esperaba una expresion luego del signo '='." None None

                if hayNuevaLinea && nuevoNivel <= nivel then
                    failwith "La expresión actual está incompleta. Se esperaba una expresión indentada."


                match sigExpresion nuevoNivel hayNuevaLinea with
                | ER_EOF -> ER_Error "Se esperaba una expresión luego de la asignacion."
                | ER_Error err -> ER_Error (sprintf "Se esperaba una expresión luego de la asignación: %s" err)
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
                    | TComentario _ -> sigExpresion nivel aceptarExprMismoNivel
                    | TNumero infoNumero ->
                        ER_Exito (ENumero infoNumero)
                    | TTexto infoTexto ->
                        ER_Exito (ETexto infoTexto)
                    | TBool infoBool ->
                        ER_Exito (EBool infoBool)
                    | TParenAb infoParen ->
                        let sigToken = sigExpresion nivel false
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
                    | TNuevaLinea _ -> sigExpresion nivel aceptarExprMismoNivel
                    | _ ->
                        ER_Error <| sprintf "%s (%A)" "No implementado :c" token

        match sigExprActual with
        | ER_EOF -> sigExprActual
        | ER_Error _ -> sigExprActual
        | ER_Exito exprAct ->
            try
                let (sigNivelIndentacion, _) = obtSigIndentacion lexer "" (Some invalidOp) None
                if aceptarExprMismoNivel && sigNivelIndentacion = nivel then
                    let sigExprTop = sigExpresion nivel aceptarExprMismoNivel
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
            with
            | :? System.InvalidOperationException as err -> ER_Error err.Message
            | _ -> sigExprActual


    let expr' = sigExpresion 0 true
    match expr' with
    | ER_Error err -> ErrorParser err
    | ER_Exito expr -> ExitoParser expr
    | ER_EOF -> ErrorParser "EOF sin tratar en el parser."

