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
    | PExito of Expresion
    | PError of string
    | PEOF

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
                | PEOF -> PError "Se esperaba una expresión luego de la asignacion."
                | PError err -> PError (sprintf "Se esperaba una expresión luego de la asignación: %s" err)
                | PExito exprFinal ->
                    PExito <| EDeclaracion {
                        mut = esMut
                        id = {
                            signatura = Indefinida
                            valor = infoTokenId
                        }
                        valor = exprFinal
                    }

            with
            | Failure err -> PError err

        let rec sigExprFuncion funExpr paramExpr nivel =
            let exprFunAct = EFuncion {
                signatura = Indefinida
                fn = funExpr
                param = paramExpr
            }
            
            match lexer.SigToken () with
            | EOF -> PExito exprFunAct
            | ErrorLexer err -> PError err
            | Token (token, indentacion) ->
                match token with
                | TIdentificador infoId2 ->
                    let expr2 = EIdentificador {
                        signatura = Indefinida
                        valor = infoId2
                    }
                    sigExprFuncion exprFunAct expr2 nivel
                | TNumero infoNum ->
                    let expr2 = ENumero infoNum
                    sigExprFuncion exprFunAct expr2 nivel
                | TTexto infoTxt ->
                    let expr2 = ETexto infoTxt
                    sigExprFuncion exprFunAct expr2 nivel
                | TBool infoBool ->
                    let expr2 = EBool infoBool
                    sigExprFuncion exprFunAct expr2 nivel
                | _ -> PExito exprFunAct


        let sigExprIdentificador infoId nivel =
            let primeraExprId = EIdentificador {
                signatura = Indefinida
                valor = infoId
            }

            match lexer.SigToken () with
            | EOF -> PExito primeraExprId
            | ErrorLexer err -> PError err
            | Token (token, indentacion) ->
                match token with
                | TIdentificador infoId2 ->
                    let expr2 = EIdentificador {
                        signatura = Indefinida
                        valor = infoId2
                    }
                    sigExprFuncion primeraExprId expr2 nivel
                | TNumero infoNum ->
                    let expr2 = ENumero infoNum
                    sigExprFuncion primeraExprId expr2 nivel
                | TTexto infoTxt ->
                    let expr2 = ETexto infoTxt
                    sigExprFuncion primeraExprId expr2 nivel
                | TBool infoBool ->
                    let expr2 = EBool infoBool
                    sigExprFuncion primeraExprId expr2 nivel
                | _ -> PExito primeraExprId


        let resultado = lexer.SigToken ()

        let sigExprActual =
            match resultado with
            | EOF -> PEOF
            | ErrorLexer err -> PError err
            | Token (token, identacion) ->
                match token with
                    | PC_SEA infoToken ->
                        sigExprDeclaracion nivel
                    | TComentario _ -> sigExpresion nivel aceptarExprMismoNivel
                    | TNumero infoNumero ->
                        PExito (ENumero infoNumero)
                    | TTexto infoTexto ->
                        PExito (ETexto infoTexto)
                    | TBool infoBool ->
                        PExito (EBool infoBool)
                    | TIdentificador infoId ->
                        sigExprIdentificador infoId nivel
                    | TParenAb infoParen ->
                        let sigToken = sigExpresion nivel false
                        match sigToken with
                        | PError _ -> sigToken
                        | PEOF ->
                            PError <| sprintf "El parentesis abierto en %i no está cerrado." infoParen.inicio
                        | PExito sigToken' ->
                            let ultimoToken = lexer.SigToken ()
                            match ultimoToken with
                            | EOF ->
                                PError <| sprintf "El parentesis abierto en %i contiene una expresion, pero no está cerrado." infoParen.inicio
                            | ErrorLexer error ->
                                PError <| sprintf "El parentesis abierto en %i no está cerrado debido a un error léxico: %s" infoParen.inicio error
                            | Token (ultimoToken', indentacion2) ->
                                match ultimoToken' with
                                | TParenCer _ -> PExito sigToken'
                                | _ ->
                                    PError <| sprintf "Se esperaba un cierre de parentesis."
                    | TNuevaLinea _ -> sigExpresion nivel aceptarExprMismoNivel
                    | _ ->
                        PError <| sprintf "%s (%A)" "No implementado :c" token

        match sigExprActual with
        | PEOF -> sigExprActual
        | PError _ -> sigExprActual
        | PExito exprAct ->
            try
                let (sigNivelIndentacion, _) = obtSigIndentacion lexer "" (Some invalidOp) None
                if aceptarExprMismoNivel && sigNivelIndentacion = nivel then
                    let sigExprTop = sigExpresion nivel aceptarExprMismoNivel
                    match sigExprTop with
                    | PError err -> PError err
                    | PEOF -> sigExprActual
                    | PExito expr ->
                        PExito <| match expr with
                                    | EBloque exprs ->
                                        EBloque <| exprAct :: exprs
                                    | _ ->
                                        EBloque <| [exprAct; expr]
                else
                    sigExprActual
            with
            | :? System.InvalidOperationException as err -> PError err.Message
            | _ -> sigExprActual


    let expr' = sigExpresion 0 true
    match expr' with
    | PError err -> ErrorParser err
    | PExito expr -> ExitoParser expr
    | PEOF -> ErrorParser "EOF sin tratar en el parser."

