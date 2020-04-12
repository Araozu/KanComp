module AnalisisSintactico.Expresion

open System
open AnalisisLexico.Parser

// ===================================
//  Expresion
// ===================================

type Signatura =
    | Indefinida
    | Simple of string
    | Array of Signatura
    | Tupla of Signatura list
    | Funcion of Signatura * Signatura


type OperadorExpr = {
    signatura: Signatura
    valor: Exito<string>
}

type IdentificadorExpr = {
    signatura: Signatura
    valor: Exito<string>
}


type Expresion =
    | IdentificadorExpr of IdentificadorExpr
    | Unidad of Exito<string>
    | NumeroExpr of Exito<float>
    | TextoExpr of Exito<string>
    | BoolExpr of Exito<bool>
    | OperadorExpr of OperadorExpr
    | OperadorAplExpr of OperadorApl
    | FuncionExpr of FuncionExpr
    | DeclaracionExpr of Declaracion
    | Modulo of Expresion list

and FuncionExpr = {
    signatura: Signatura
    funcion: Expresion
    parametro: Expresion
}


and OperadorApl = {
    op: OperadorExpr
    izq: Expresion
    der: Expresion
}


and Declaracion = {
    mut: bool
    id: IdentificadorExpr
    valor: Expresion
}


type ExprRes =
    | ErrorExpr of string
    | ExitoExpr of Expresion


let crearExpresion stream esFinEntrada =

    let sigTokenExc tipo valor =
        let res = stream ()

        match (res, tipo, valor) with
        | (Error err, _, _) -> failwith err
        | (Exito res, None, _) -> res
        | (Exito res, Some tipo', _) when not (tipo' = res.tipo) ->
            failwithf "Se esperaba un token de tipo %A pero se obtuvo un %A" tipo' res.tipo
        | (Exito res, Some tipo', None) when tipo' = res.tipo -> res
        | (Exito res, Some tipo', Some v) when tipo' = res.tipo ->
            if res.res = v then res
            else failwithf "Se esperaba %s pero se obtuvo %s" v res.res
        | _ -> failwith "Se enviaron datos incorrectos a sigTokenExc."


    let rec sigExpresion nivel: ExprRes =

        let rec sigExprFuncion nivel exprFun =
            try
                let t2 = sigTokenExc None None
                match t2.tipo with
                | Identificador when t2.res = "true" || t2.res = "false" ->
                    sigExprFuncion nivel {
                        signatura = Indefinida 
                        funcion   = FuncionExpr exprFun
                        parametro = BoolExpr {
                            res       = t2.res = "true"
                            posInicio = t2.posInicio
                            posFinal  = t2.posFinal
                            tipo      = t2.tipo
                        }
                    }

                | Identificador | Numero | Texto ->
                    let expresionParametro =
                        match t2.tipo with
                        | Identificador ->
                            IdentificadorExpr {
                                signatura = Indefinida
                                valor     = t2
                            }
                        | Numero ->
                            NumeroExpr {
                                res = float t2.res
                                posInicio = t2.posInicio
                                posFinal = t2.posFinal
                                tipo = t2.tipo
                            }
                        | Texto -> TextoExpr t2
                        | _ -> failwith "Error imposible. Otro tipo de dato se coló."

                    sigExprFuncion nivel {
                        signatura = Indefinida 
                        funcion   = FuncionExpr exprFun
                        parametro = expresionParametro
                    }

                | NuevaLinea -> ExitoExpr <| FuncionExpr exprFun
                | _ -> ErrorExpr "No implementado (Funcion)"
                
            with
            | Failure err -> ErrorExpr err


        let sigExprIdentificador nivel token =
            try
                let t2 = sigTokenExc None None
                match t2.tipo with
                | Identificador when t2.res = "true" || t2.res = "false" ->
                    let t3 = sigTokenExc None None
                    match t3.tipo with
                    | NuevaLinea ->
                        ExitoExpr <| BoolExpr {
                            res       = t3.res = "true"
                            posInicio = t3.posInicio
                            posFinal  = t3.posFinal
                            tipo      = t3.tipo
                        }
                    | _ -> failwith "Token luego de bool no implementado."
                        
                | Identificador | Numero | Texto ->
                    let expresionParametro =
                        match t2.tipo with
                        | Identificador ->
                            IdentificadorExpr {
                                signatura = Indefinida
                                valor     = t2
                            }
                        | Numero ->
                            NumeroExpr {
                                res = float t2.res
                                posInicio = t2.posInicio
                                posFinal = t2.posFinal
                                tipo = t2.tipo
                            }
                        | Texto -> TextoExpr t2
                        | _ -> failwith "Error imposible. Otro tipo de dato se coló."

                    sigExprFuncion nivel {
                        signatura = Indefinida 
                        funcion   = IdentificadorExpr {
                            signatura = Indefinida
                            valor     = token
                        }
                        parametro = expresionParametro
                    }
                | NuevaLinea ->
                    ExitoExpr <| IdentificadorExpr {
                        signatura = Indefinida
                        valor     = token
                    }
                | _ -> ErrorExpr "No implementado (Identificador)"
            with
            | Failure err -> ErrorExpr err


        let sigExprDeclaracion nivel =

            try
                let t2 = sigTokenExc (Some Identificador) None
                let mutable esMutable = false
                let mutable tokenIdentificador = t2

                if t2.res = "mut" then
                    esMutable <- true
                    tokenIdentificador <- sigTokenExc (Some Identificador) None
                    ()
                    
                let tAsignacion = sigTokenExc (Some Operadores) (Some "=")

                let tValor = sigExpresion 0

                match tValor with
                | ErrorExpr err -> tValor
                | ExitoExpr res ->
                    ExitoExpr <| DeclaracionExpr {
                        mut = esMutable
                        id = {
                            signatura = Indefinida
                            valor = tokenIdentificador
                        }
                        valor = res
                    }

            with
            | Failure err -> ErrorExpr err

        
        let resultado: Resultado<string> = stream ()
        match resultado with
        | Error err -> ErrorExpr err
        | Exito token ->
            match token.tipo with
            | Identificador when token.res = "sea" ->
                sigExprDeclaracion nivel
            | Identificador ->
                sigExprIdentificador nivel token
            | Numero ->
                ExitoExpr <| NumeroExpr {
                    res = float token.res
                    posInicio = token.posInicio
                    posFinal = token.posFinal
                    tipo = token.tipo
                }
             | _ ->
                ErrorExpr "No implementado :c"

    let mutable expresiones = []
    while not (esFinEntrada ()) do
        let expr' = sigExpresion 0
        match expr' with
        | ErrorExpr err -> printfn "%s" err
        | ExitoExpr expr ->
            expresiones <- expresiones @ [expr]

    Modulo expresiones
