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
    | DeclaracionExpr of Declaracion
    | Modulo of Expresion list


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
