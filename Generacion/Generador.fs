module Generador

open AnalisisLexico.Lexer
open AnalisisSintactico.Expresion


let exprEjm = DeclaracionExpr {
    mut = false
    id = {
        signatura = Simple "Txt"
        valor = {
            res = "hola"
            posInicio = 0
            posFinal = 4
            tipo = Nada
        }
    }
    valor = NumeroExpr {
        res = 322.0
        posInicio = 6
        posFinal = 10
        tipo = Texto
    }
}


let rec generarJs (expr: Expresion) =
    
    let generarNumeroExpr valor = valor.res.ToString()

    let generarTextoExpr valor = "\"" + valor.res + "\""

    let generarBoolExpr valor = valor.res.ToString()

    let generarIdentificadorExpr (v: IdentificadorExpr) = v.valor.res

    let generarDeclaracionExpr dec =
        let inicio = if dec.mut then "let" else "const"
        let id = generarIdentificadorExpr dec.id
        let valor = generarJs dec.valor
        inicio + " " + id + " = " + valor + ";"


    match expr with
    | IdentificadorExpr v -> generarIdentificadorExpr v
    | Unidad _ -> "undefined"
    | NumeroExpr v -> generarNumeroExpr v
    | TextoExpr v -> generarTextoExpr v
    | BoolExpr v -> generarBoolExpr v
    | DeclaracionExpr dec -> generarDeclaracionExpr dec
    | _ -> "/* No implementado :c */"




let mfn () =
    printfn "%A" <| generarJs exprEjm
