module Generador

open AnalisisLexico.Lexer
open AnalisisSintactico.Parser


let exprEjm = EBloque [EDeclaracion {
    mut = true
    id = {
        signatura = Simple "Txt"
        valor = {
            valor = "hola"
            inicio = 0
            final = 4
        }
    }
    valor = ENumero {
        valor = 322.0
        inicio = 6
        final = 10
    }
}]


let rec generarJs (expr: Expresion) toplevel =
    
    let generarJS_ENumero (info: InfoToken<float>) = info.valor.ToString()

    let generarJS_ETexto (info: InfoToken<string>) = "\"" + info.valor + "\""

    let generarJS_EBool (info: InfoToken<bool>) = info.valor.ToString()

    let generarJS_EIdentificador (identificador: EIdentificador) =
        identificador.valor.valor

    let generarJS_EDeclaracion dec =
        let inicio = if dec.mut then "let" else "const"
        let id = generarJS_EIdentificador dec.id
        let valor = generarJs dec.valor false
        inicio + " " + id + " = " + valor + ";"

    let generarJS_EBloque exprs toplevel =

        let rec generarInner exprs =
            match exprs with
            | [] -> ""
            | e :: [] ->
                if toplevel then
                    generarJs e false
                else
                    "return " + generarJs e false
            | e :: es ->
                generarJs e false + "\n\n" + generarInner es 

        generarInner exprs


    match expr with
    | EBloque exprs ->
        generarJS_EBloque exprs toplevel
    | EUnidad _ -> "undefined"
    | ENumero infoToken -> generarJS_ENumero infoToken
    | ETexto info -> generarJS_ETexto info
    | EBool info -> generarJS_EBool info
    | EDeclaracion dec -> generarJS_EDeclaracion dec
    | _ -> "/* No implementado :c */"

let mfn () =
    printfn "%s" <| generarJs exprEjm true
