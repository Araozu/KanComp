module Generador

open AnalisisLexico.Lexer
open AnalisisSintactico.Parser


let rec generarJs (expr: Expresion) toplevel nivel =

    let indentacionNivel = new string (' ', nivel * 4)
    let indentacionNivelSig = new string (' ' , (nivel + 1) * 4)
    let indentacionNivelAnt =
        if nivel = 0 then ""
        else new string (' ', (nivel - 1) * 4)
    
    let generarJS_ENumero (info: InfoToken<float>) = info.valor.ToString()

    let generarJS_ETexto (info: InfoToken<string>) = "\"" + info.valor + "\""

    let generarJS_EBool (info: InfoToken<bool>) = info.valor.ToString()

    let generarJS_EIdentificador (identificador: EIdentificador) =
        identificador.valor.valor

    let generarJS_EDeclaracion dec =
        let inicio = if dec.mut then "let" else "const"
        let id = generarJS_EIdentificador dec.id
        let valor = generarJs dec.valor false (nivel + 1)
        match dec.valor with
        | EDeclaracion _ ->
            inicio + " " + id + " = " + "(() => {\n" + indentacionNivelSig + valor + "\n" + indentacionNivelSig + "return undefined;\n" + indentacionNivel + "})()"
        | _ ->
            inicio + " " + id + " = " + valor

    let generarJS_EBloque exprs toplevel =

        let rec generarInner exprs =
            indentacionNivel + (
                match exprs with
                | [] -> ""
                | e :: [] ->
                    if toplevel then
                        generarJs e false nivel + ";"
                    else
                        match e with
                        | EDeclaracion _ ->
                            generarJs e false nivel + ";\n" + indentacionNivel + "return undefined;"
                        | _ ->
                            "return " + generarJs e false nivel + ";"
                | e :: es ->
                    generarJs e false nivel + ";" + (if toplevel then "\n" else "") + "\n" + generarInner es
            )

        if toplevel then
            generarInner exprs
        else
            "(() => {\n" + generarInner exprs + "\n" + indentacionNivelAnt + "})()"


    match expr with
    | EBloque exprs ->
        generarJS_EBloque exprs toplevel
    | EUnidad _ -> "undefined"
    | ENumero infoToken -> generarJS_ENumero infoToken
    | ETexto info -> generarJS_ETexto info
    | EBool info -> generarJS_EBool info
    | EIdentificador datos -> generarJS_EIdentificador datos
    | EDeclaracion dec -> generarJS_EDeclaracion dec
    | _ -> "/* No implementado :c */"

