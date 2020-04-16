module App

open AnalisisLexico.Gramatica
open AnalisisLexico.Lexer
open AnalisisSintactico.Expresion
open AnalisisSintactico.Parser
open Generador


let rec repl () =
    printf "> "
    let entrada = System.Console.ReadLine ()
    if entrada = ":s" then ()
    else
        let lexer = new Lexer (entrada)
        let expresion = parseTokens lexer
        printfn "%A" expresion
        repl ()

(*
    Uso:
    --help | -h       Muestra esta informacion
    --repl            Inicia el REPL
    --compile <FILE>  Compila el archivo FILE, imprime el resultado en stdout.
*)


[<EntryPoint>]
let main parametros =
    try
        let comando = parametros.[0]
        match comando with
        | "--repl" ->
            repl ()
        | "--compile" ->
            let direccion = parametros.[1]
            printfn "TODO (%s)" direccion
        | "--help" | "-h" ->
            printfn """
            Uso:
            --help | -h       Muestra esta informacion
            --repl            Inicia el REPL
            --compile <FILE>  Compila el archivo FILE, imprime el resultado en stdout
            """
        | _ ->
            printfn "Comando invalido. Usa --help para obtener ayuda."
    with
    | _ -> eprintfn "Parametro incorrecto. Usa --help para obtener ayuda."
    0
