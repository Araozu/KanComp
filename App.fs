module App

open AnalisisLexico.Gramatica
open AnalisisLexico.Lexer
open AnalisisSintactico.Expresion
open AnalisisSintactico.Parser
open Generador


let entrada = """
sea resultado = repetir "Hola" 10 true

"""

let funs = generarParser entrada
let (extraerSigToken, esFinEntrada) = funs

let rec repl () =
    printf "> "
    let entrada = System.Console.ReadLine ()
    if entrada = ":s" then ()
    else
        let lexer = new Lexer (entrada)
        let expresion = parseTokens lexer
        printfn "%A" expresion
        repl ()


let transpilar () =
    let expresion = crearExpresion extraerSigToken esFinEntrada
    printfn "%A" expresion


[<EntryPoint>]
let main _ =
    repl ()
    0
