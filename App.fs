module App

open AnalisisLexico.Gramatica
open AnalisisLexico.Parser
open AnalisisSintactico.Expresion
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
        let (streamFun, esFinEntrada) = generarParser <| entrada + "\n"
        let expresion = crearExpresion streamFun esFinEntrada
        printfn "%A" expresion
        repl ()


let transpilar () =
    let expresion = crearExpresion extraerSigToken esFinEntrada
    printfn "%A" expresion


[<EntryPoint>]
let main _ =
    repl ()
    0
