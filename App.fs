module App

open AnalisisLexico.Gramatica
open AnalisisLexico.Parser
open AnalisisSintactico.Expresion
open Generador


let entrada = "sea mut nombre = 22000"

let funs = generarParser entrada
let (extraerSigToken, esFinEntrada) = funs



let transpilar () =
    let expresion = crearExpresion extraerSigToken esFinEntrada
    printfn "%A" expresion


[<EntryPoint>]
let main _ =
    transpilar ()
    0
