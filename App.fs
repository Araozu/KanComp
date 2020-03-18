module App

// open Browser.Dom
open Gramatica
open System
open Parser


[<EntryPoint>]
let main _ =
    let entrada = "\t"
    let res = run parserGeneral entrada 0
    printf "El resultado es:\n%A\n\n" res
    0
