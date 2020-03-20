module App

open Gramatica
open Parser


let entrada = """
sea a =
          _
        20
"""
let extraerSigToken = generarParser entrada


let rec imprimirTokens () =
    let res = extraerSigToken ()
    match res with
    | Error err ->
        printfn "Error al parsear entrada:\n%A" err
        ()
    | Exito ex ->
        printfn "%A" ex
        imprimirTokens ()


[<EntryPoint>]
let main _ =
    imprimirTokens ()
    0
