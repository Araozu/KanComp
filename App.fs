module App

open Gramatica
open Parser


let entrada = "sea nombre = 300"
let extraerSigToken = generarParser entrada


let rec imprimirTokens () =
    let res = extraerSigToken ()
    match res with
    | Error _ -> ()
    | Exito ex ->
        printfn "%A" ex
        imprimirTokens ()


[<EntryPoint>]
let main _ =
    imprimirTokens ()
    0
