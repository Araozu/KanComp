module App

open Gramatica
open Parser


let entrada = """
clase Animal 
 
    campo nombre: Txt 
    campo vida: Num  // Puntos de vida del animal. 
    campo dañoAtaque: Num 
 
    constructor nombre vida dañoAtaque 
 
 
    met recibirAtaque daño = 
        @vida -= daño 
        console.log "${@nombre} recibió ${daño} de daño!" 
 
 
    met pub atacar objetivo = 
        console.log "${@nombre} ataca a ${objetivo.nombre}." 
        objetivo.recibirAtaque @dañoAtaque  
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
