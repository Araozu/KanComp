module App

open Browser.Dom
open Gramatica
open Parser

let mutable count = 0

let boton = document.getElementById "my-button" :?> Browser.Types.HTMLButtonElement

let entrada = document.getElementById "entrada" :?> Browser.Types.HTMLInputElement



let parsearEntrada () =
    let txtEntrada = entrada.value
    let res = run parseOperadores txtEntrada 0
    printf "La salida es `%A`" res
    ()


boton.onclick <- fun _ ->
    parsearEntrada ()

