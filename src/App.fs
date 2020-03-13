module App

open Browser.Dom

let mutable count = 0

let boton = document.getElementById "my-button" :?> Browser.Types.HTMLButtonElement

let entrada = document.getElementById "entrada" :?> Browser.Types.HTMLInputElement



let parsearEntrada () =
    printf "La salida es `%s`" "eehh"
    ()


boton.onclick <- fun _ ->
    parsearEntrada ()

