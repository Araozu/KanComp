module Utils

open System.IO
open AnalisisLexico.Gramatica
open AnalisisSintactico.Parser
open Generador


type ResIO =
    | ExitoIO of string
    | ErrorIO of string

let obtenerStrArchivo ruta =
    try
        use file = File.Open (ruta, FileMode.Open)
        let tamanoArchivo = file.Length
        printfn "Usando %i bytes de espacio." tamanoArchivo
        let byteArr: byte array = Array.zeroCreate (int tamanoArchivo)
        ignore <| file.Read (byteArr, 0, int tamanoArchivo)
        
        ExitoIO <| System.Text.Encoding.Default.GetString (byteArr)
    with
    | :? FileNotFoundException as ex -> ErrorIO "El archivo no existe."
    | :? DirectoryNotFoundException as ex -> ErrorIO "La ruta provista no existe."
    | :? IOException as ex -> ErrorIO "Error de I/O. Asegúrate de tener permiso para leer el archivo."


let compilarDesdeArchivo ruta =
    match obtenerStrArchivo ruta with
    | ErrorIO err -> eprintf "%s" err
    | ExitoIO codigo ->
        let lexer = new Lexer (codigo)
        let expresion = parseTokens lexer
        printfn "%A" expresion
        match expresion with
        | ErrorParser err -> eprintfn "%s" err
        | ExitoParser expr ->
            printfn "%s" <| generarJs expr true
