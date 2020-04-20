[<RequireQualifiedAccess>]
module AnalisisSintactico.Expect

open AnalisisLexico.Lexer
open AnalisisLexico.Gramatica


let private extraerToken resLexer msgError =
    match resLexer with
    | ErrorLexer err -> failwithf "%s (%s)" msgError err
    | EOF -> failwithf "%s (EOF)" msgError
    | Token (token, indentacion) -> token





let Any resLexer msgError someFnError =
    let fnError = match someFnError with
                  | None -> failwith
                  | Some f -> f

    match resLexer with
    | ErrorLexer err -> fnError (sprintf "%s (%s)" msgError err)
    | EOF -> fnError (sprintf "%s (EOF)" msgError)
    | Token (token, indentacion) -> (token, indentacion)


let TNuevaLinea resLexer valorOpc msgError =
    let preToken = extraerToken resLexer msgError
    try
        let (TNuevaLinea infoToken) = preToken
        match valorOpc with
        | Some v ->
            if infoToken.valor = v then infoToken
            else failwith ""
        | None -> infoToken

    with
    | _ -> failwith msgError


let TIdentificador resLexer valorOpc msgError =
    let preToken = extraerToken resLexer msgError
    try
        let (TIdentificador infoToken) = preToken
        match valorOpc with
        | Some v ->
            if infoToken.valor = v then infoToken
            else failwith ""
        | None -> infoToken

    with
    | _ -> failwith msgError


let PC_SEA resLexer valorOpc msgError =
    let preToken = extraerToken resLexer msgError
    try
        let (PC_SEA infoToken) = preToken
        match valorOpc with
        | Some v ->
            if infoToken.valor = v then infoToken
            else failwith ""
        | None -> infoToken

    with
    | _ -> failwith msgError

let PC_MUT resLexer valorOpc msgError =
    let preToken = extraerToken resLexer msgError
    try
        let (PC_MUT infoToken) = preToken
        match valorOpc with
        | Some v ->
            if infoToken.valor = v then infoToken
            else failwith ""
        | None -> infoToken

    with
    | _ -> failwith msgError



let TOperador resLexer valorOpc msgError =
    let preToken = extraerToken resLexer msgError
    try
        let (TOperador infoToken) = preToken
        match valorOpc with
        | Some v ->
            if infoToken.valor = v then infoToken
            else failwith ""
        | None -> infoToken

    with
    | _ -> failwith msgError
