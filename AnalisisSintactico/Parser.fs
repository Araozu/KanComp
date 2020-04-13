module AnalisisSintactico.Parser

open AnalisisLexico.Lexer
open AnalisisLexico.Gramatica


// ===================================
//  Expresion
// ===================================

type Signatura =
    | Indefinida
    | Simple of string
    | Array of Signatura
    | Tupla of Signatura list
    | Funcion of Signatura * Signatura

type EIdentificador = {
    signatura: Signatura
    valor: Token2
}

and EOperador = {
    signatura: Signatura
    valor: Exito<string>
}

and EOperadorApl = {
    op: EOperador
    izq: Expresion
    der: Expresion
}

and EFuncion = {
    signatura: Signatura
    fn: Expresion
    param: Expresion
}

and EDeclaracion = {
    mut: bool
    id: EIdentificador
    valor: Expresion
}


and Expresion =
    | EIdentificador of EIdentificador
    | EUnidad of Token2
    | ENumero of Token2
    | ETexto of Token2
    | EBool of Token2
    | EOperador of EOperador
    | EOperadorApl of EOperadorApl
    | EFuncion of EFuncion
    | EDeclaracion of EDeclaracion
    | EModulo of Expresion list


type ExprRes =
    | ErrorExpr of string
    | ExitoExpr of Expresion
    | EOF


type RLexer =
    | RToken of Token2
    | EOF


let parseTokens (lexer: Lexer) =

    let sigTokenExc () : RLexer =
        let res = lexer.SigToken ()

        match res with
        | ResLexer.EOF -> RLexer.EOF
        | ErrorLexer err -> failwith err
        | Token t -> RToken t

    let rec sigExpresion nivel =
        ()


    
    
    ()
