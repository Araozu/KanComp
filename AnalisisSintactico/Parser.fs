module AnalisisSintactico.Parser

open AnalisisLexico.Lexer


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
    | EUnidad
    | ENumero of Token2
    | ETexto of Token2
    | EBool of Token2
    | EOperador of EOperador
    | EOperadorApl of EOperadorApl
    | EFuncion of EFuncion
    | EDeclaracion of EDeclaracion
    | EModulo of Expresion list


