module AnalisisSintactico.Expresion

open System
open AnalisisLexico.Parser

// ===================================
//  Expresion
// ===================================

type Signatura =
    | Simple of string
    | Array of Signatura
    | Tupla of Signatura list
    | Funcion of Signatura * Signatura


type OperadorExpr = {
    signatura: Signatura
    valor: Exito<string>
}

type IdentificadorExpr = {
    signatura: Signatura
    valor: Exito<string>
}

type Expresion =
    | IdentificadorExpr of IdentificadorExpr
    | Unidad of Exito<string>
    | NumeroExpr of Exito<float>
    | TextoExpr of Exito<string>
    | BoolExpr of Exito<bool>
    | OperadorExpr of OperadorExpr
    | OperadorAplExpr of OperadorApl
    | DeclaracionExpr of Declaracion


and OperadorApl = {
    op: OperadorExpr
    izq: Expresion
    der: Expresion
}


and Declaracion = {
    mut: bool
    id: IdentificadorExpr
    valor: Expresion
}

