module AnalisisSintactico.Expresion

open System
open AnalisisLexico.Parser

// ===================================
//  Expresion
// ===================================

type OperadorExpr = OperadorExpr of Exito<string>

type IdentificadorExpr = IdentificadorExpr of Exito<string>

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

