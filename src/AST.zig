const std = @import("std");
const lexer = @import("./lexer.zig");

//<program> ::= <function>
//
//<function> ::= "int" <identifier> "(" "void" ")" "{" <statement> "}"
//
//<statement> ::= "return" <exp> ";"
//
//<exp> ::= <int>
//
//<identifier> ::= ? An identifier token ?
//
//<int> ::= ? A constant token ?

pub const ASTType = enum {
    Program,
    FunctionDef,
    Statement,
    Expression,
};

pub const StatementType = enum {};
pub const ExpressionType = enum {};

pub const Program = struct {
    function: FunctionDef,
};
pub const Return = struct {
    expression: *Expression,
};
pub const FunctionDef = struct {
    name: *lexer.Token,
    statement: *Statement,
};
pub const Statement = union(StatementType) {
    Return: Return,
};
pub const Expression = union(ExpressionType) {
    Integer: u32,
};

pub const Node = union(enum) {
    Program: Program,
    FunctionDef: FunctionDef,
    Statement: Statement,
    Expression: Expression,
};
