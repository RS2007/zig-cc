const std = @import("std");
const lexer = @import("./lexer.zig");
const parser = @import("./parser.zig");

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

const TempGenerator = struct {
    state: u32,

    pub fn genTemp(self: *TempGenerator, allocator: std.mem.Allocator) []u8 {
        const tempPrefix = "tmp";
        const tempVar = try std.fmt.allocPrint(allocator, "{s}{d}", .{ tempPrefix, self.state });
        return tempVar;
    }
};

pub const ASTType = enum {
    Program,
    FunctionDef,
    Statement,
    Expression,
};

pub const CodegenError = error{
    OutOfMemory,
};

pub const StatementType = enum {
    Return,
};
pub const ExpressionType = enum {
    Integer,
};

pub const Program = struct {
    function: FunctionDef,

    pub fn codegen(program: *Program, allocator: std.mem.Allocator) CodegenError![]u8 {
        const fnCode = try program.function.codegen(allocator);
        const asmHeader = ".section .note.GNU-stack,\"\",@progbits\n.text\n.global main";
        var buffer = try allocator.alloc(u8, asmHeader.len + 1 + fnCode.len);
        std.mem.copy(u8, buffer, asmHeader);
        buffer[asmHeader.len] = '\n';
        std.mem.copy(u8, buffer[asmHeader.len + 1 ..], fnCode);
        allocator.free(fnCode);
        return buffer;
    }
};
pub const Return = struct {
    expression: *Expression,
};
pub const FunctionDef = struct {
    name: []u8,
    statement: *Statement,

    pub fn codegen(function: *FunctionDef, allocator: std.mem.Allocator) CodegenError![]u8 {
        //const statementCode = function.statement.codegen();
        const statementCode = try function.statement.codegen(allocator);
        const functionCode = try allocator.alloc(u8, function.name.len + 3 + statementCode.len);
        std.mem.copy(u8, functionCode[0..], function.name);
        functionCode[function.name.len + 1] = ':';
        functionCode[function.name.len + 2] = '\n';
        std.mem.copy(u8, functionCode[function.name.len + 3 ..], statementCode);
        allocator.free(statementCode);
        return functionCode;
    }

    pub fn countLocals(function: *FunctionDef) u8 {
        return function.statement.countLocals();
    }
};
pub const Statement = union(StatementType) {
    Return: Return,

    pub fn codegen(statement: *Statement, allocator: std.mem.Allocator) CodegenError![]u8 {
        switch (statement.*) {
            .Return => |retStmt| {
                const expression = try retStmt.expression.codegen(allocator);
                const returnCodeInst = "movq $0x3c,%rax\nsyscall\nleaveq\nretq";
                defer allocator.free(expression);
                var buf = try allocator.alloc(u8, expression.len + returnCodeInst.len);
                std.mem.copy(
                    u8,
                    buf,
                    expression,
                );
                std.mem.copy(
                    u8,
                    buf[expression.len..],
                    returnCodeInst,
                );
                return buf;
            },
            //else => {
            //    std.debug.assert(false);
            //},
        }
    }

    pub fn countLocals(statement: *Statement) u8 {
        switch (statement.*) {
            .Return => |retStmt| {
                return retStmt.expression.countLocals();
            },
        }
    }
};

pub const Expression = union(ExpressionType) {
    Integer: u32,

    pub fn codegen(expression: *Expression, allocator: std.mem.Allocator) CodegenError![]u8 {
        switch (expression.*) {
            .Integer => |integer| {
                return try std.fmt.allocPrint(allocator, "mov $0x{x},%rdi\n", .{integer});
            },
        }
    }
    pub fn countLocals(expression: *Expression) u8 {
        switch (expression.*) {
            .Integer => {
                return 8;
            },
        }
        std.debug.assert(false);
        return 0;
    }
};

pub const Node = union(enum) {
    Program: Program,
    FunctionDef: FunctionDef,
    Statement: Statement,
    Expression: Expression,
};

test "Codegenerator basic" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr = "int main(){ return 42; }";
    var l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    var program = try p.parseProgram();
    std.log.warn("\n{s}", .{try program.codegen(allocator)});
}

test "Negation and bitwise complement codegeneration" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr = "int main(){ return ~(-2); }";
    var l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    var program = try p.parseProgram();
    std.log.warn("\n{s}", .{try program.codegen(allocator)});
}
