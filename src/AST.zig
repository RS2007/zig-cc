const std = @import("std");
const lexer = @import("./lexer.zig");
const parser = @import("./parser.zig");
const tac = @import("./TAC.zig");

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

    pub fn genTemp(self: *TempGenerator, allocator: std.mem.Allocator) CodegenError![]u8 {
        const tempPrefix = "tmp";
        const tempVar = try std.fmt.allocPrint(allocator, "{s}{d}", .{ tempPrefix, self.state });
        self.state += 1;
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
    Unary,
    Binary,
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

    pub fn genTAC(program: *Program, allocator: std.mem.Allocator) CodegenError!std.ArrayList(*tac.Instruction) {
        var instructions = std.ArrayList(*tac.Instruction).init(allocator);
        try program.function.genTAC(&instructions, allocator);
        return instructions;
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
    pub fn genTAC(functionDef: FunctionDef, instructions: *std.ArrayList(*tac.Instruction), allocator: std.mem.Allocator) CodegenError!void {
        try functionDef.statement.genTACInstructions(instructions, allocator);
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

    pub fn genTACInstructions(statement: *Statement, instructions: *std.ArrayList(*tac.Instruction), allocator: std.mem.Allocator) CodegenError!void {
        switch (statement.*) {
            .Return => |retStmt| {
                var returnSymbol = try retStmt.expression.genTACInstructions(instructions, allocator);
                var returnInst = try allocator.create(tac.Instruction);
                returnInst.* = tac.Instruction{ .Return = tac.Return{
                    .val = returnSymbol,
                } };
                try instructions.append(returnInst);
            },
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

pub const UnaryOp = enum {
    NEGATE,
    COMPLEMENT,
};

pub const BinOp = enum {
    ADD,
    SUBTRACT,
    MULTIPLY,
    DIVIDE,
    REMAINDER,
};

pub const Unary = struct {
    unaryOp: UnaryOp,
    exp: *Expression,
};

pub const Binary = struct {
    op: BinOp,
    lhs: *Expression,
    rhs: *Expression,
};

var tempGen = TempGenerator{ .state = 0 };

pub const Expression = union(ExpressionType) {
    Integer: u32,
    Unary: Unary,
    Binary: Binary,

    pub fn codegen(expression: *Expression, allocator: std.mem.Allocator) CodegenError![]u8 {
        switch (expression.*) {
            .Integer => |integer| {
                return try std.fmt.allocPrint(allocator, "mov $0x{x},%rdi\n", .{integer});
            },
            .Unary => |unary| {
                switch (unary.unaryOp) {
                    .NEGATE => {
                        return "";
                    },
                    .COMPLEMENT => {
                        return "";
                    },
                }
            },
            else => {
                unreachable;
            },
        }
    }
    pub fn genTACInstructions(expression: *Expression, instructions: *std.ArrayList(*tac.Instruction), allocator: std.mem.Allocator) CodegenError!*tac.Val {
        switch (expression.*) {
            .Integer => |integer| {
                var val = try allocator.create(tac.Val);
                val.* = tac.Val{ .Constant = integer };
                return val;
            },
            .Unary => |unary| {
                switch (unary.unaryOp) {
                    .NEGATE => {
                        var rhsVal = try unary.exp.genTACInstructions(instructions, allocator);
                        var lhsVal = try allocator.create(tac.Val);
                        var temp = try tempGen.genTemp(allocator);
                        lhsVal.* = tac.Val{
                            .Variable = temp,
                        };
                        var instr = try allocator.create(tac.Instruction);
                        instr.* = tac.Instruction{ .Unary = tac.Unary{
                            .unary = tac.UnaryOp.NEGATE,
                            .src = rhsVal,
                            .dest = lhsVal,
                        } };
                        try instructions.append(instr);
                        return lhsVal;
                    },
                    .COMPLEMENT => {
                        var rhsVal = try unary.exp.genTACInstructions(instructions, allocator);
                        var lhsVal = try allocator.create(tac.Val);
                        var temp = try tempGen.genTemp(allocator);
                        lhsVal.* = tac.Val{
                            .Variable = temp,
                        };
                        var instr = try allocator.create(tac.Instruction);
                        instr.* = tac.Instruction{ .Unary = tac.Unary{
                            .unary = tac.UnaryOp.COMPLEMENT,
                            .src = rhsVal,
                            .dest = lhsVal,
                        } };
                        try instructions.append(instr);
                        return lhsVal;
                    },
                }
            },
            else => {
                unreachable;
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
    std.log.warn("{}", .{program.function.statement.Return.expression.Unary.exp});
    std.log.warn("\n{s}", .{try program.codegen(allocator)});
}

test "codegen TAC" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr = "int main(){ return ~(-2); }";
    var l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    var program = try p.parseProgram();
    var instructions = try program.genTAC(allocator);
    std.log.warn("\n \x1b[34m{any}\x1b[0m", .{instructions.items[0]});
    std.log.warn("\n \x1b[34m{any}\x1b[0m", .{instructions.items[1]});
    std.log.warn("\n \x1b[34m{s}\x1b[0m", .{instructions.items[0].Unary.dest.Variable});
    std.log.warn("\n \x1b[34m{s}\x1b[0m", .{instructions.items[1].Unary.dest.Variable});
    std.log.warn("\n \x1b[34m{any}\x1b[0m", .{instructions.items[2]});
}
