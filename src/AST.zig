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

    pub fn countLocals(function: *FunctionDef) u8 {
        return function.statement.countLocals();
    }
    pub fn genTAC(functionDef: FunctionDef, instructions: *std.ArrayList(*tac.Instruction), allocator: std.mem.Allocator) CodegenError!void {
        try functionDef.statement.genTACInstructions(instructions, allocator);
    }
};
pub const Statement = union(StatementType) {
    Return: Return,

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

pub fn tacBinOpFromASTBinOp(op: BinOp) tac.BinaryOp {
    return switch (op) {
        .ADD => tac.BinaryOp.ADD,
        .SUBTRACT => tac.BinaryOp.SUBTRACT,
        .MULTIPLY => tac.BinaryOp.MULTIPLY,
        .DIVIDE => tac.BinaryOp.DIVIDE,
        .REMAINDER => tac.BinaryOp.REMAINDER,
    };
}

pub const Expression = union(ExpressionType) {
    Integer: u32,
    Unary: Unary,
    Binary: Binary,

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
                            .op = tac.UnaryOp.NEGATE,
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
                            .op = tac.UnaryOp.COMPLEMENT,
                            .src = rhsVal,
                            .dest = lhsVal,
                        } };
                        try instructions.append(instr);
                        return lhsVal;
                    },
                }
            },
            .Binary => |binary| {
                var valLeft = try binary.lhs.genTACInstructions(instructions, allocator);
                var valRight = try binary.rhs.genTACInstructions(instructions, allocator);
                var storeTemp = try allocator.create(tac.Val);
                storeTemp.* = tac.Val{ .Variable = try tempGen.genTemp(allocator) };
                var instr = try allocator.create(tac.Instruction);
                instr.* = tac.Instruction{ .Binary = tac.Binary{
                    .op = tacBinOpFromASTBinOp(binary.op),
                    .left = valLeft,
                    .right = valRight,
                    .dest = storeTemp,
                } };
                try instructions.append(instr);
                return storeTemp;
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
    std.log.warn("program: {any}\n", .{program});
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
