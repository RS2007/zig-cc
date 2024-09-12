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
        const tempVar = try std.fmt.allocPrint(allocator, "{s}{d}", .{ tempPrefix, self.genId() });
        return tempVar;
    }

    pub fn genId(self: *TempGenerator) u32 {
        defer self.state += 1;
        return self.state;
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

    pub fn genTAC(functionDef: FunctionDef, instructions: *std.ArrayList(*tac.Instruction), allocator: std.mem.Allocator) CodegenError!void {
        try functionDef.statement.genTACInstructions(instructions, allocator);
    }
};
pub const Statement = union(StatementType) {
    Return: Return,

    pub fn genTACInstructions(statement: *Statement, instructions: *std.ArrayList(*tac.Instruction), allocator: std.mem.Allocator) CodegenError!void {
        switch (statement.*) {
            .Return => |retStmt| {
                const returnSymbol = try retStmt.expression.genTACInstructions(instructions, allocator);
                const returnInst = try allocator.create(tac.Instruction);
                returnInst.* = tac.Instruction{ .Return = tac.Return{
                    .val = returnSymbol,
                } };
                try instructions.append(returnInst);
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
    LESS_THAN,
    LESS_THAN_EQ,
    GREATER_THAN,
    GREATER_THAN_EQ,
    EQUALS,
    NOT_EQUALS,
    LOGIC_AND,
    LOGIC_OR,
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
        .EQUALS => tac.BinaryOp.EQ,
        .LOGIC_OR => tac.BinaryOp.OR,
        .LOGIC_AND => tac.BinaryOp.AND,
        .NOT_EQUALS => tac.BinaryOp.NOT_EQ,
        .GREATER_THAN => tac.BinaryOp.GT,
        .GREATER_THAN_EQ => tac.BinaryOp.GT_EQ,
        .LESS_THAN => tac.BinaryOp.LT,
        .LESS_THAN_EQ => tac.BinaryOp.LT_EQ,
    };
}

pub const Expression = union(ExpressionType) {
    Integer: u32,
    Unary: Unary,
    Binary: Binary,

    pub fn genTACInstructions(expression: *Expression, instructions: *std.ArrayList(*tac.Instruction), allocator: std.mem.Allocator) CodegenError!*tac.Val {
        switch (expression.*) {
            .Integer => |integer| {
                const val = try allocator.create(tac.Val);
                val.* = tac.Val{ .Constant = integer };
                return val;
            },
            .Unary => |unary| {
                switch (unary.unaryOp) {
                    .NEGATE => {
                        const rhsVal = try unary.exp.genTACInstructions(instructions, allocator);
                        const lhsVal = try allocator.create(tac.Val);
                        const temp = try tempGen.genTemp(allocator);
                        lhsVal.* = tac.Val{
                            .Variable = temp,
                        };
                        const instr = try allocator.create(tac.Instruction);
                        instr.* = tac.Instruction{ .Unary = tac.Unary{
                            .op = tac.UnaryOp.NEGATE,
                            .src = rhsVal,
                            .dest = lhsVal,
                        } };
                        try instructions.append(instr);
                        return lhsVal;
                    },
                    .COMPLEMENT => {
                        const rhsVal = try unary.exp.genTACInstructions(instructions, allocator);
                        const lhsVal = try allocator.create(tac.Val);
                        const temp = try tempGen.genTemp(allocator);
                        lhsVal.* = tac.Val{
                            .Variable = temp,
                        };
                        const instr = try allocator.create(tac.Instruction);
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
                const one = try allocator.create(tac.Val);
                one.* = tac.Val{ .Constant = 1 };
                const zero = try allocator.create(tac.Val);
                zero.* = tac.Val{ .Constant = 0 };
                if (binary.op == BinOp.LOGIC_AND or binary.op == BinOp.LOGIC_OR) {
                    const valLeft = try binary.lhs.genTACInstructions(instructions, allocator);
                    const storeTemp = try allocator.create(tac.Val);
                    storeTemp.* = tac.Val{ .Variable = try tempGen.genTemp(allocator) };
                    if (binary.op == BinOp.LOGIC_AND) {
                        const falseLabel = try std.fmt.allocPrint(allocator, "falseLabel_{d}", .{tempGen.genId()});
                        const endLabel = try std.fmt.allocPrint(allocator, "endLabel_{d}", .{tempGen.genId()});
                        const falseLabelInstr = try allocator.create(tac.Instruction);
                        falseLabelInstr.* = tac.Instruction{
                            .Label = falseLabel,
                        };
                        const jmpIfZeroLeft = try allocator.create(tac.Instruction);
                        jmpIfZeroLeft.* = tac.Instruction{ .JumpIfZero = tac.Jmp{
                            .condition = valLeft,
                            .target = falseLabel,
                        } };
                        try instructions.append(jmpIfZeroLeft);
                        const valRight = try binary.rhs.genTACInstructions(instructions, allocator);
                        const jmpIfZeroRight = try allocator.create(tac.Instruction);
                        jmpIfZeroRight.* = tac.Instruction{ .JumpIfZero = tac.Jmp{
                            .condition = valRight,
                            .target = falseLabel,
                        } };
                        try instructions.append(jmpIfZeroRight);
                        const cpOneToDest = try allocator.create(tac.Instruction);
                        cpOneToDest.* = tac.Instruction{ .Copy = tac.Copy{
                            .src = one,
                            .dest = storeTemp,
                        } };
                        try instructions.append(cpOneToDest);
                        const jmpToEnd = try allocator.create(tac.Instruction);
                        jmpToEnd.* = tac.Instruction{ .Jump = endLabel };
                        try instructions.append(jmpToEnd);
                        try instructions.append(falseLabelInstr);
                        const cpZeroToDest = try allocator.create(tac.Instruction);
                        cpZeroToDest.* = tac.Instruction{ .Copy = tac.Copy{
                            .src = zero,
                            .dest = storeTemp,
                        } };
                        try instructions.append(cpZeroToDest);
                        const endLabelInst = try allocator.create(tac.Instruction);
                        endLabelInst.* = tac.Instruction{ .Label = endLabel };
                        try instructions.append(endLabelInst);
                        return storeTemp;
                    }
                    if (binary.op == BinOp.LOGIC_OR) {
                        const endLabel = try std.fmt.allocPrint(allocator, "endLabel_{d}", .{tempGen.genId()});
                        const trueLabel = try std.fmt.allocPrint(allocator, "trueLabel_{d}", .{tempGen.genId()});
                        const trueLabelInst = try allocator.create(tac.Instruction);
                        trueLabelInst.* = tac.Instruction{
                            .Label = trueLabel,
                        };
                        const jmpIfNotZero = try allocator.create(tac.Instruction);
                        jmpIfNotZero.* = tac.Instruction{ .JumpIfNotZero = tac.Jmp{
                            .condition = valLeft,
                            .target = trueLabel,
                        } };
                        try instructions.append(jmpIfNotZero);
                        const valRight = try binary.rhs.genTACInstructions(instructions, allocator);
                        const jmpIfNotZeroRight = try allocator.create(tac.Instruction);
                        jmpIfNotZeroRight.* = tac.Instruction{ .JumpIfNotZero = tac.Jmp{
                            .condition = valRight,
                            .target = trueLabel,
                        } };
                        try instructions.append(jmpIfNotZeroRight);
                        const cpZeroToDest = try allocator.create(tac.Instruction);
                        cpZeroToDest.* = tac.Instruction{ .Copy = tac.Copy{
                            .src = zero,
                            .dest = storeTemp,
                        } };
                        try instructions.append(cpZeroToDest);
                        const jmpToEnd = try allocator.create(tac.Instruction);
                        jmpToEnd.* = tac.Instruction{ .Jump = endLabel };
                        try instructions.append(jmpToEnd);
                        try instructions.append(trueLabelInst);
                        const cpOneToDest = try allocator.create(tac.Instruction);
                        cpOneToDest.* = tac.Instruction{ .Copy = tac.Copy{
                            .src = one,
                            .dest = storeTemp,
                        } };
                        try instructions.append(cpOneToDest);
                        const endLabelInst = try allocator.create(tac.Instruction);
                        endLabelInst.* = tac.Instruction{ .Label = endLabel };
                        try instructions.append(endLabelInst);
                        return storeTemp;
                    }
                    unreachable;
                }
                const valLeft = try binary.lhs.genTACInstructions(instructions, allocator);
                const valRight = try binary.rhs.genTACInstructions(instructions, allocator);
                const storeTemp = try allocator.create(tac.Val);
                storeTemp.* = tac.Val{ .Variable = try tempGen.genTemp(allocator) };
                const instr = try allocator.create(tac.Instruction);
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
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const program = try p.parseProgram();
    std.log.warn("program: {any}\n", .{program});
}

test "Negation and bitwise complement codegeneration" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr = "int main(){ return ~(-2); }";
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const program = try p.parseProgram();
    std.log.warn("{}", .{program.function.statement.Return.expression.Unary.exp});
}

test "codegen TAC" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr = "int main(){ return ~(-2); }";
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    var program = try p.parseProgram();
    const instructions = try program.genTAC(allocator);
    std.log.warn("\n \x1b[34m{any}\x1b[0m", .{instructions.items[0]});
    std.log.warn("\n \x1b[34m{any}\x1b[0m", .{instructions.items[1]});
    std.log.warn("\n \x1b[34m{s}\x1b[0m", .{instructions.items[0].Unary.dest.Variable});
    std.log.warn("\n \x1b[34m{s}\x1b[0m", .{instructions.items[1].Unary.dest.Variable});
    std.log.warn("\n \x1b[34m{any}\x1b[0m", .{instructions.items[2]});
}

test "codegen TAC with logical and relational ops" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr = "int main(){ return 2 && ( 3 || 4 ) ; }";
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    var program = try p.parseProgram();
    const instructions = try program.genTAC(allocator);
    std.log.warn("\n \x1b[34m{any}\x1b[0m", .{instructions.items[0]});
    std.log.warn("\n \x1b[34m{any}\x1b[0m", .{instructions.items[1]});
}
