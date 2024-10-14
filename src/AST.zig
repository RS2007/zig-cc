const std = @import("std");
const lexer = @import("./lexer.zig");
const parser = @import("./parser.zig");
const tac = @import("./TAC.zig");
const semantic = @import("./semantic.zig");

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

pub const MemoryError = error{
    OutOfMemory,
};

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
    BlockItem,
    Statement,
    Expression,
};

pub const BlockItemType = enum {
    Statement,
    Declaration,
};

pub const If = struct {
    condition: *Expression,
    thenStmt: *Statement,
    elseStmt: ?*Statement = null,
};

pub const BlockItem = union(BlockItemType) {
    Statement: *Statement,
    Declaration: *Declaration,
    const Self = @This();

    pub fn genTAC(self: Self, instructions: *std.ArrayList(*tac.Instruction), allocator: std.mem.Allocator) CodegenError!void {
        switch (self) {
            .Statement => |stmt| {
                try stmt.genTACInstructions(instructions, allocator);
            },
            .Declaration => |decl| {
                try decl.genTACInstructions(instructions, allocator);
            },
        }
    }
};

pub const Declaration = struct {
    name: []u8,
    expression: ?*Expression,

    const Self = @This();
    pub fn genTACInstructions(self: Self, instructions: *std.ArrayList(*tac.Instruction), allocator: std.mem.Allocator) CodegenError!void {
        const hasExpr = self.expression;
        if (hasExpr) |expression| {
            const rhs = try expression.genTACInstructions(instructions, allocator);
            const instr = try allocator.create(tac.Instruction);
            const lhs = try allocator.create(tac.Val);
            lhs.* = tac.Val{ .Variable = self.name };
            instr.* = tac.Instruction{ .Copy = tac.Copy{ .src = rhs, .dest = lhs } };
            try instructions.append(instr);
        }
        //const varInst = try allocator.create(tac.Instruction);
        //const cpInstr = try allocator.create(tac.Instruction);
        //try instructions.append();
        //try instructions.append();
    }
};

pub const CodegenError = error{
    OutOfMemory,
};

pub const StatementType = enum {
    Return,
    If,
    Expression,
    Null,
    Goto,
    Label,
    Compound,
    Break,
    Continue,
    DoWhile,
    While,
    For,
};
pub const ExpressionType = enum {
    Integer,
    Unary,
    Binary,
    Ternary,
    Identifier,
    Assignment,
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
    blockItems: std.ArrayList(*BlockItem),

    pub fn genTAC(functionDef: FunctionDef, instructions: *std.ArrayList(*tac.Instruction), allocator: std.mem.Allocator) CodegenError!void {
        for (functionDef.blockItems.items) |blockItem| {
            try blockItem.genTAC(instructions, allocator);
        }
    }
};
pub const While = struct {
    condition: *Expression,
    body: *Statement,
};
pub const For = struct {};

pub const Statement = union(StatementType) {
    Return: Return,
    If: If,
    Expression: *Expression,
    Null: void,
    Goto: []u8,
    Label: []u8,
    Compound: std.ArrayList(*BlockItem),
    Break: void,
    Continue: void,
    DoWhile: While,
    While: While,
    For: For,

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
            .Expression => |expr| {
                _ = try expr.genTACInstructions(instructions, allocator);
            },
            .Null => {},
            .If => |ifStmt| {
                const jmpIfZero = try allocator.create(tac.Instruction);
                const condVal = try ifStmt.condition.genTACInstructions(instructions, allocator);
                const falseLabel = try allocator.create(tac.Instruction);
                const falseLabelName = try std.fmt.allocPrint(allocator, "falseLabel_{d}", .{tempGen.genId()});
                const exitLabelName = try std.fmt.allocPrint(allocator, "exitLabel_{d}", .{tempGen.genId()});
                const exitLabel = try allocator.create(tac.Instruction);
                exitLabel.* = tac.Instruction{
                    .Label = exitLabelName,
                };
                const unCondJmpFromTrue = try allocator.create(tac.Instruction);
                unCondJmpFromTrue.* = tac.Instruction{ .Jump = exitLabelName };
                falseLabel.* = tac.Instruction{
                    .Label = falseLabelName,
                };
                jmpIfZero.* = tac.Instruction{ .JumpIfZero = tac.Jmp{
                    .condition = condVal,
                    .target = falseLabelName,
                } };
                try instructions.append(jmpIfZero);
                try ifStmt.thenStmt.genTACInstructions(instructions, allocator);
                try instructions.append(unCondJmpFromTrue);
                try instructions.append(falseLabel);
                if (ifStmt.elseStmt) |elseStmt| {
                    try elseStmt.genTACInstructions(instructions, allocator);
                }
                try instructions.append(exitLabel);
            },
            .Label => |label| {
                const tacLabel = try allocator.create(tac.Instruction);
                tacLabel.* = tac.Instruction{
                    .Label = label,
                };
                try instructions.append(tacLabel);
            },
            .Goto => |goto| {
                const tacUncondJump = try allocator.create(tac.Instruction);
                tacUncondJump.* = tac.Instruction{
                    .Jump = goto,
                };
                try instructions.append(tacUncondJump);
            },
            .Compound => |compound| {
                for (compound.items) |compoundStatement| {
                    try compoundStatement.genTAC(instructions, allocator);
                }
            },
            .Continue => {},
            .Break => {},
            .DoWhile => {},
            .While => {},
            .For => {},
        }
    }
};

pub fn prettyPrintAST(node: Node, writer: anytype, depth: usize) !void {
    const colors = struct {
        const reset = "\x1b[0m";
        const bold = "\x1b[1m";
        const red = "\x1b[31m";
        const green = "\x1b[32m";
        const yellow = "\x1b[33m";
        const blue = "\x1b[34m";
        const magenta = "\x1b[35m";
        const cyan = "\x1b[36m";
    };

    try writer.writeByteNTimes(' ', depth * 2);
    try writer.print("{s}├─ ", .{colors.bold});

    switch (node) {
        .Program => |program| {
            try writer.print("\n{s}Program{s}\n", .{ colors.red, colors.reset });
            try prettyPrintAST(Node{ .FunctionDef = &program.function }, writer, depth + 1);
        },
        .FunctionDef => |func| {
            try writer.print("{s}Function: {s}{s}\n", .{ colors.green, func.name, colors.reset });
            for (func.blockItems.items, 0..) |item, i| {
                const is_last = i == func.blockItems.items.len - 1;
                try writer.writeByteNTimes(' ', (depth + 1) * 2);
                try writer.print("{s}{s} ", .{ colors.bold, if (is_last) "└─" else "├─" });
                switch (item.*) {
                    .Statement => |stmt| try prettyPrintAST(Node{ .Statement = stmt }, writer, depth + 2),
                    .Declaration => |decl| try prettyPrintAST(Node{ .Declaration = decl }, writer, depth + 2),
                }
            }
        },
        .Declaration => |decl| {
            try writer.print("{s}Declaration: {s}{s}\n", .{ colors.yellow, decl.name, colors.reset });
        },
        .Statement => |stmt| {
            switch (stmt.*) {
                .Return => |ret| {
                    try writer.print("{s}Return{s}\n", .{ colors.blue, colors.reset });
                    try prettyPrintAST(Node{ .Expression = ret.expression }, writer, depth + 5);
                },
                .Expression => |expr| {
                    try writer.print("{s}Expression Statement{s}\n", .{ colors.magenta, colors.reset });
                    try prettyPrintAST(Node{ .Expression = expr }, writer, depth + 1);
                },
                .Null => try writer.print("{s}Null Statement{s}\n", .{ colors.cyan, colors.reset }),
                //TODO: implement if pretty printer properly
                .If => try writer.print("{s} If {s}\n", .{ colors.cyan, colors.reset }),
                .Label => |label| try writer.print("{s} Label: {s}{s}", .{ colors.cyan, label, colors.reset }),
                .Goto => |goto| try writer.print("{s} Goto: {s}{s}", .{ colors.cyan, goto, colors.reset }),
                .Compound => try writer.print("{s} Compound: TODO{s}", .{ colors.cyan, colors.reset }),
            }
        },
        .Expression => |expr| {
            switch (expr.*) {
                .Integer => |int| try writer.print("{s}Integer: {d}{s}\n", .{ colors.yellow, int, colors.reset }),
                .Unary => |unary| {
                    try writer.print("{s}Unary: {s}{s}\n", .{ colors.magenta, @tagName(unary.unaryOp), colors.reset });
                    try prettyPrintAST(Node{ .Expression = unary.exp }, writer, depth + 1);
                },
                .Binary => |binary| {
                    try writer.print("{s}Binary: {any}{s}\n", .{ colors.cyan, @tagName(binary.op), colors.reset });
                    try prettyPrintAST(Node{ .Expression = binary.lhs }, writer, depth + 1);
                    try prettyPrintAST(Node{ .Expression = binary.rhs }, writer, depth + 1);
                },
                .Assignment => |ass| {
                    try writer.print("{s}Assignment: {s}\n", .{ colors.cyan, colors.reset });
                    try prettyPrintAST(Node{ .Expression = ass.lhs }, writer, depth + 1);
                    try prettyPrintAST(Node{ .Expression = ass.rhs }, writer, depth + 1);
                },
                .Identifier => |ident| try writer.print("{s}Identifier: {s}{s}\n", .{ colors.green, ident, colors.reset }),
                .Ternary => |tern| {
                    try writer.print("{s}Ternary{s}\n", .{ colors.cyan, colors.reset });
                    try prettyPrintAST(Node{ .Expression = tern.condition }, writer, depth + 1);
                    try prettyPrintAST(Node{ .Expression = tern.lhs }, writer, depth + 1);
                    try prettyPrintAST(Node{ .Expression = tern.rhs }, writer, depth + 1);
                },
            }
        },
    }
}

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

pub const Ternary = struct {
    condition: *Expression,
    lhs: *Expression,
    rhs: *Expression,
};

pub var tempGen = TempGenerator{ .state = 0 };

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

pub const Assignment = struct {
    lhs: *Expression,
    rhs: *Expression,
};

pub const Expression = union(ExpressionType) {
    Integer: u32,
    Unary: Unary,
    Binary: Binary,
    Ternary: Ternary,
    Identifier: []u8,
    Assignment: Assignment,

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
            .Identifier => |iden| {
                const tacVal = try allocator.create(tac.Val);
                tacVal.* = tac.Val{
                    .Variable = iden,
                };
                return tacVal;
            },
            .Assignment => |assignment| {
                const lhs = try assignment.lhs.genTACInstructions(instructions, allocator);
                const rhs = try assignment.rhs.genTACInstructions(instructions, allocator);
                const cpInstr = try allocator.create(tac.Instruction);
                cpInstr.* = tac.Instruction{ .Copy = tac.Copy{
                    .src = rhs,
                    .dest = lhs,
                } };
                try instructions.append(cpInstr);
                return lhs;
            },
            .Ternary => |ternary| {
                const comparision = try ternary.condition.genTACInstructions(instructions, allocator);
                const storeTemp = try allocator.create(tac.Val);
                storeTemp.* = tac.Val{
                    .Variable = try tempGen.genTemp(allocator),
                };
                const jmpIfZeroInst = try allocator.create(tac.Instruction);
                const endLabel = try allocator.create(tac.Instruction);
                const falseLabel = try allocator.create(tac.Instruction);
                const falseLabelName = try std.fmt.allocPrint(allocator, "falseLabel_{d}", .{tempGen.genId()});
                const endLabelName = try std.fmt.allocPrint(allocator, "endLabel_{d}", .{tempGen.genId()});
                falseLabel.* = tac.Instruction{
                    .Label = falseLabelName,
                };
                endLabel.* = tac.Instruction{
                    .Label = endLabelName,
                };
                jmpIfZeroInst.* = tac.Instruction{
                    .JumpIfZero = .{
                        .condition = comparision,
                        .target = falseLabelName,
                    },
                };
                try instructions.append(jmpIfZeroInst);
                const middle = try ternary.lhs.genTACInstructions(instructions, allocator);
                const cpMiddleToDest = try allocator.create(tac.Instruction);
                cpMiddleToDest.* = tac.Instruction{
                    .Copy = .{
                        .dest = storeTemp,
                        .src = middle,
                    },
                };
                try instructions.append(cpMiddleToDest);
                const uncondJumpFromTrue = try allocator.create(tac.Instruction);
                uncondJumpFromTrue.* = tac.Instruction{
                    .Jump = endLabelName,
                };
                try instructions.append(uncondJumpFromTrue);
                try instructions.append(falseLabel);
                const end = try ternary.rhs.genTACInstructions(instructions, allocator);
                const cpEndToDest = try allocator.create(tac.Instruction);
                cpEndToDest.* = tac.Instruction{
                    .Copy = .{
                        .src = end,
                        .dest = storeTemp,
                    },
                };
                try instructions.append(cpEndToDest);
                try instructions.append(endLabel);
                return storeTemp;
            },
        }
    }
};

pub fn expressionScopeVariableResolve(expression: *Expression, currentScope: u32, allocator: std.mem.Allocator) MemoryError!void {
    switch (expression.*) {
        .Integer => {},
        .Identifier => |identifier| {
            expression.Identifier =
                try std.fmt.allocPrint(allocator, "{s}{d}", .{ identifier, currentScope });
        },
        .Assignment => |assignment| {
            try expressionScopeVariableResolve(assignment.lhs, currentScope, allocator);
            try expressionScopeVariableResolve(assignment.rhs, currentScope, allocator);
        },
        .Ternary => |ternary| {
            try expressionScopeVariableResolve(ternary.condition, currentScope, allocator);
            try expressionScopeVariableResolve(ternary.lhs, currentScope, allocator);
            try expressionScopeVariableResolve(ternary.rhs, currentScope, allocator);
        },
        .Unary => |unary| {
            try expressionScopeVariableResolve(unary.exp, currentScope, allocator);
        },
        .Binary => |binary| {
            try expressionScopeVariableResolve(binary.lhs, currentScope, allocator);
            try expressionScopeVariableResolve(binary.rhs, currentScope, allocator);
        },
    }
}

pub fn blockStatementScopeVariableResolve(blockItem: *BlockItem, currentScope: u32, allocator: std.mem.Allocator) MemoryError!void {
    switch (blockItem.*) {
        .Statement => |statement| {
            try statementScopeVariableResolve(statement, currentScope, allocator);
        },
        .Declaration => |decl| {
            blockItem.Declaration.name = try std.fmt.allocPrint(allocator, "{s}{d}", .{ decl.name, currentScope });
        },
    }
}
pub fn statementScopeVariableResolve(statement: *Statement, currentScope: u32, allocator: std.mem.Allocator) MemoryError!void {
    switch (statement.*) {
        .Compound => |compound| {
            for (compound.items) |blockItemCompound| {
                try blockStatementScopeVariableResolve(blockItemCompound, currentScope + 1, allocator);
            }
        },
        .Null, .Label, .Goto => {},
        .If => |ifStmt| {
            try expressionScopeVariableResolve(ifStmt.condition, currentScope, allocator);
            try statementScopeVariableResolve(ifStmt.thenStmt, currentScope, allocator);
            if (ifStmt.elseStmt) |elseStmt| {
                try statementScopeVariableResolve(elseStmt, currentScope, allocator);
            }
        },
        .Return => |ret| {
            try expressionScopeVariableResolve(ret.expression, currentScope, allocator);
        },
        .Expression => |expression| {
            try expressionScopeVariableResolve(expression, currentScope, allocator);
        },
    }
}

pub fn scopeVariableResolutionPass(program: *Program, allocator: std.mem.Allocator) MemoryError!void {
    for (program.function.blockItems.items) |blockItem| {
        try blockStatementScopeVariableResolve(blockItem, 0, allocator);
    }
}

pub const Node = union(enum) {
    Program: *Program,
    Declaration: *Declaration,
    FunctionDef: *FunctionDef,
    Statement: *Statement,
    Expression: *Expression,
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
    std.log.warn("{}", .{program.function.blockItems.items[0].Statement.Return.expression.Unary.exp});
}

test "Declarations" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr = "int main(){ int x = 2; int y = -x; return ~y; }";
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const program = try p.parseProgram();
    try semantic.varResolutionPass(allocator, program);
    const stdout = std.io.getStdOut().writer();
    try prettyPrintAST(Node{ .Program = program }, stdout, 0);
    std.log.warn("{any}", .{program.function.blockItems.items[2].Statement.Return.expression.Unary.exp});
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

test "codegen TAC with declarations" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr = "int main(){ int x = 2; int y = 3 || 4; return x && y; }";
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    var program = try p.parseProgram();
    const instructions = try program.genTAC(allocator);
    std.log.warn("\n \x1b[34m{any}\x1b[0m", .{instructions.items[0]});
    std.log.warn("\n \x1b[34m{any}\x1b[0m", .{instructions.items[1]});
}

test "test tac generation for ternary" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr = "int main(){int y = 4; int x = 3;return x == 3?x:y}";
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const program = try p.parseProgram();
    _ = try program.genTAC(allocator);
    //for(instructions.items,0..) |inst,i| {
    //    std.log.warn("Inst at {}: {any}\n",.{i,inst});
    //}
}
