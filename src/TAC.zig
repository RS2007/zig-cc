const std = @import("std");
const ast = @import("./AST.zig");
const assembly = @import("./Assembly.zig");
const lexer = @import("./lexer.zig");
const parser = @import("./parser.zig");
const semantic = @import("./semantic.zig");

pub const FunctionDef = struct {
    name: []u8,
    global: bool,
    args: std.ArrayList([]u8),
    //TODO: should I be putting type information here? We only have ints for now, so names are fine (FOR NOW)
    instructions: std.ArrayList(*Instruction),
};

pub const StaticVar = struct {
    name: []u8,
    global: bool,
    init: u32,
};

pub const Program = struct {
    topLevelDecls: std.ArrayList(*TopLevel),
    symbolTable: std.StringHashMap(*semantic.Symbol),
    pub fn codegen(self: *Program, allocator: std.mem.Allocator) !*assembly.Program {
        const topLevelDecls = std.ArrayList(*assembly.TopLevelDecl).init(allocator);
        const asmProgram = try allocator.create(assembly.Program);
        asmProgram.* = assembly.Program{ .topLevelDecls = topLevelDecls };
        for (self.topLevelDecls.items) |topLevelDecl| {
            const asmTopLevelDecl = try allocator.create(assembly.TopLevelDecl);
            switch (topLevelDecl.*) {
                .StaticVar => |statItem| {
                    const staticVar = try allocator.create(assembly.StaticVar);
                    staticVar.* = .{
                        .name = statItem.name,
                        .global = statItem.global,
                        .init = statItem.init,
                    };
                    asmTopLevelDecl.* = .{
                        .StaticVar = staticVar,
                    };
                    try asmProgram.topLevelDecls.append(asmTopLevelDecl);
                },
                .Function => |fnItem| {
                    const func = try allocator.create(assembly.Function);
                    func.* = .{
                        .name = fnItem.name,
                        .instructions = std.ArrayList(*assembly.Instruction).init(allocator),
                        .args = fnItem.args,
                    };
                    const registers = [_]assembly.Reg{ assembly.Reg.EDI, assembly.Reg.ESI, assembly.Reg.EDX, assembly.Reg.ECX, assembly.Reg.R8, assembly.Reg.R9 };
                    for (fnItem.args.items, 0..) |arg, i| {
                        const movInstructoin = try allocator.create(assembly.Instruction);
                        movInstructoin.* = assembly.Instruction{ .Mov = assembly.MovInst{
                            .src = assembly.Operand{ .Reg = registers[i] },
                            .dest = assembly.Operand{ .Pseudo = arg },
                        } };
                        try func.instructions.append(movInstructoin);
                    }
                    for (fnItem.instructions.items) |instruction| {
                        try instruction.codegen(self.symbolTable, &func.instructions, allocator);
                    }
                    asmTopLevelDecl.* = .{
                        .Function = func,
                    };
                    try asmProgram.topLevelDecls.append(asmTopLevelDecl);
                },
            }
        }
        return asmProgram;
    }
};

pub const TopLevelType = enum {
    Function,
    StaticVar,
};
pub const TopLevel = union(TopLevelType) {
    Function: *FunctionDef,
    StaticVar: *StaticVar,
};

pub const InstructionType = enum {
    Return,
    Unary,
    Binary,
    Copy,
    Jump,
    JumpIfZero,
    JumpIfNotZero,
    Label,
    FunctionCall,
};

pub const Return = struct {
    val: *Val,
};

pub const UnaryOp = enum {
    NEGATE,
    COMPLEMENT,
};

pub const BinaryOp = enum {
    ADD,
    SUBTRACT,
    MULTIPLY,
    DIVIDE,
    REMAINDER,
    EQ,
    NOT_EQ,
    LT,
    LT_EQ,
    GT,
    GT_EQ,
    OR,
    AND,
};

pub const Unary = struct {
    op: UnaryOp,
    src: *Val,
    dest: *Val,
};

pub const Binary = struct {
    op: BinaryOp,
    left: *Val,
    right: *Val,
    dest: *Val,
};

fn tacOpToAssemblyOp(op: BinaryOp) assembly.BinaryOp {
    switch (op) {
        .ADD => {
            return assembly.BinaryOp.Add;
        },
        .SUBTRACT => {
            return assembly.BinaryOp.Subtract;
        },
        .MULTIPLY => {
            return assembly.BinaryOp.Multiply;
        },
        else => unreachable,
    }
}

pub const Copy = struct {
    src: *Val,
    dest: *Val,
};

pub const Jmp = struct {
    condition: *Val,
    target: []u8,
};

pub const FunctionCall = struct {
    name: []u8,
    args: std.ArrayList(*Val),
    dest: *Val,
};

pub const Instruction = union(InstructionType) {
    Return: Return,
    Unary: Unary,
    Binary: Binary,
    Copy: Copy,
    Jump: []u8,
    JumpIfZero: Jmp,
    JumpIfNotZero: Jmp,
    Label: []u8,
    FunctionCall: FunctionCall,
    pub fn codegen(instruction: *Instruction, symbolTable: std.StringHashMap(*semantic.Symbol), instructions: *std.ArrayList(*assembly.Instruction), allocator: std.mem.Allocator) ast.CodegenError!void {
        switch (instruction.*) {
            .Return => |ret| {
                const val = try ret.val.codegen(symbolTable, allocator);
                const movInst = try allocator.create(assembly.Instruction);
                movInst.* = assembly.Instruction{ .Mov = assembly.MovInst{
                    .src = val,
                    .dest = assembly.Operand{ .Reg = assembly.Reg.AX },
                } };
                const retInst = try allocator.create(assembly.Instruction);
                retInst.* = assembly.Instruction{ .Ret = {} };
                try instructions.append(movInst);
                try instructions.append(retInst);
            },
            .Unary => |unary| {
                const dest = try unary.dest.codegen(symbolTable, allocator);
                const src = try unary.src.codegen(symbolTable, allocator);
                const movInst = try allocator.create(assembly.Instruction);
                const unaryInst = try allocator.create(assembly.Instruction);
                movInst.* = assembly.Instruction{
                    .Mov = assembly.MovInst{
                        .src = src,
                        .dest = dest,
                    },
                };
                switch (unary.op) {
                    .NEGATE => {
                        unaryInst.* = assembly.Instruction{
                            .Unary = assembly.UnaryInst{
                                .op = assembly.UnaryOp.Neg,
                                .rhs = dest,
                            },
                        };
                    },
                    .COMPLEMENT => {
                        unaryInst.* = assembly.Instruction{
                            .Unary = assembly.UnaryInst{
                                .op = assembly.UnaryOp.Not,
                                .rhs = dest,
                            },
                        };
                    },
                }
                try instructions.append(movInst);
                try instructions.append(unaryInst);
            },
            .Binary => |binary| {
                const storeDest = try binary.dest.codegen(symbolTable, allocator);
                const left = try binary.left.codegen(symbolTable, allocator);
                const right = try binary.right.codegen(symbolTable, allocator);
                switch (binary.op) {
                    .ADD, .SUBTRACT, .MULTIPLY => {
                        const movLeftToDest = try allocator.create(assembly.Instruction);
                        movLeftToDest.* = assembly.Instruction{ .Mov = assembly.MovInst{
                            .src = left,
                            .dest = storeDest,
                        } };
                        const binaryInstr = try allocator.create(assembly.Instruction);
                        binaryInstr.* = assembly.Instruction{ .Binary = assembly.BinaryInst{
                            .lhs = storeDest,
                            .rhs = right,
                            .op = tacOpToAssemblyOp(binary.op),
                        } };
                        try instructions.append(movLeftToDest);
                        try instructions.append(binaryInstr);
                    },
                    .DIVIDE => {
                        const movLeftToAX = try allocator.create(assembly.Instruction);
                        const cdqInstr = try allocator.create(assembly.Instruction);
                        const idivWithRight = try allocator.create(assembly.Instruction);
                        const movAXToDest = try allocator.create(assembly.Instruction);
                        movLeftToAX.* = assembly.Instruction{ .Mov = assembly.MovInst{
                            .src = left,
                            .dest = assembly.Operand{ .Reg = assembly.Reg.AX },
                        } };
                        cdqInstr.* = assembly.Instruction{
                            .Cdq = {},
                        };
                        idivWithRight.* = assembly.Instruction{
                            .Idiv = right,
                        };
                        movAXToDest.* = assembly.Instruction{ .Mov = assembly.MovInst{
                            .src = assembly.Operand{ .Reg = assembly.Reg.AX },
                            .dest = storeDest,
                        } };
                        try instructions.append(movLeftToAX);
                        try instructions.append(cdqInstr);
                        try instructions.append(idivWithRight);
                        try instructions.append(movAXToDest);
                    },
                    .REMAINDER => {
                        const movLeftToDX = try allocator.create(assembly.Instruction);
                        const cdqInstr = try allocator.create(assembly.Instruction);
                        const idivWithRight = try allocator.create(assembly.Instruction);
                        const movDXToDest = try allocator.create(assembly.Instruction);
                        movLeftToDX.* = assembly.Instruction{ .Mov = assembly.MovInst{
                            .src = left,
                            .dest = assembly.Operand{ .Reg = assembly.Reg.AX },
                        } };
                        cdqInstr.* = assembly.Instruction{
                            .Cdq = {},
                        };
                        idivWithRight.* = assembly.Instruction{
                            .Idiv = right,
                        };
                        movDXToDest.* = assembly.Instruction{ .Mov = assembly.MovInst{
                            .src = assembly.Operand{ .Reg = assembly.Reg.DX },
                            .dest = storeDest,
                        } };
                        try instructions.append(movLeftToDX);
                        try instructions.append(cdqInstr);
                        try instructions.append(idivWithRight);
                        try instructions.append(movDXToDest);
                    },
                    .EQ, .NOT_EQ, .LT, .LT_EQ, .GT, .GT_EQ => {
                        const cmpInstr = try allocator.create(assembly.Instruction);
                        const movLeftToDest = try allocator.create(assembly.Instruction);
                        movLeftToDest.* = assembly.Instruction{ .Mov = assembly.MovInst{
                            .src = left,
                            .dest = storeDest,
                        } };
                        cmpInstr.* = assembly.Instruction{ .Cmp = assembly.Cmp{
                            .op1 = right,
                            .op2 = storeDest,
                        } };
                        const setCC = try allocator.create(assembly.Instruction);
                        setCC.* = assembly.Instruction{ .SetCC = assembly.SetCC{
                            .code = assembly.CondCode.getFromTacOp(binary.op),
                            .dest = storeDest,
                        } };
                        try instructions.append(movLeftToDest);
                        try instructions.append(cmpInstr);
                        try instructions.append(setCC);
                    },
                    .OR => {
                        unreachable;
                    },
                    .AND => {
                        unreachable;
                    },
                }
            },
            .Copy => |cp| {
                const cpInstr = try allocator.create(assembly.Instruction);
                const src = try cp.src.codegen(symbolTable, allocator);
                const dest = try cp.dest.codegen(symbolTable, allocator);
                cpInstr.* = assembly.Instruction{ .Mov = assembly.MovInst{
                    .src = src,
                    .dest = dest,
                } };
                try instructions.append(cpInstr);
            },
            .Jump => |jmp| {
                const jmpInstr = try allocator.create(assembly.Instruction);
                jmpInstr.* = assembly.Instruction{
                    .Jmp = jmp,
                };
                try instructions.append(jmpInstr);
            },
            .JumpIfZero => |jmp| {
                const movToTemp = try allocator.create(assembly.Instruction);
                const checkZero = try allocator.create(assembly.Instruction);
                const val = try jmp.condition.codegen(symbolTable, allocator);
                movToTemp.* = assembly.Instruction{ .Mov = assembly.MovInst{
                    .src = val,
                    .dest = assembly.Operand{ .Reg = assembly.Reg.R10 },
                } };
                checkZero.* = assembly.Instruction{ .Cmp = assembly.Cmp{
                    .op2 = assembly.Operand{ .Reg = assembly.Reg.R10 },
                    .op1 = assembly.Operand{
                        .Imm = 0,
                    },
                } };
                try instructions.append(movToTemp);
                try instructions.append(checkZero);
                const jump = try allocator.create(assembly.Instruction);
                jump.* = assembly.Instruction{ .JmpCC = assembly.JmpCC{
                    .code = assembly.CondCode.E,
                    .label = jmp.target,
                } };
                try instructions.append(jump);
            },
            .JumpIfNotZero => |jmp| {
                const movToTemp = try allocator.create(assembly.Instruction);
                const checkZero = try allocator.create(assembly.Instruction);
                const val = try jmp.condition.codegen(symbolTable, allocator);
                movToTemp.* = assembly.Instruction{ .Mov = assembly.MovInst{
                    .src = val,
                    .dest = assembly.Operand{ .Reg = assembly.Reg.R10 },
                } };
                checkZero.* = assembly.Instruction{ .Cmp = assembly.Cmp{
                    .op2 = assembly.Operand{ .Reg = assembly.Reg.R10 },
                    .op1 = assembly.Operand{
                        .Imm = 0,
                    },
                } };
                try instructions.append(movToTemp);
                try instructions.append(checkZero);
                const jump = try allocator.create(assembly.Instruction);
                jump.* = assembly.Instruction{ .JmpCC = assembly.JmpCC{
                    .code = assembly.CondCode.NE,
                    .label = jmp.target,
                } };
                try instructions.append(jump);
            },
            .Label => |labelName| {
                const label = try allocator.create(assembly.Instruction);
                label.* = assembly.Instruction{
                    .Label = labelName,
                };
                try instructions.append(label);
            },
            .FunctionCall => |fnCall| {
                const registers = [_]assembly.Reg{ assembly.Reg.EDI, assembly.Reg.ESI, assembly.Reg.EDX, assembly.Reg.ECX, assembly.Reg.R8, assembly.Reg.R9 };
                if (fnCall.args.items.len < 6) {
                    for (fnCall.args.items, 0..) |arg, i| {
                        const movArgToReg = try allocator.create(assembly.Instruction);
                        const assemblyArg = try arg.codegen(symbolTable, allocator);
                        movArgToReg.* = assembly.Instruction{
                            .Mov = assembly.MovInst{
                                .src = assemblyArg,
                                .dest = assembly.Operand{ .Reg = registers[i] },
                            },
                        };
                        try instructions.append(movArgToReg);
                    }
                    const call = try allocator.create(assembly.Instruction);
                    call.* = assembly.Instruction{
                        .FnCall = .{
                            .name = fnCall.name,
                        },
                    };
                    try instructions.append(call);
                    const movReturnToReg = try allocator.create(assembly.Instruction);
                    const asmDest = try fnCall.dest.codegen(symbolTable, allocator);
                    movReturnToReg.* = assembly.Instruction{
                        .Mov = assembly.MovInst{
                            .dest = asmDest,
                            .src = assembly.Operand{ .Reg = assembly.Reg.EAX },
                        },
                    };
                    try instructions.append(movReturnToReg);
                } else {
                    // TODO: People should be taxed for using more than six
                    // arguments
                    unreachable();
                }
            },
        }
    }
};

pub const ValType = enum {
    Constant,
    Variable,
};

pub const Val = union(ValType) {
    Constant: u32,
    Variable: []u8,
    pub fn codegen(val: *Val, symbolTable: std.StringHashMap(*semantic.Symbol), allocator: std.mem.Allocator) ast.CodegenError!assembly.Operand {
        switch (val.*) {
            .Constant => |constant| {
                const operand = try allocator.create(assembly.Operand);
                operand.* = assembly.Operand{
                    .Imm = constant,
                };
                return operand.*;
            },
            .Variable => |variable| {
                const operand = try allocator.create(assembly.Operand);
                if (symbolTable.get(variable)) |sym| {
                    switch (sym.attributes) {
                        .StaticAttr => {
                            operand.* = assembly.Operand{
                                .Data = variable,
                            };
                        },
                        else => {
                            operand.* = assembly.Operand{
                                .Pseudo = variable,
                            };
                        },
                    }
                    return operand.*;
                }
                operand.* = assembly.Operand{
                    .Pseudo = variable,
                };
                return operand.*;
            },
        }
    }
};

test "testing assembly generation - unary" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr = "int main(){ return ~(-2); }";
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    var program = try p.parseProgram();
    const varResolver = try ast.VarResolver.init(allocator);
    try varResolver.resolve(program);
    try ast.loopLabelPass(program, allocator);
    const asmProgram = try (try program.genTAC(allocator)).codegen(allocator);
    try asmProgram.stringify(std.io.getStdErr().writer(), allocator);
}

test "testing assembly generation - binary" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr = "int main(){ return (2*3)%5+6; }";
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    var program = try p.parseProgram();
    const varResolver = try ast.VarResolver.init(allocator);
    try varResolver.resolve(program);
    try ast.loopLabelPass(program, allocator);
    const asmProgram = try (try program.genTAC(allocator)).codegen(allocator);
    try asmProgram.stringify(std.io.getStdErr().writer(), allocator);
}

test "testing assembly generation - >= and <=" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr = "int main(){ return 2 >= 3 + 1 <= 5; }";
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const program = try p.parseProgram();

    const varResolver = try ast.VarResolver.init(allocator);
    try varResolver.resolve(program);
    try ast.loopLabelPass(program, allocator);
    const asmProgram = try (try program.genTAC(allocator)).codegen(allocator);
    try asmProgram.stringify(std.io.getStdErr().writer(), allocator);
}

test "testing assembly generation - short circuiting with logical AND and OR" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr = "int main(){ return 2 && ( 3 || 4 ) ; }";
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const program = try p.parseProgram();

    const varResolver = try ast.VarResolver.init(allocator);
    try varResolver.resolve(program);
    try ast.loopLabelPass(program, allocator);
    const asmProgram = try (try program.genTAC(allocator)).codegen(allocator);
    try asmProgram.stringify(std.io.getStdErr().writer(), allocator);
}

test "testing assembly generation - declarations" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr = "int main(){ int x = 2; int y = 3 || 4; return x && y; }";
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    var program = try p.parseProgram();
    const varResolver = try ast.VarResolver.init(allocator);
    try varResolver.resolve(program);
    try ast.loopLabelPass(program, allocator);
    const asmProgram = try (try program.genTAC(allocator)).codegen(allocator);
    try asmProgram.stringify(std.io.getStdErr().writer(), allocator);
}

test "tac generation - if" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr = "int main(){int y; int x = y = 3;if(x == y) return x; else return y;}";
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const program = try p.parseProgram();
    const varResolver = try ast.VarResolver.init(allocator);
    try varResolver.resolve(program);
    try ast.loopLabelPass(program, allocator);
    const asmProgram = try (try program.genTAC(allocator)).codegen(allocator);
    try asmProgram.stringify(std.io.getStdErr().writer(), allocator);
}

test "tac generation - if nested" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr = "int main(){int y; int x = y = 3;if(x == y) if(x > 3) return x;else; else return y; return 1;}";
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const program = try p.parseProgram();
    const varResolver = try ast.VarResolver.init(allocator);
    try varResolver.resolve(program);
    try ast.loopLabelPass(program, allocator);
    const asmProgram = try (try program.genTAC(allocator)).codegen(allocator);
    try asmProgram.stringify(std.io.getStdErr().writer(), allocator);
}

test "assembly generation with ternary" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr = "int main(){int y = 4; int x = 3; return x == 3?x:y}";
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const program = try p.parseProgram();
    const varResolver = try ast.VarResolver.init(allocator);
    try varResolver.resolve(program);
    try ast.loopLabelPass(program, allocator);
    const asmProgram = try (try program.genTAC(allocator)).codegen(allocator);
    try asmProgram.stringify(std.io.getStdErr().writer(), allocator);
}

test "assembly generation with nested ternary" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr = "int main(){int y = 4; int x = 3; return x == 3?y == 4?x+y:x-y:0;}";
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const program = try p.parseProgram();
    const varResolver = try ast.VarResolver.init(allocator);
    try varResolver.resolve(program);
    try ast.loopLabelPass(program, allocator);
    const asmProgram = try (try program.genTAC(allocator)).codegen(allocator);
    try asmProgram.stringify(std.io.getStdErr().writer(), allocator);
}

test "assembly generation with labelled statements and goto" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr = "int main(){int y = 4; int x = 3; y = 69;goto sup; supTwo:return x+y;sup:return x-y;}";
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const program = try p.parseProgram();
    const varResolver = try ast.VarResolver.init(allocator);
    try varResolver.resolve(program);
    try ast.loopLabelPass(program, allocator);
    const asmProgram = try (try program.genTAC(allocator)).codegen(allocator);
    try asmProgram.stringify(std.io.getStdErr().writer(), allocator);
}

test "testing assembly generation with compound statement parsing" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr = "int main(){int y = 4; int x = 2; {int x = 3;} return x+y;}";
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const program = try p.parseProgram();
    const varResolver = try ast.VarResolver.init(allocator);
    try varResolver.resolve(program);
    const asmProgram = try (try program.genTAC(allocator)).codegen(allocator);
    try asmProgram.stringify(std.io.getStdErr().writer(), allocator);
}

test "testing assembly generation with do and while loop" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr =
        \\ int main(){
        \\     int x = 0;
        \\     while(x < 5) x = x + 1;
        \\     do x = x + 1; while(x < 10);
        \\     return x;
        \\ }
    ;
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const program = try p.parseProgram();
    const varResolver = try ast.VarResolver.init(allocator);
    try varResolver.resolve(program);
    try ast.loopLabelPass(program, allocator);
    const asmProgram = try (try program.genTAC(allocator)).codegen(allocator);
    try asmProgram.stringify(std.io.getStdErr().writer(), allocator);
}

test "testing assembly generation loop with breaks and continue" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr =
        \\ int main(){
        \\     int x = 0;
        \\     while(x < 5){
        \\      if(x == 3) break;
        \\      x = x + 1;
        \\     }
        \\     return x;
        \\ }
    ;
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const program = try p.parseProgram();
    const varResolver = try ast.VarResolver.init(allocator);
    try varResolver.resolve(program);
    try ast.loopLabelPass(program, allocator);
    const asmProgram = try (try program.genTAC(allocator)).codegen(allocator);
    try asmProgram.stringify(std.io.getStdErr().writer(), allocator);
}

test "nested while and do while loops with continue" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr =
        \\ int main() {
        \\   int num = 1;
        \\   int count = 0;
        \\   while (num <= 10) {
        \\     if (num % 2 == 0) {
        \\       num = num + 1;
        \\       count = count + 1;
        \\       continue;
        \\     }
        \\     num = num + 1;
        \\   }
        \\   return count;
        \\ }
    ;
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const program = try p.parseProgram();
    const varResolver = try ast.VarResolver.init(allocator);
    try varResolver.resolve(program);
    try ast.loopLabelPass(program, allocator);
    const asmProgram = try (try program.genTAC(allocator)).codegen(allocator);
    try asmProgram.stringify(std.io.getStdErr().writer(), allocator);
}

test "test assembly generation for for loops" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr =
        \\ int main(){
        \\     int y = 0;
        \\     for(int x = 0; x < 5; x = x + 1){ y = y + 1;}
        \\     return y;
        \\ }
    ;
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const program = try p.parseProgram();
    const varResolver = try ast.VarResolver.init(allocator);
    try varResolver.resolve(program);
    const typechecker = try semantic.Typechecker.init(allocator);
    const hasTypeErr = try typechecker.check(program);
    if (hasTypeErr) |typeError| {
        std.log.warn("\x1b[33mError\x1b[0m: {s}\n", .{typeError});
        std.debug.assert(false);
    }
    try ast.loopLabelPass(program, allocator);
    const asmProgram = try (try program.genTAC(typechecker.symbolTable, allocator)).codegen(allocator);
    const sFile = try std.fs.cwd().createFile("./cFiles/sup2.s", .{});
    try asmProgram.stringify(std.io.getStdErr().writer(), allocator);
    try asmProgram.stringify(sFile.writer(), allocator);
}
//
test "multiple functions and call" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr =
        \\ int add(int x, int y, int z) { return x+y+z;}
        \\ int main(){
        \\     return add(2,3,5);
        \\ }
    ;
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const program = try p.parseProgram();
    const varResolver = try ast.VarResolver.init(allocator);
    try varResolver.resolve(program);
    const typechecker = try semantic.Typechecker.init(allocator);
    const hasTypeErr = try typechecker.check(program);
    if (hasTypeErr) |typeError| {
        std.log.warn("\x1b[33mError\x1b[0m: {s}\n", .{typeError});
        std.debug.assert(false);
    }
    try ast.loopLabelPass(program, allocator);
    const asmProgram = try (try program.genTAC(typechecker.symbolTable, allocator)).codegen(allocator);
    const cfile = try std.fs.cwd().createFile("./cFiles/sup.s", .{});
    try asmProgram.stringify(std.io.getStdErr().writer(), allocator);
    try asmProgram.stringify(cfile.writer(), allocator);
}

test "global variable codegeneration" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr =
        \\ int four = 4; 
        \\ int main(){
        \\     return four; 
        \\ }
    ;
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const program = try p.parseProgram();
    const varResolver = try ast.VarResolver.init(allocator);
    try varResolver.resolve(program);
    const typechecker = try semantic.Typechecker.init(allocator);
    const hasTypeErr = try typechecker.check(program);
    if (hasTypeErr) |typeError| {
        std.log.warn("\x1b[33mError\x1b[0m: {s}\n", .{typeError});
        std.debug.assert(false);
    }
    try ast.loopLabelPass(program, allocator);
    const asmProgram = try (try program.genTAC(typechecker.symbolTable, allocator)).codegen(allocator);
    const cfile = try std.fs.cwd().createFile("./cFiles/global1.s", .{});
    try asmProgram.stringify(std.io.getStdErr().writer(), allocator);
    try asmProgram.stringify(cfile.writer(), allocator);
}

test "global variable codegenaration with multiple funcs" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr =
        \\ int four = 4; 
        \\ int add(int x, int y){ return x+y; }
        \\ int main(){
        \\     return add(four, 5); 
        \\ }
    ;
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const program = try p.parseProgram();
    const varResolver = try ast.VarResolver.init(allocator);
    try varResolver.resolve(program);
    const typechecker = try semantic.Typechecker.init(allocator);
    const hasTypeErr = try typechecker.check(program);
    if (hasTypeErr) |typeError| {
        std.log.warn("\x1b[33mError\x1b[0m: {s}\n", .{typeError});
        std.debug.assert(false);
    }
    try ast.loopLabelPass(program, allocator);
    const asmProgram = try (try program.genTAC(typechecker.symbolTable, allocator)).codegen(allocator);
    const cfile = try std.fs.cwd().createFile("./cFiles/global1.s", .{});
    try asmProgram.stringify(std.io.getStdErr().writer(), allocator);
    try asmProgram.stringify(cfile.writer(), allocator);
}
