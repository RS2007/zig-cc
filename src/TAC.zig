const std = @import("std");
const ast = @import("./AST.zig");
const assembly = @import("./Assembly.zig");
const lexer = @import("./lexer.zig");
const parser = @import("./parser.zig");

pub const Program = struct {
    function: FunctionDef,
};

pub const FunctionDef = struct {
    name: []u8,
    instructions: std.ArrayList(*Instruction),
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

pub const Instruction = union(InstructionType) {
    Return: Return,
    Unary: Unary,
    Binary: Binary,
    Copy: Copy,
    Jump: []u8,
    JumpIfZero: Jmp,
    JumpIfNotZero: Jmp,
    Label: []u8,
    pub fn codegen(instruction: *Instruction, instructions: *std.ArrayList(*assembly.Instruction), allocator: std.mem.Allocator) ast.CodegenError!void {
        switch (instruction.*) {
            .Return => |ret| {
                const val = try ret.val.codegen(allocator);
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
                const dest = try unary.dest.codegen(allocator);
                const src = try unary.src.codegen(allocator);
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
                const storeDest = try binary.dest.codegen(allocator);
                const left = try binary.left.codegen(allocator);
                const right = try binary.right.codegen(allocator);
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
                const src = try cp.src.codegen(allocator);
                const dest = try cp.dest.codegen(allocator);
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
                const val = try jmp.condition.codegen(allocator);
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
                const val = try jmp.condition.codegen(allocator);
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
    pub fn codegen(val: *Val, allocator: std.mem.Allocator) ast.CodegenError!assembly.Operand {
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
    const instructions = try program.genTAC(allocator);
    var asmInstructions = std.ArrayList(*assembly.Instruction).init(allocator);
    for (instructions.items) |inst| {
        try inst.codegen(&asmInstructions, allocator);
    }
    try assembly.replacePseudoRegs(&asmInstructions, allocator);
    const fixedAsmInstructions = try assembly.fixupInstructions(&asmInstructions, allocator);
    std.log.warn("POST PSEUDO REPLACEMENT AND STACK TO STACK MOVES", .{});
    // for (asmInstructions.items) |asmInst| {
    //     std.log.warn("\n \x1b[34m{any}\x1b[0m", .{asmInst});
    // }
    var mem: [2048]u8 = std.mem.zeroes([2048]u8);
    var buf = @as([]u8, &mem);
    const header = try std.fmt.bufPrint(buf, ".globl main\nmain:\npush %rbp", .{});
    buf = buf[header.len..];
    for (fixedAsmInstructions.items) |asmInst| {
        const printedSlice = try std.fmt.bufPrint(buf, "\n{s}", .{try asmInst.stringify(allocator)});
        buf = buf[printedSlice.len..];
    }

    std.log.warn("\n\x1b[33m{s}\x1b[0m", .{mem});
}

test "testing assembly generation - binary" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr = "int main(){ return (2*3)%5+6; }";
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    var program = try p.parseProgram();
    const instructions = try program.genTAC(allocator);
    var asmInstructions = std.ArrayList(*assembly.Instruction).init(allocator);
    for (instructions.items) |inst| {
        try inst.codegen(&asmInstructions, allocator);
    }
    try assembly.replacePseudoRegs(&asmInstructions, allocator);
    const fixedAsmInstructions = try assembly.fixupInstructions(&asmInstructions, allocator);
    std.log.warn("POST PSEUDO REPLACEMENT AND STACK TO STACK MOVES", .{});
    // for (asmInstructions.items) |asmInst| {
    //     std.log.warn("\n \x1b[34m{any}\x1b[0m", .{asmInst});
    // }
    var mem: [2048]u8 = std.mem.zeroes([2048]u8);
    var buf = @as([]u8, &mem);
    const header = try std.fmt.bufPrint(buf, ".globl main\nmain:\npush %rbp", .{});
    buf = buf[header.len..];
    for (fixedAsmInstructions.items) |asmInst| {
        const printedSlice = try std.fmt.bufPrint(buf, "\n{s}", .{try asmInst.stringify(allocator)});
        buf = buf[printedSlice.len..];
    }

    std.log.warn("\n\x1b[33m{s}\x1b[0m", .{mem});
}

test "testing assembly generation - >= and <=" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr = "int main(){ return 2 >= 3 + 1 <= 5; }";
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const program = try p.parseProgram();

    const instructions = try program.genTAC(allocator);
    var asmInstructions = std.ArrayList(*assembly.Instruction).init(allocator);
    for (instructions.items) |inst| {
        try inst.codegen(&asmInstructions, allocator);
    }
    try assembly.replacePseudoRegs(&asmInstructions, allocator);
    const fixedAsmInstructions = try assembly.fixupInstructions(&asmInstructions, allocator);
    std.log.warn("POST PSEUDO REPLACEMENT AND STACK TO STACK MOVES", .{});
    // for (asmInstructions.items) |asmInst| {
    //     std.log.warn("\n \x1b[34m{any}\x1b[0m", .{asmInst});
    // }
    var mem: [2048]u8 = std.mem.zeroes([2048]u8);
    var buf = @as([]u8, &mem);
    const header = try std.fmt.bufPrint(buf, ".globl main\nmain:\npush %rbp", .{});
    buf = buf[header.len..];
    for (fixedAsmInstructions.items) |asmInst| {
        const printedSlice = try std.fmt.bufPrint(buf, "\n{s}", .{try asmInst.stringify(allocator)});
        buf = buf[printedSlice.len..];
    }

    std.log.warn("\n\x1b[33m{s}\x1b[0m", .{mem});
}

test "testing assembly generation - short circuiting with logical AND and OR" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr = "int main(){ return 2 && ( 3 || 4 ) ; }";
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const program = try p.parseProgram();

    const instructions = try program.genTAC(allocator);
    var asmInstructions = std.ArrayList(*assembly.Instruction).init(allocator);
    for (instructions.items) |inst| {
        try inst.codegen(&asmInstructions, allocator);
    }
    try assembly.replacePseudoRegs(&asmInstructions, allocator);
    const fixedAsmInstructions = try assembly.fixupInstructions(&asmInstructions, allocator);
    std.log.warn("POST PSEUDO REPLACEMENT AND STACK TO STACK MOVES", .{});
    // for (asmInstructions.items) |asmInst| {
    //     std.log.warn("\n \x1b[34m{any}\x1b[0m", .{asmInst});
    // }
    var mem: [2048]u8 = std.mem.zeroes([2048]u8);
    var buf = @as([]u8, &mem);
    const header = try std.fmt.bufPrint(buf, ".globl main\nmain:\npush %rbp", .{});
    buf = buf[header.len..];
    for (fixedAsmInstructions.items) |asmInst| {
        const printedSlice = try std.fmt.bufPrint(buf, "\n{s}", .{try asmInst.stringify(allocator)});
        buf = buf[printedSlice.len..];
    }

    std.log.warn("\n\x1b[33m{s}\x1b[0m", .{mem});
}

test "testing assembly generation - declarations" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr = "int main(){ int x = 2; int y = 3 || 4; return x && y; }";
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    var program = try p.parseProgram();
    const instructions = try program.genTAC(allocator);
    var asmInstructions = std.ArrayList(*assembly.Instruction).init(allocator);
    for (instructions.items) |inst| {
        try inst.codegen(&asmInstructions, allocator);
    }
    try assembly.replacePseudoRegs(&asmInstructions, allocator);
    const fixedAsmInstructions = try assembly.fixupInstructions(&asmInstructions, allocator);
    std.log.warn("POST PSEUDO REPLACEMENT AND STACK TO STACK MOVES", .{});
    // for (asmInstructions.items) |asmInst| {
    //     std.log.warn("\n \x1b[34m{any}\x1b[0m", .{asmInst});
    // }
    var mem: [2048]u8 = std.mem.zeroes([2048]u8);
    var buf = @as([]u8, &mem);
    const header = try std.fmt.bufPrint(buf, ".globl main\nmain:\npush %rbp", .{});
    buf = buf[header.len..];
    for (fixedAsmInstructions.items) |asmInst| {
        const printedSlice = try std.fmt.bufPrint(buf, "\n{s}", .{try asmInst.stringify(allocator)});
        buf = buf[printedSlice.len..];
    }

    std.log.warn("\n\x1b[33m{s}\x1b[0m", .{mem});
}

test "tac generation - if" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr = "int main(){int y; int x = y = 3;if(x == y) return x; else return y;}";
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const program = try p.parseProgram();
    const instructions = try program.genTAC(allocator);
    var asmInstructions = std.ArrayList(*assembly.Instruction).init(allocator);
    std.log.warn("second declaration: {any}\n", .{program.function.blockItems.items[1].Declaration});
    std.log.warn("second declaration more info: {any} \n", .{program.function.blockItems.items[1].Declaration.expression.?.Assignment});

    for (instructions.items) |inst| {
        std.log.warn("inst: {any}\n", .{inst});
        try inst.codegen(&asmInstructions, allocator);
    }
    try assembly.replacePseudoRegs(&asmInstructions, allocator);
    const fixedAsmInstructions = try assembly.fixupInstructions(&asmInstructions, allocator);
    std.log.warn("POST PSEUDO REPLACEMENT AND STACK TO STACK MOVES", .{});
    var mem: [2048]u8 = std.mem.zeroes([2048]u8);
    var buf = @as([]u8, &mem);
    const header = try std.fmt.bufPrint(buf, ".globl main\nmain:\npush %rbp", .{});
    buf = buf[header.len..];
    for (fixedAsmInstructions.items) |asmInst| {
        const printedSlice = try std.fmt.bufPrint(buf, "\n{s}", .{try asmInst.stringify(allocator)});
        buf = buf[printedSlice.len..];
    }

    std.log.warn("\n\x1b[33m{s}\x1b[0m", .{mem});
}

test "tac generation - if nested" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr = "int main(){int y; int x = y = 3;if(x == y) if(x > 3) return x;else; else return y; return 1;}";
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const program = try p.parseProgram();
    const instructions = try program.genTAC(allocator);
    var asmInstructions = std.ArrayList(*assembly.Instruction).init(allocator);
    std.log.warn("second declaration: {any}\n", .{program.function.blockItems.items[1].Declaration});
    std.log.warn("second declaration more info: {any} \n", .{program.function.blockItems.items[1].Declaration.expression.?.Assignment});

    for (instructions.items) |inst| {
        std.log.warn("inst: {any}\n", .{inst});
        try inst.codegen(&asmInstructions, allocator);
    }
    try assembly.replacePseudoRegs(&asmInstructions, allocator);
    const fixedAsmInstructions = try assembly.fixupInstructions(&asmInstructions, allocator);
    std.log.warn("POST PSEUDO REPLACEMENT AND STACK TO STACK MOVES", .{});
    var mem: [2048]u8 = std.mem.zeroes([2048]u8);
    var buf = @as([]u8, &mem);
    const header = try std.fmt.bufPrint(buf, ".globl main\nmain:\npush %rbp", .{});
    buf = buf[header.len..];
    for (fixedAsmInstructions.items) |asmInst| {
        const printedSlice = try std.fmt.bufPrint(buf, "\n{s}", .{try asmInst.stringify(allocator)});
        buf = buf[printedSlice.len..];
    }

    std.log.warn("\n\x1b[33m{s}\x1b[0m", .{mem});
}

test "assembly generation with ternary" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr = "int main(){int y = 4; int x = 3; return x == 3?x:y}";
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const program = try p.parseProgram();
    const instructions = try program.genTAC(allocator);
    var asmInstructions = std.ArrayList(*assembly.Instruction).init(allocator);
    for (instructions.items) |inst| {
        try inst.codegen(&asmInstructions, allocator);
    }
    try assembly.replacePseudoRegs(&asmInstructions, allocator);
    const fixedAsmInstructions = try assembly.fixupInstructions(&asmInstructions, allocator);
    std.log.warn("POST PSEUDO REPLACEMENT AND STACK TO STACK MOVES", .{});
    var mem: [2048]u8 = std.mem.zeroes([2048]u8);
    var buf = @as([]u8, &mem);
    const header = try std.fmt.bufPrint(buf, ".globl main\nmain:\npush %rbp", .{});
    buf = buf[header.len..];
    for (fixedAsmInstructions.items) |asmInst| {
        const printedSlice = try std.fmt.bufPrint(buf, "\n{s}", .{try asmInst.stringify(allocator)});
        buf = buf[printedSlice.len..];
    }

    std.log.warn("\n\x1b[33m{s}\x1b[0m", .{mem});
}

test "assembly generation with nested ternary" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr = "int main(){int y = 4; int x = 3; return x == 3?y == 4?x+y:x-y:0;}";
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const program = try p.parseProgram();
    const instructions = try program.genTAC(allocator);
    var asmInstructions = std.ArrayList(*assembly.Instruction).init(allocator);
    for (instructions.items) |inst| {
        try inst.codegen(&asmInstructions, allocator);
    }
    try assembly.replacePseudoRegs(&asmInstructions, allocator);
    const fixedAsmInstructions = try assembly.fixupInstructions(&asmInstructions, allocator);
    std.log.warn("POST PSEUDO REPLACEMENT AND STACK TO STACK MOVES", .{});
    var mem: [2048]u8 = std.mem.zeroes([2048]u8);
    var buf = @as([]u8, &mem);
    const header = try std.fmt.bufPrint(buf, ".globl main\nmain:\npush %rbp", .{});
    buf = buf[header.len..];
    for (fixedAsmInstructions.items) |asmInst| {
        const printedSlice = try std.fmt.bufPrint(buf, "\n{s}", .{try asmInst.stringify(allocator)});
        buf = buf[printedSlice.len..];
    }

    std.log.warn("\n\x1b[33m{s}\x1b[0m", .{mem});
}

test "assembly generation with labelled statements and goto" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr = "int main(){int y = 4; int x = 3; y = 69;goto sup; supTwo:return x+y;sup:return x-y;}";
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const program = try p.parseProgram();
    const instructions = try program.genTAC(allocator);
    var asmInstructions = std.ArrayList(*assembly.Instruction).init(allocator);
    for (instructions.items) |inst| {
        try inst.codegen(&asmInstructions, allocator);
    }
    try assembly.replacePseudoRegs(&asmInstructions, allocator);
    const fixedAsmInstructions = try assembly.fixupInstructions(&asmInstructions, allocator);
    std.log.warn("POST PSEUDO REPLACEMENT AND STACK TO STACK MOVES", .{});
    var mem: [2048]u8 = std.mem.zeroes([2048]u8);
    var buf = @as([]u8, &mem);
    const header = try std.fmt.bufPrint(buf, ".globl main\nmain:\npush %rbp", .{});
    buf = buf[header.len..];
    for (fixedAsmInstructions.items) |asmInst| {
        const printedSlice = try std.fmt.bufPrint(buf, "\n{s}", .{try asmInst.stringify(allocator)});
        buf = buf[printedSlice.len..];
    }

    std.log.warn("\n\x1b[33m{s}\x1b[0m", .{mem});
}

test "testing assembly generation with compound statement parsing" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr = "int main(){int y = 4; int x = 2; {int x = 3;} return x+y;}";
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const program = try p.parseProgram();
    try ast.scopeVariableResolutionPass(program, allocator);
    const instructions = try program.genTAC(allocator);
    var asmInstructions = std.ArrayList(*assembly.Instruction).init(allocator);
    for (instructions.items) |inst| {
        try inst.codegen(&asmInstructions, allocator);
    }
    try assembly.replacePseudoRegs(&asmInstructions, allocator);
    const fixedAsmInstructions = try assembly.fixupInstructions(&asmInstructions, allocator);
    std.log.warn("POST PSEUDO REPLACEMENT AND STACK TO STACK MOVES", .{});
    var mem: [2048]u8 = std.mem.zeroes([2048]u8);
    var buf = @as([]u8, &mem);
    const header = try std.fmt.bufPrint(buf, ".globl main\nmain:\npush %rbp", .{});
    buf = buf[header.len..];
    for (fixedAsmInstructions.items) |asmInst| {
        const printedSlice = try std.fmt.bufPrint(buf, "\n{s}", .{try asmInst.stringify(allocator)});
        buf = buf[printedSlice.len..];
    }

    std.log.warn("\n\x1b[33m{s}\x1b[0m", .{mem});
}
