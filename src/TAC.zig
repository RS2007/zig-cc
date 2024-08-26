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

pub const Instruction = union(InstructionType) {
    Return: Return,
    Unary: Unary,
    Binary: Binary,
    pub fn codegen(instruction: *Instruction, instructions: *std.ArrayList(*assembly.Instruction), allocator: std.mem.Allocator) ast.CodegenError!void {
        switch (instruction.*) {
            .Return => |ret| {
                var val = try ret.val.codegen(allocator);
                var movInst = try allocator.create(assembly.Instruction);
                movInst.* = assembly.Instruction{ .Mov = assembly.MovInst{
                    .src = val,
                    .dest = assembly.Operand{ .Reg = assembly.Reg.AX },
                } };
                var retInst = try allocator.create(assembly.Instruction);
                retInst.* = assembly.Instruction{ .Ret = {} };
                try instructions.append(movInst);
                try instructions.append(retInst);
            },
            .Unary => |unary| {
                var dest = try unary.dest.codegen(allocator);
                var src = try unary.src.codegen(allocator);
                var movInst = try allocator.create(assembly.Instruction);
                var unaryInst = try allocator.create(assembly.Instruction);
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
                var storeDest = try binary.dest.codegen(allocator);
                var left = try binary.left.codegen(allocator);
                var right = try binary.right.codegen(allocator);
                switch (binary.op) {
                    .ADD, .SUBTRACT => {
                        var movLeftToDest = try allocator.create(assembly.Instruction);
                        movLeftToDest.* = assembly.Instruction{ .Mov = assembly.MovInst{
                            .src = left,
                            .dest = storeDest,
                        } };
                        var binaryInstr = try allocator.create(assembly.Instruction);
                        binaryInstr.* = assembly.Instruction{ .Binary = assembly.BinaryInst{
                            .lhs = storeDest,
                            .rhs = right,
                            .op = tacOpToAssemblyOp(binary.op),
                        } };
                        try instructions.append(movLeftToDest);
                        try instructions.append(binaryInstr);
                    },
                    .MULTIPLY => {
                        var movLeftToR11 = try allocator.create(assembly.Instruction);
                        var binaryInstr = try allocator.create(assembly.Instruction);
                        var movR11ToDest = try allocator.create(assembly.Instruction);
                        movLeftToR11.* = assembly.Instruction{
                            .Mov = assembly.MovInst{
                                .src = left,
                                .dest = assembly.Operand{ .Reg = assembly.Reg.R11 },
                            },
                        };
                        binaryInstr.* = assembly.Instruction{ .Binary = assembly.BinaryInst{
                            .op = assembly.BinaryOp.Multiply,
                            .lhs = assembly.Operand{ .Reg = assembly.Reg.R11 },
                            .rhs = right,
                        } };
                        movR11ToDest.* = assembly.Instruction{ .Mov = assembly.MovInst{
                            .src = assembly.Operand{ .Reg = assembly.Reg.R11 },
                            .dest = storeDest,
                        } };
                        try instructions.append(movLeftToR11);
                        try instructions.append(binaryInstr);
                        try instructions.append(movR11ToDest);
                    },
                    .DIVIDE => {
                        var movLeftToAX = try allocator.create(assembly.Instruction);
                        var cdqInstr = try allocator.create(assembly.Instruction);
                        var idivWithRight = try allocator.create(assembly.Instruction);
                        var movAXToDest = try allocator.create(assembly.Instruction);
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
                        var movLeftToDX = try allocator.create(assembly.Instruction);
                        var cdqInstr = try allocator.create(assembly.Instruction);
                        var idivWithRight = try allocator.create(assembly.Instruction);
                        var movDXToDest = try allocator.create(assembly.Instruction);
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
    pub fn codegen(val: *Val, allocator: std.mem.Allocator) ast.CodegenError!assembly.Operand {
        switch (val.*) {
            .Constant => |constant| {
                var operand = try allocator.create(assembly.Operand);
                operand.* = assembly.Operand{
                    .Imm = constant,
                };
                return operand.*;
            },
            .Variable => |variable| {
                var operand = try allocator.create(assembly.Operand);
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
    var l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    var program = try p.parseProgram();
    var instructions = try program.genTAC(allocator);
    var asmInstructions = std.ArrayList(*assembly.Instruction).init(allocator);
    for (instructions.items) |inst| {
        try inst.codegen(&asmInstructions, allocator);
    }
    for (asmInstructions.items) |asmInst| {
        std.log.warn("\n \x1b[34m{any}\x1b[0m", .{asmInst});
    }
    try assembly.replacePseudoRegs(&asmInstructions, allocator);
    try assembly.replaceStackToStackMov(&asmInstructions, allocator);
    std.log.warn("POST PSEUDO REPLACEMENT AND STACK TO STACK MOVES", .{});
    // for (asmInstructions.items) |asmInst| {
    //     std.log.warn("\n \x1b[34m{any}\x1b[0m", .{asmInst});
    // }
    var mem: [2048]u8 = std.mem.zeroes([2048]u8);
    var buf = @as([]u8, &mem);
    var header = try std.fmt.bufPrint(buf, ".globl main\nmain:\npush %rbp", .{});
    buf = buf[header.len..];
    for (asmInstructions.items) |asmInst| {
        var printedSlice = try std.fmt.bufPrint(buf, "\n{s}", .{try asmInst.stringify(allocator)});
        buf = buf[printedSlice.len..];
    }

    std.log.warn("\n\x1b[33m{s}\x1b[0m", .{mem});
}

test "testing assembly generation - binary" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr = "int main(){ return (2*3)%5+6; }";
    var l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    var program = try p.parseProgram();
    var instructions = try program.genTAC(allocator);
    var asmInstructions = std.ArrayList(*assembly.Instruction).init(allocator);
    for (instructions.items) |inst| {
        try inst.codegen(&asmInstructions, allocator);
    }
    for (asmInstructions.items) |asmInst| {
        std.log.warn("\n \x1b[34m{any}\x1b[0m", .{asmInst});
    }
    try assembly.replacePseudoRegs(&asmInstructions, allocator);
    try assembly.replaceStackToStackMov(&asmInstructions, allocator);
    std.log.warn("POST PSEUDO REPLACEMENT AND STACK TO STACK MOVES", .{});
    // for (asmInstructions.items) |asmInst| {
    //     std.log.warn("\n \x1b[34m{any}\x1b[0m", .{asmInst});
    // }
    var mem: [2048]u8 = std.mem.zeroes([2048]u8);
    var buf = @as([]u8, &mem);
    var header = try std.fmt.bufPrint(buf, ".globl main\nmain:\npush %rbp", .{});
    buf = buf[header.len..];
    for (asmInstructions.items) |asmInst| {
        var printedSlice = try std.fmt.bufPrint(buf, "\n{s}", .{try asmInst.stringify(allocator)});
        buf = buf[printedSlice.len..];
    }

    std.log.warn("\n\x1b[33m{s}\x1b[0m", .{mem});
}
