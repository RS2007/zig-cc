const std = @import("std");
const ast = @import("./AST.zig");

fn abs(src: i32) u32 {
    if (src < 0) {
        return @intCast(-src);
    }
    return @intCast(src);
}
pub const Reg = enum {
    AX,
    R10,
    DX,
    R11,
    pub fn stringify(register: Reg, allocator: std.mem.Allocator) ast.CodegenError![]u8 {
        switch (register) {
            .AX => {
                return (try std.fmt.allocPrint(allocator, "%eax", .{}));
            },
            .R10 => {
                return (try std.fmt.allocPrint(allocator, "%r10d", .{}));
            },
            .DX => {
                return (try std.fmt.allocPrint(allocator, "%edx", .{}));
            },
            .R11 => {
                return (try std.fmt.allocPrint(allocator, "%r11d", .{}));
            },
        }
    }
};

pub const OperandType = enum { Imm, Reg, Pseudo, Stack };

pub const Operand = union(OperandType) {
    Imm: u32,
    Reg: Reg,
    Pseudo: []u8,
    Stack: i32,
    pub fn stringify(operand: *Operand, allocator: std.mem.Allocator) ast.CodegenError![]u8 {
        switch (operand.*) {
            .Imm => |imm| {
                return (try std.fmt.allocPrint(allocator, "${d}", .{imm}));
            },
            .Reg => |reg| {
                return reg.stringify(allocator);
            },
            .Stack => |stackOff| {
                return (try std.fmt.allocPrint(allocator, "-0x{x}(%rbp)", .{abs(stackOff)}));
            },
            else => {
                unreachable;
            },
        }
    }
};

pub const UnaryOp = enum {
    Neg,
    Not,
};

pub const BinaryOp = enum {
    Add,
    Subtract,
    Multiply,
    pub fn stringify(self: BinaryOp, allocator: std.mem.Allocator) ast.CodegenError![]u8 {
        return (try std.fmt.allocPrint(allocator, "{s}", .{switch (self) {
            .Add => "addl",
            .Subtract => "subl",
            .Multiply => "imull",
        }}));
    }
};

pub const InstructionType = enum {
    Mov,
    Unary,
    AllocateStack,
    Ret,
    Binary,
    Idiv,
    Cdq,
};

pub const MovInst = struct {
    src: Operand,
    dest: Operand,
};

pub const UnaryInst = struct {
    op: UnaryOp,
    rhs: Operand,
};

pub const BinaryInst = struct {
    op: BinaryOp,
    lhs: Operand,
    rhs: Operand,
};

pub const Instruction = union(InstructionType) {
    Mov: MovInst,
    Unary: UnaryInst,
    AllocateStack: u32,
    Ret: void,
    Binary: BinaryInst,
    Idiv: Operand,
    Cdq: void,

    pub fn stringify(instruction: *Instruction, allocator: std.mem.Allocator) ast.CodegenError![]u8 {
        switch (instruction.*) {
            .Mov => |mov| {
                return (try std.fmt.allocPrint(allocator, "movl {s},{s}", .{ try Operand.stringify(@constCast(&mov.src), allocator), try Operand.stringify(@constCast(&mov.dest), allocator) }));
            },
            .Unary => |unary| {
                switch (unary.op) {
                    .Neg => {
                        return (try std.fmt.allocPrint(allocator, "negl {s}", .{try @constCast(&unary.rhs).stringify(allocator)}));
                    },
                    .Not => {
                        return (try std.fmt.allocPrint(allocator, "notl {s}", .{try @constCast(&unary.rhs).stringify(allocator)}));
                    },
                }
            },
            .AllocateStack => |allocStack| {
                return (try std.fmt.allocPrint(allocator, "mov %rsp,%rbp\nsub $0x{x},%rsp", .{allocStack}));
            },
            .Ret => {
                return (try std.fmt.allocPrint(allocator, "movl %eax,%edi\nmovl $0x3c,%eax\nsyscall\nleaveq\nretq", .{}));
            },
            .Binary => |binary| {
                var lhsOperand = try @constCast(&binary.lhs).stringify(allocator);
                var rhsOperand = try @constCast(&binary.rhs).stringify(allocator);
                var instrString = try binary.op.stringify(allocator);
                return (try std.fmt.allocPrint(allocator, "{s} {s},{s}", .{ instrString, rhsOperand, lhsOperand }));
            },
            .Cdq => {
                return (try std.fmt.allocPrint(allocator, "cdq", .{}));
            },
            .Idiv => |operand| {
                var operandStringified = try @constCast(&operand).stringify(allocator);
                return (try std.fmt.allocPrint(allocator, "idiv {s}", .{operandStringified}));
            },
        }
    }
};

pub fn replaceStackToStackMov(instructions: *std.ArrayList(*Instruction), allocator: std.mem.Allocator) ast.CodegenError!void {
    for (instructions.items, 0..) |inst, i| {
        switch (inst.*) {
            .Mov => |mov| {
                var isSrcStack = false;
                var isDestStack = false;
                switch (mov.src) {
                    .Stack => {
                        isSrcStack = true;
                    },
                    else => {},
                }
                switch (mov.dest) {
                    .Stack => {
                        isDestStack = true;
                    },
                    else => {},
                }
                if (isSrcStack and isDestStack) {
                    _ = instructions.orderedRemove(i);
                    var movInstSrc = try allocator.create(Instruction);
                    var movInstDest = try allocator.create(Instruction);
                    movInstSrc.* = Instruction{ .Mov = MovInst{ .src = mov.src, .dest = Operand{ .Reg = Reg.R10 } } };
                    movInstDest.* = Instruction{ .Mov = MovInst{
                        .src = Operand{ .Reg = Reg.R10 },
                        .dest = mov.dest,
                    } };
                    try instructions.insert(
                        i,
                        movInstSrc,
                    );
                    try instructions.insert(
                        i + 1,
                        movInstDest,
                    );
                }
            },
            else => {},
        }
    }
}

pub fn replacePseudoRegs(instructions: *std.ArrayList(*Instruction), allocator: std.mem.Allocator) ast.CodegenError!void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const hashAllocator = arena.allocator();
    defer arena.deinit();
    var lookup = std.StringHashMap(i32).init(hashAllocator);
    var topOfStack: i32 = 0;
    for (instructions.items, 0..) |inst, i| {
        switch (inst.*) {
            .Mov => |mov| {
                switch (mov.src) {
                    .Pseudo => |pseudo| {
                        if (lookup.contains(pseudo)) {
                            inst.Mov.src = Operand{ .Stack = lookup.get(pseudo).? };
                        } else {
                            topOfStack -= 4;
                            try lookup.put(
                                pseudo,
                                topOfStack,
                            );
                            inst.Mov.src = Operand{ .Stack = topOfStack };
                        }
                    },
                    else => {},
                }
                switch (mov.dest) {
                    .Pseudo => |pseudo| {
                        if (lookup.contains(pseudo)) {
                            inst.Mov.dest = Operand{ .Stack = lookup.get(pseudo).? };
                        } else {
                            topOfStack -= 4;
                            try lookup.put(
                                pseudo,
                                topOfStack,
                            );
                            inst.Mov.dest = Operand{ .Stack = topOfStack };
                        }
                    },
                    else => {},
                }
            },
            .Unary => |unary| {
                switch (unary.rhs) {
                    .Pseudo => |pseudo| {
                        if (lookup.contains(pseudo)) {
                            inst.Unary.rhs = Operand{ .Stack = lookup.get(pseudo).? };
                            continue;
                        } else {
                            topOfStack -= 4;
                            try lookup.put(
                                pseudo,
                                topOfStack,
                            );
                            inst.Unary.rhs = Operand{ .Stack = topOfStack };
                        }
                    },
                    else => {},
                }
            },
            .Binary => |binary| {
                switch (binary.lhs) {
                    .Pseudo => |pseudo| {
                        if (lookup.contains(pseudo)) {
                            inst.Binary.lhs = Operand{ .Stack = lookup.get(pseudo).? };
                            continue;
                        } else {
                            topOfStack -= 4;
                            try lookup.put(
                                pseudo,
                                topOfStack,
                            );
                            inst.Binary.lhs = Operand{ .Stack = topOfStack };
                        }
                    },
                    else => {},
                }
                switch (binary.rhs) {
                    .Pseudo => |pseudo| {
                        if (lookup.contains(pseudo)) {
                            inst.Binary.rhs = Operand{ .Stack = lookup.get(pseudo).? };
                            continue;
                        } else {
                            topOfStack -= 4;
                            try lookup.put(
                                pseudo,
                                topOfStack,
                            );
                            inst.Binary.rhs = Operand{ .Stack = topOfStack };
                        }
                    },
                    else => {},
                }
            },
            .Idiv => |idiv| {
                switch (idiv) {
                    .Reg => |reg| {
                        if (reg != Reg.R10) {
                            var movIdivArgToR10 = try allocator.create(Instruction);
                            movIdivArgToR10.* = Instruction{ .Mov = MovInst{
                                .src = idiv,
                                .dest = Operand{ .Reg = Reg.R10 },
                            } };
                            try instructions.insert(
                                i,
                                movIdivArgToR10,
                            );
                            inst.Idiv = Operand{ .Reg = Reg.R10 };
                        }
                    },
                    .Pseudo => |pseudo| {
                        var movIdivArgToR10 = try allocator.create(Instruction);
                        movIdivArgToR10.* = Instruction{ .Mov = MovInst{
                            .src = Operand{ .Pseudo = pseudo },
                            .dest = Operand{ .Reg = Reg.R10 },
                        } };
                        try instructions.insert(
                            i,
                            movIdivArgToR10,
                        );
                        inst.Idiv = Operand{ .Reg = Reg.R10 };
                    },
                    .Imm => |imm| {
                        var movIdivArgToR10 = try allocator.create(Instruction);
                        movIdivArgToR10.* = Instruction{ .Mov = MovInst{
                            .src = Operand{ .Imm = imm },
                            .dest = Operand{ .Reg = Reg.R10 },
                        } };
                        try instructions.insert(
                            i,
                            movIdivArgToR10,
                        );
                        inst.Idiv = Operand{ .Reg = Reg.R10 };
                    },
                    .Stack => {
                        unreachable;
                    },
                }
            },
            .Cdq, .AllocateStack, .Ret => {},
        }
    }
    var allocateStackInst = try allocator.create(Instruction);
    allocateStackInst.* = Instruction{
        .AllocateStack = @as(u32, abs(topOfStack)),
    };
    try instructions.insert(
        0,
        allocateStackInst,
    );
}
