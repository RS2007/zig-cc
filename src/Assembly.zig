const std = @import("std");
const ast = @import("./AST.zig");
const tac = @import("./TAC.zig");

inline fn abs(src: i32) u32 {
    return if (src < 0) @intCast(-src) else @intCast(src);
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

pub const CondCode = enum {
    E,
    NE,
    G,
    GE,
    L,
    LE,

    const Self = @This();

    pub fn stringify(self: Self, allocator: std.mem.Allocator) ![]u8 {
        return switch (self) {
            .E => try std.fmt.allocPrint(allocator, "e", .{}),
            .NE => try std.fmt.allocPrint(allocator, "ne", .{}),
            .G => try std.fmt.allocPrint(allocator, "ne", .{}),
            .GE => try std.fmt.allocPrint(allocator, "ge", .{}),
            .L => try std.fmt.allocPrint(allocator, "l", .{}),
            .LE => try std.fmt.allocPrint(allocator, "le", .{}),
        };
    }

    pub fn getFromTacOp(op: tac.BinaryOp) Self {
        return switch (op) {
            .EQ => CondCode.E,
            .NOT_EQ => CondCode.NE,
            .LT => CondCode.L,
            .LT_EQ => CondCode.LE,
            .GT => CondCode.G,
            .GT_EQ => CondCode.GE,
            else => unreachable,
        };
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
            else => |op| {
                std.log.warn("Operand stringify: op={s}\n", .{op.Pseudo});
                unreachable;
            },
        }
    }

    pub fn is(self: Operand, comptime operandType: OperandType) bool {
        return std.meta.activeTag(self) == operandType;
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
    Cmp,
    Jmp,
    JmpCC,
    SetCC,
    Label,
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

pub const Cmp = struct {
    op1: Operand,
    op2: Operand,
};
pub const JmpCC = struct {
    code: CondCode,
    label: []u8,
};

pub const SetCC = struct {
    code: CondCode,
    dest: Operand,
};

pub const Instruction = union(InstructionType) {
    Mov: MovInst,
    Unary: UnaryInst,
    AllocateStack: u32,
    Ret: void,
    Binary: BinaryInst,
    Idiv: Operand,
    Cdq: void,
    Cmp: Cmp,
    Jmp: []u8,
    JmpCC: JmpCC,
    SetCC: SetCC,
    Label: []u8,

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
                const lhsOperand = try @constCast(&binary.lhs).stringify(allocator);
                const rhsOperand = try @constCast(&binary.rhs).stringify(allocator);
                const instrString = try binary.op.stringify(allocator);
                return (try std.fmt.allocPrint(allocator, "{s} {s},{s}", .{ instrString, rhsOperand, lhsOperand }));
            },
            .Cdq => {
                return (try std.fmt.allocPrint(allocator, "cdq", .{}));
            },
            .Idiv => |operand| {
                const operandStringified = try @constCast(&operand).stringify(allocator);
                return (try std.fmt.allocPrint(allocator, "idiv {s}", .{operandStringified}));
            },
            .Cmp => |cmp| {
                const op1Stringified = try @constCast(&cmp.op1).stringify(allocator);
                const op2Stringified = try @constCast(&cmp.op2).stringify(allocator);
                return (try std.fmt.allocPrint(allocator, "cmpl {s},{s}", .{ op1Stringified, op2Stringified }));
            },
            .Jmp => |jmpLabel| {
                return (try std.fmt.allocPrint(allocator, "jmp .L{s}", .{jmpLabel}));
            },
            .JmpCC => |jmpCC| {
                return (try std.fmt.allocPrint(allocator, "j{s} .L{s}", .{ (try jmpCC.code.stringify(allocator)), jmpCC.label }));
            },
            .SetCC => |setCC| {
                const destStringified = try @constCast(&setCC.dest).stringify(allocator);
                const code = try setCC.code.stringify(allocator);
                return (try std.fmt.allocPrint(allocator, "set{s} {s}", .{ code, destStringified }));
            },
            .Label => |label| {
                return (try std.fmt.allocPrint(allocator, ".L{s}:", .{label}));
            },
        }
    }
};

// This function fixes the instructions, stack to stack moves
pub fn fixupInstructions(instructions: *std.ArrayList(*Instruction), allocator: std.mem.Allocator) ast.CodegenError!std.ArrayList(*Instruction) {
    var fixedInstructions = try std.ArrayList(*Instruction).initCapacity(allocator, instructions.items.len);
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
                    //_ = instructions.orderedRemove(i);
                    const movInstSrc = try allocator.create(Instruction);
                    const movInstDest = try allocator.create(Instruction);
                    movInstSrc.* = Instruction{ .Mov = MovInst{ .src = mov.src, .dest = Operand{ .Reg = Reg.R10 } } };
                    movInstDest.* = Instruction{ .Mov = MovInst{
                        .src = Operand{ .Reg = Reg.R10 },
                        .dest = mov.dest,
                    } };
                    try fixedInstructions.append(
                        movInstSrc,
                    );
                    try fixedInstructions.append(
                        movInstDest,
                    );
                    continue;
                } else {
                    try fixedInstructions.append(inst);
                    continue;
                }
            },
            .Cmp => |cmp| {
                std.log.warn("op1 tagName: {s} and op2 tagName: {s}\n", .{ @tagName(cmp.op1), @tagName(cmp.op2) });
                if (std.mem.eql(u8, @tagName(cmp.op1), "Stack") and std.mem.eql(u8, @tagName(cmp.op2), "Stack")) {
                    const movInst = try allocator.create(Instruction);
                    movInst.* = Instruction{
                        .Mov = MovInst{ .src = cmp.op1, .dest = Operand{ .Reg = Reg.R10 } },
                    };
                    inst.Cmp.op1 = Operand{ .Reg = Reg.R10 };
                    try fixedInstructions.insert(i, movInst);
                    try fixedInstructions.append(inst);
                    continue;
                } else {
                    try fixedInstructions.append(inst);
                    continue;
                }
            },
            else => {
                try fixedInstructions.append(inst);
            },
        }
    }
    instructions.deinit(); // T
    // TODO:(less priority) doing this in place is a possible enhancement, also the stack allocation is problematic
    // TODO:(high priority) fix the stack allocation issue by passing a fixedInstructions array list pointer to this function.
    return fixedInstructions;
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
                            const movIdivArgToR10 = try allocator.create(Instruction);
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
                        const movIdivArgToR10 = try allocator.create(Instruction);
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
                        const movIdivArgToR10 = try allocator.create(Instruction);
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
            .Cmp => |cmp| {
                std.log.warn("Old inst: {any}\n", .{inst.Cmp});
                switch (cmp.op1) {
                    .Pseudo => |pseudo| {
                        if (lookup.contains(pseudo)) {
                            inst.Cmp.op1 = Operand{ .Stack = lookup.get(pseudo).? };
                        } else {
                            topOfStack -= 4;
                            try lookup.put(
                                pseudo,
                                topOfStack,
                            );
                            inst.Cmp.op1 = Operand{ .Stack = topOfStack };
                        }
                    },
                    else => {},
                }
                switch (cmp.op2) {
                    .Pseudo => |pseudo| {
                        if (lookup.contains(pseudo)) {
                            inst.Cmp.op2 = Operand{ .Stack = lookup.get(pseudo).? };
                            continue;
                        } else {
                            topOfStack -= 4;
                            try lookup.put(
                                pseudo,
                                topOfStack,
                            );
                            inst.Cmp.op2 = Operand{ .Stack = topOfStack };
                        }
                    },
                    else => {},
                }
                std.log.warn("New inst: {any}\n", .{inst.Cmp});
            },
            .SetCC => |setCC| {
                switch (setCC.dest) {
                    .Pseudo => |pseudo| {
                        if (lookup.contains(pseudo)) {
                            inst.SetCC.dest = Operand{ .Stack = lookup.get(pseudo).? };
                            continue;
                        } else {
                            topOfStack -= 4;
                            try lookup.put(
                                pseudo,
                                topOfStack,
                            );
                            inst.SetCC.dest = Operand{ .Stack = topOfStack };
                        }
                    },
                    else => {},
                }
            },
            .Jmp, .Label, .JmpCC => {
                // Jmp does not involve any operands
            },
        }
    }
    const allocateStackInst = try allocator.create(Instruction);
    allocateStackInst.* = Instruction{
        .AllocateStack = @as(u32, abs(topOfStack)),
    };
    try instructions.insert(
        0,
        allocateStackInst,
    );
}
