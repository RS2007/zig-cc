const std = @import("std");
const ast = @import("./AST.zig");
const tac = @import("./TAC.zig");

inline fn abs(src: i32) u32 {
    return if (src < 0) @intCast(-src) else @intCast(src);
}

pub const Program = struct {
    topLevelDecls: std.ArrayList(*TopLevelDecl),
    const Self = @This();
    pub fn stringify(self: *Self, writer: std.fs.File.Writer, allocator: std.mem.Allocator, asmSymbolTable: std.StringHashMap(*Symbol)) !void {
        try writer.writeAll(
            \\ .section .note.GNU-stack,"",@progbits
            \\ .section .text
            \\
        );
        try writer.writeAll(".globl main\n");
        for (self.topLevelDecls.items) |topLevelDecl| {
            switch (topLevelDecl.*) {
                .Function => |function| {
                    try replacePseudoRegs(function, allocator, asmSymbolTable);
                    const fixedAsmInstructions = try fixupInstructions(&function.instructions, allocator);
                    function.instructions = fixedAsmInstructions;
                    const fnString = try function.stringify(allocator);
                    try writer.writeAll(fnString);
                    try writer.writeAll("\n");
                },
                .StaticVar => |staticVar| {
                    const staticVarString = try staticVar.stringify(allocator);
                    try writer.writeAll(staticVarString);
                    try writer.writeAll("\n");
                },
            }
        }
    }
};

pub const TopLevelDeclType = enum {
    Function,
    StaticVar,
};

pub const AsmType = enum {
    LongWord,
    QuadWord,
    pub fn suffix(asmType: AsmType) u8 {
        return switch (asmType) {
            .LongWord => 'l',
            .QuadWord => 'q',
        };
    }
};

pub const TopLevelDecl = union(TopLevelDeclType) {
    Function: *Function,
    StaticVar: *StaticVar,
};

pub const Function = struct {
    name: []u8,
    args: std.ArrayList([]u8),
    instructions: std.ArrayList(*Instruction),
    const Self = @This();
    pub fn stringify(self: *Self, allocator: std.mem.Allocator) ast.CodegenError![]u8 {
        var buf = std.ArrayList(u8).init(allocator);
        const fnLabelOffset = try std.fmt.allocPrint(allocator, ".section .text\n {s}:\n", .{self.name});
        try buf.appendSlice(fnLabelOffset);

        const fnPrologue = try std.fmt.allocPrint(allocator, "push %rbp\nmov %rsp, %rbp", .{});
        try buf.appendSlice(fnPrologue);

        //TODO: probably should add the mov instruction here, but omitting for
        //now
        for (self.instructions.items) |asmInst| {
            const printedSlice = try std.fmt.allocPrint(allocator, "\n{s}", .{try asmInst.stringify(allocator)});
            try buf.appendSlice(printedSlice);
        }
        return buf.toOwnedSlice();
    }
};

pub const Symbol = union(enum) {
    Function: struct {
        defined: bool,
        //INFO: Is function defined in the translation unit? If
        //not then its from libc
    },
    Obj: struct {
        type: AsmType,
        static: bool,
    },
};

pub const StaticInit = union(enum) {
    Integer: u32,
    Long: u64,

    pub fn isZero(self: StaticInit) bool {
        return switch (self) {
            .Integer => |integer| integer == 0,
            .Long => |long| long == 0,
        };
    }
};

pub const StaticVar = struct {
    name: []u8,
    global: bool,
    init: StaticInit,
    alignment: u32,
    const Self = @This();
    pub fn stringify(self: *Self, allocator: std.mem.Allocator) ![]u8 {
        // TODO: Hack for now, whenever a glob is encountered, emit the data
        // section directive
        // And for functions always start with a .text directive
        var buf = std.ArrayList(u8).init(allocator);
        const code = try std.fmt.allocPrint(allocator,
            \\ {s} {s}
            \\ {s}
            \\ .align 4
            \\ {s}:
            \\ {s} 4
        , .{
            if (self.global) ".globl" else ".local",
            self.name,
            if (self.init.isZero()) ".bss" else ".data",
            self.name,
            if (self.init.isZero()) ".zero" else ".long",
        });
        try buf.appendSlice(code);

        return buf.toOwnedSlice();
    }
};

pub const Reg = enum {
    AX,
    R10,
    DX,
    R11,
    EDI,
    ESI,
    EDX,
    ECX,
    R8,
    R9,
    EAX,
    R11_64, // TODO: Rename this later
    R10_64,
    pub fn stringify(register: Reg, allocator: std.mem.Allocator) ast.CodegenError![]u8 {
        switch (register) {
            .AX => {
                return (try std.fmt.allocPrint(allocator, "%eax", .{}));
            },
            .R10 => {
                return (try std.fmt.allocPrint(allocator, "%r10d", .{}));
            },
            .R10_64 => {
                return (try std.fmt.allocPrint(allocator, "%r10", .{}));
            },
            .DX => {
                return (try std.fmt.allocPrint(allocator, "%edx", .{}));
            },
            .R11 => {
                return (try std.fmt.allocPrint(allocator, "%r11d", .{}));
            },
            .R11_64 => {
                return (try std.fmt.allocPrint(allocator, "%r11", .{}));
            },
            .EDI => {
                return (try std.fmt.allocPrint(allocator, "%edi", .{}));
            },
            .ESI => {
                return (try std.fmt.allocPrint(allocator, "%esi", .{}));
            },
            .EDX => {
                return (try std.fmt.allocPrint(allocator, "%edx", .{}));
            },
            .ECX => {
                return (try std.fmt.allocPrint(allocator, "%ecx", .{}));
            },
            .R8 => {
                return (try std.fmt.allocPrint(allocator, "%r8d", .{}));
            },
            .R9 => {
                return (try std.fmt.allocPrint(allocator, "%r9d", .{}));
            },
            .EAX => {
                return (try std.fmt.allocPrint(allocator, "%eax", .{}));
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

pub const OperandType = enum { Imm, Reg, Pseudo, Data, Stack };

pub const Operand = union(OperandType) {
    Imm: u64,
    Reg: Reg,
    Pseudo: []u8,
    Data: []u8,
    Stack: i32,
    inline fn isOfKind(self: Operand, comptime operandType: OperandType) bool {
        return std.meta.activeTag(self) == operandType;
    }
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
            .Data => |data| {
                return (try std.fmt.allocPrint(allocator, "{s}(%rip)", .{data}));
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
    FnCall,
    Movsx,
};

pub const MovInst = struct {
    src: Operand,
    dest: Operand,
    type: AsmType,
};

pub const UnaryInst = struct {
    op: UnaryOp,
    rhs: Operand,
    type: AsmType,
};

pub const BinaryInst = struct {
    op: BinaryOp,
    lhs: Operand,
    rhs: Operand,
    type: AsmType,
};

pub const Cmp = struct {
    op1: Operand,
    op2: Operand,
    type: AsmType,
};
pub const JmpCC = struct {
    code: CondCode,
    label: []u8,
};

pub const SetCC = struct {
    code: CondCode,
    dest: Operand,
};

pub const FnCall = struct {
    name: []u8,
};

pub const Idiv = struct {
    src: Operand,
    type: AsmType,
};

pub const Movsx = struct {
    src: Operand,
    dest: Operand,
};

pub const Instruction = union(InstructionType) {
    Mov: MovInst,
    Unary: UnaryInst,
    AllocateStack: u32,
    Ret: void,
    Binary: BinaryInst,
    Idiv: Operand,
    Cdq: AsmType,
    Cmp: Cmp,
    Jmp: []u8,
    JmpCC: JmpCC,
    SetCC: SetCC,
    Label: []u8,
    FnCall: FnCall,
    Movsx: Movsx,

    pub fn stringify(instruction: *Instruction, allocator: std.mem.Allocator) ast.CodegenError![]u8 {
        switch (instruction.*) {
            .Mov => |mov| {
                std.log.warn("Mov stringify: {any}\n", .{mov});
                return (try std.fmt.allocPrint(allocator, "mov{c} {s},{s}", .{ mov.type.suffix(), try Operand.stringify(@constCast(&mov.src), allocator), try Operand.stringify(@constCast(&mov.dest), allocator) }));
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
                return (try std.fmt.allocPrint(allocator, "sub $0x{x},%rsp", .{allocStack}));
            },
            .Ret => {
                return (try std.fmt.allocPrint(allocator, "leaveq\nretq", .{}));
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
                return (try std.fmt.allocPrint(allocator, "cmp{c} {s},{s}", .{ cmp.type.suffix(), op1Stringified, op2Stringified }));
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
            .FnCall => |fnCall| {
                return (try std.fmt.allocPrint(allocator, "call {s}", .{fnCall.name}));
            },
            .Movsx => |movsx| {
                std.log.warn("Hey I am hit\n", .{});
                return (try std.fmt.allocPrint(allocator, "movsx {s},{s}", .{ try Operand.stringify(@constCast(&movsx.src), allocator), try Operand.stringify(@constCast(&movsx.dest), allocator) }));
            },
        }
    }
};

inline fn fixMultiply(inst: *Instruction, allocator: std.mem.Allocator, fixedInstructions: *std.ArrayList(*Instruction)) ast.CodegenError!void {
    if (inst.Binary.lhs.isOfKind(.Stack)) {
        const movLeftToR11 = try allocator.create(Instruction);
        movLeftToR11.* = Instruction{
            .Mov = MovInst{
                .dest = Operand{ .Reg = Reg.R11 },
                .src = inst.Binary.lhs,
                .type = inst.Binary.type,
            },
        };
        const movR11ToLeft = try allocator.create(Instruction);
        movR11ToLeft.* = Instruction{ .Mov = MovInst{
            .dest = inst.Binary.lhs,
            .src = Operand{ .Reg = Reg.R11 },
            .type = inst.Binary.type,
        } };
        inst.Binary.lhs = Operand{ .Reg = Reg.R11 };
        try fixedInstructions.append(movLeftToR11);
        try fixedInstructions.append(inst);
        try fixedInstructions.append(movR11ToLeft);
    } else {
        try fixedInstructions.append(inst);
    }
}

// This function fixes the instructions, stack to stack moves
pub fn fixupInstructions(instructions: *std.ArrayList(*Instruction), allocator: std.mem.Allocator) ast.CodegenError!std.ArrayList(*Instruction) {
    var fixedInstructions = try std.ArrayList(*Instruction).initCapacity(allocator, instructions.items.len);
    for (instructions.items) |inst| {
        switch (inst.*) {
            .Mov => |mov| {
                const isSrcMem = mov.src.isOfKind(.Data) or mov.src.isOfKind(.Stack);
                const isDestMem = mov.dest.isOfKind(.Data) or mov.dest.isOfKind(.Stack);
                if (isSrcMem and isDestMem) {
                    //_ = instructions.orderedRemove(i);
                    const movInstSrc = try allocator.create(Instruction);
                    const movInstDest = try allocator.create(Instruction);
                    movInstSrc.* = Instruction{ .Mov = MovInst{
                        .src = mov.src,
                        .dest = Operand{ .Reg = switch (mov.type) {
                            .LongWord => Reg.R10,
                            .QuadWord => Reg.R10_64,
                        } },
                        .type = mov.type,
                    } };
                    movInstDest.* = Instruction{ .Mov = MovInst{
                        .src = Operand{ .Reg = switch (mov.type) {
                            .LongWord => Reg.R10,
                            .QuadWord => Reg.R10_64,
                        } },
                        .dest = mov.dest,
                        .type = mov.type,
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
            .Movsx => {
                try fixedInstructions.append(inst);
            },
            .Binary => |binary| {
                if (binary.op == BinaryOp.Multiply) {
                    try fixMultiply(inst, allocator, &fixedInstructions);
                    continue;
                }

                const isSrcMem = binary.lhs.isOfKind(.Data) or binary.lhs.isOfKind(.Stack);
                const isDestMem = binary.rhs.isOfKind(.Data) or binary.rhs.isOfKind(.Stack);

                if (isSrcMem and isDestMem) {
                    const cpInstruction = try allocator.create(Instruction);
                    cpInstruction.* = Instruction{
                        .Mov = .{
                            .dest = Operand{ .Reg = switch (binary.type) {
                                .LongWord => Reg.R10,
                                .QuadWord => Reg.R10_64,
                            } },
                            .src = binary.rhs,
                            .type = binary.type,
                        },
                    };
                    inst.Binary.rhs = Operand{ .Reg = switch (binary.type) {
                        .LongWord => Reg.R10,
                        .QuadWord => Reg.R10_64,
                    } };
                    try fixedInstructions.append(cpInstruction);
                    try fixedInstructions.append(inst);
                    continue;
                } else {
                    try fixedInstructions.append(inst);
                    continue;
                }
            },
            .Cmp => |cmp| {
                const isOp1Mem = cmp.op1.isOfKind(.Data) or cmp.op1.isOfKind(.Stack);
                const isOp2Mem = cmp.op2.isOfKind(.Data) or cmp.op2.isOfKind(.Stack);
                if (isOp1Mem and isOp2Mem) {
                    const movInst = try allocator.create(Instruction);
                    movInst.* = Instruction{
                        .Mov = MovInst{
                            .src = cmp.op1,
                            .dest = Operand{ .Reg = switch (cmp.type) {
                                .LongWord => Reg.R10,
                                .QuadWord => Reg.R10_64,
                            } },
                            .type = cmp.type,
                        },
                    };
                    inst.Cmp.op1 = Operand{ .Reg = switch (cmp.type) {
                        .LongWord => Reg.R10,
                        .QuadWord => Reg.R10_64,
                    } };
                    try fixedInstructions.append(movInst);
                    try fixedInstructions.append(inst);
                    continue;
                } else {
                    try fixedInstructions.append(inst);
                    continue;
                }
            },
            .Idiv => {
                try fixupIdiv(
                    inst,
                    allocator,
                    &fixedInstructions,
                );
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

inline fn fixupIdiv(
    instruction: *Instruction,
    allocator: std.mem.Allocator,
    fixedInstructions: *std.ArrayList(*Instruction),
) ast.CodegenError!void {
    switch (instruction.Idiv) {
        .Reg => {
            const movIdivArgToR10 = try allocator.create(Instruction);
            movIdivArgToR10.* = Instruction{ .Mov = MovInst{
                .src = instruction.Idiv,
                .dest = Operand{ .Reg = Reg.R10 },
                .type = AsmType.LongWord,
            } };
            try fixedInstructions.append(
                movIdivArgToR10,
            );
            const divInstruction = try allocator.create(Instruction);
            divInstruction.* = .{
                .Idiv = Operand{ .Reg = Reg.R10 },
            };
            try fixedInstructions.append(divInstruction);
        },
        .Pseudo => {
            unreachable;
        },
        .Imm => |imm| {
            const movIdivArgToR10 = try allocator.create(Instruction);
            movIdivArgToR10.* = Instruction{ .Mov = MovInst{
                .src = Operand{ .Imm = imm },
                .dest = Operand{ .Reg = Reg.R10 },
                .type = AsmType.LongWord,
            } };
            try fixedInstructions.append(
                movIdivArgToR10,
            );
            const divInstruction = try allocator.create(Instruction);
            divInstruction.* = .{
                .Idiv = Operand{ .Reg = Reg.R10 },
            };
            try fixedInstructions.append(divInstruction);
        },
        .Data => |data| {
            const movIdivArgToR10 = try allocator.create(Instruction);
            movIdivArgToR10.* = Instruction{ .Mov = MovInst{
                .src = Operand{ .Data = data },
                .dest = Operand{ .Reg = Reg.R10 },
                .type = AsmType.LongWord,
            } };
            try fixedInstructions.append(
                movIdivArgToR10,
            );
            const divInstruction = try allocator.create(Instruction);
            divInstruction.* = .{
                .Idiv = Operand{ .Reg = Reg.R10 },
            };
            try fixedInstructions.append(divInstruction);
        },
        .Stack => |stack| {
            const movIdivArgToR10 = try allocator.create(Instruction);
            movIdivArgToR10.* = Instruction{ .Mov = MovInst{
                .src = Operand{ .Stack = stack },
                .dest = Operand{ .Reg = Reg.R10 },
                .type = AsmType.LongWord,
            } };
            try fixedInstructions.append(
                movIdivArgToR10,
            );
            const divInstruction = try allocator.create(Instruction);
            divInstruction.* = .{
                .Idiv = Operand{ .Reg = Reg.R10 },
            };
            try fixedInstructions.append(divInstruction);
        },
    }
}

pub fn replacePseudoRegs(function: *Function, allocator: std.mem.Allocator, asmSymbolTable: std.StringHashMap(*Symbol)) ast.CodegenError!void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const hashAllocator = arena.allocator();
    defer arena.deinit();
    var lookup = std.StringHashMap(i32).init(hashAllocator);
    var topOfStack: i32 = 0;
    for (function.instructions.items) |inst| {
        switch (inst.*) {
            .Mov => |mov| {
                if (mov.src.isOfKind(.Pseudo)) {
                    try replacePseudo(
                        &inst.Mov.src,
                        &lookup,
                        @constCast(&asmSymbolTable),
                        &topOfStack,
                    );
                }
                if (mov.dest.isOfKind(.Pseudo)) {
                    try replacePseudo(
                        &inst.Mov.dest,
                        &lookup,
                        @constCast(&asmSymbolTable),
                        &topOfStack,
                    );
                }
            },
            .Movsx => |mov| {
                if (mov.src.isOfKind(.Pseudo)) {
                    try replacePseudo(
                        &inst.Movsx.src,
                        &lookup,
                        @constCast(&asmSymbolTable),
                        &topOfStack,
                    );
                }
                if (mov.dest.isOfKind(.Pseudo)) {
                    try replacePseudo(
                        &inst.Movsx.dest,
                        &lookup,
                        @constCast(&asmSymbolTable),
                        &topOfStack,
                    );
                }
            },
            .Unary => |unary| {
                if (unary.rhs.isOfKind(.Pseudo)) {
                    try replacePseudo(
                        &inst.Unary.rhs,
                        &lookup,
                        @constCast(&asmSymbolTable),
                        &topOfStack,
                    );
                }
            },
            .Binary => |binary| {
                if (binary.lhs.isOfKind(.Pseudo)) {
                    try replacePseudo(
                        &inst.Binary.lhs,
                        &lookup,
                        @constCast(&asmSymbolTable),
                        &topOfStack,
                    );
                }
                if (binary.rhs.isOfKind(.Pseudo)) {
                    try replacePseudo(
                        &inst.Binary.rhs,
                        &lookup,
                        @constCast(&asmSymbolTable),
                        &topOfStack,
                    );
                }
            },
            .Idiv => |idiv| {
                if (idiv.isOfKind(.Pseudo)) {
                    try replacePseudo(
                        &inst.Idiv,
                        &lookup,
                        @constCast(&asmSymbolTable),
                        &topOfStack,
                    );
                }
            },
            .Cdq, .AllocateStack, .Ret => {},
            .Cmp => |cmp| {
                if (cmp.op1.isOfKind(.Pseudo)) {
                    try replacePseudo(
                        &inst.Cmp.op1,
                        &lookup,
                        @constCast(&asmSymbolTable),
                        &topOfStack,
                    );
                }
                if (cmp.op2.isOfKind(.Pseudo)) {
                    try replacePseudo(
                        &inst.Cmp.op2,
                        &lookup,
                        @constCast(&asmSymbolTable),
                        &topOfStack,
                    );
                }
            },
            .SetCC => |setCC| {
                if (setCC.dest.isOfKind(.Pseudo)) {
                    try replacePseudo(
                        &inst.SetCC.dest,
                        &lookup,
                        @constCast(&asmSymbolTable),
                        &topOfStack,
                    );
                }
            },
            .Jmp, .Label, .JmpCC, .FnCall => {
                // Jmp does not involve any operands
            },
        }
    }
    if (topOfStack != 0) {
        const allocateStackInst = try allocator.create(Instruction);
        allocateStackInst.* = Instruction{
            .AllocateStack = @as(u32, abs(topOfStack)),
        };
        try function.instructions.insert(
            0,
            allocateStackInst,
        );
    }
}

inline fn replacePseudo(
    operand: *Operand,
    lookup: *std.StringHashMap(i32),
    asmSymbolTable: *std.StringHashMap(*Symbol),
    topOfStack: *i32,
) ast.CodegenError!void {
    if (lookup.contains(operand.Pseudo)) {
        operand.* = Operand{ .Stack = lookup.get(operand.Pseudo).? };
    } else {
        const offset: i32 = switch (asmSymbolTable.get(operand.Pseudo).?.Obj.type) {
            .QuadWord => 8,
            .LongWord => 4,
        };
        topOfStack.* = topOfStack.* - offset;
        try lookup.put(
            operand.Pseudo,
            topOfStack.*,
        );
        operand.* = Operand{ .Stack = topOfStack.* };
    }
}
