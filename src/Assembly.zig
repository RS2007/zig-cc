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
            \\ .section .data
            \\ .local longUpperBound 
            \\ .align 8
            \\ longUpperBound:
            \\ .double 9223372036854775808.0
            \\ .local uIntUpperBound
            \\ .align 8 
            \\ uIntUpperBound: 
            \\ .double 4294967295.0
            \\ .local negZero 
            \\ .align 16
            \\ negZero:
            \\ .double -0.0
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

pub inline fn createInst(comptime kind: InstructionType, contents: anytype, allocator: std.mem.Allocator) ast.CodegenError!*Instruction {
    const inst = try allocator.create(Instruction);
    inst.* = @unionInit(
        Instruction,
        @tagName(kind),
        contents,
    );
    return inst;
}

pub const AsmType = enum {
    LongWord,
    QuadWord,
    Float,

    const Self = @This();
    pub inline fn from(comptime T: type, val: T) Self {
        if (T == ast.Type) {
            const casted: ast.Type = val;
            return switch (casted) {
                .Integer, .UInteger => .LongWord,
                .Long, .ULong => .QuadWord,
                .Float => .Float,
                else => unreachable,
            };
        }
        unreachable;
    }

    pub inline fn getCmpInst(asmType: AsmType) []u8 {
        return @constCast(switch (asmType) {
            .LongWord => "cmpl",
            .QuadWord => "cmpq",
            .Float => "comisd",
        });
    }
    pub inline fn suffix(asmType: AsmType) []u8 {
        return @constCast(switch (asmType) {
            .LongWord => "l",
            .QuadWord => "q",
            .Float => "sd",
        });
    }
    pub inline fn getAXVariety(asmType: AsmType) Reg {
        return switch (asmType) {
            .LongWord => .EAX,
            .QuadWord => .RAX,
            .Float => .XMM0,
        };
    }
    pub inline fn getDXVariety(asmType: AsmType) Reg {
        return switch (asmType) {
            .LongWord => .EDX,
            .QuadWord => .RDX,
            .Float => unreachable,
        };
    }
    pub inline fn getR10Variety(asmType: AsmType) Reg {
        return switch (asmType) {
            .LongWord => .R10,
            .QuadWord => .R10_64,
            .Float => .XMM15,
        };
    }
    pub inline fn getR11Variety(asmType: AsmType) Reg {
        return switch (asmType) {
            .LongWord => .R11,
            .QuadWord => .R11_64,
            .Float => .XMM14,
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
        signed: bool,
    },
};

pub const StaticInit = union(enum) {
    Integer: i32,
    Long: i64,
    Float: f64,

    pub fn isZero(self: StaticInit) bool {
        return switch (self) {
            .Integer => |integer| integer == 0,
            .Long => |long| long == 0,
            .Float => false,
        };
    }
    pub fn asmTypeString(self: StaticInit, allocator: std.mem.Allocator) ast.CodegenError![]u8 {
        return switch (self) {
            .Integer => try std.fmt.allocPrint(allocator, ".long", .{}),
            .Long => try std.fmt.allocPrint(allocator, ".quad", .{}),
            .Float => try std.fmt.allocPrint(allocator, ".double", .{}),
        };
    }
    pub fn render(self: StaticInit, allocator: std.mem.Allocator) ast.CodegenError![]u8 {
        return switch (self) {
            .Integer => |integer| try std.fmt.allocPrint(allocator, "{}", .{integer}),
            .Long => |long| try std.fmt.allocPrint(allocator, "{}", .{long}),
            .Float => |float| try std.fmt.allocPrint(allocator, "{}", .{float}),
        };
    }
};

pub const StaticVar = struct {
    name: []u8,
    global: bool,
    init: StaticInit,
    alignment: u32,
    const Self = @This();
    pub fn stringify(self: *Self, allocator: std.mem.Allocator) ast.CodegenError![]u8 {
        // TODO: Hack for now, whenever a glob is encountered, emit the data
        // section directive
        // And for functions always start with a .text directive
        var buf = std.ArrayList(u8).init(allocator);
        const code = try std.fmt.allocPrint(allocator,
            \\ {s} {s}
            \\ {s}
            \\ .align {d}
            \\ {s}:
            \\ {s} {s}
        , .{
            if (self.global) ".globl" else ".local",
            self.name,
            if (self.init.isZero()) ".bss" else ".data",
            self.alignment,
            self.name,
            if (self.init.isZero()) ".zero" else (try self.init.asmTypeString(allocator)),
            if (self.init.isZero()) (try std.fmt.allocPrint(allocator, "{}", .{self.alignment})) else try self.init.render(allocator),
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
    RDI,
    RSI,
    RDX,
    RCX,
    R8_64,
    R9_64,
    EAX,
    RAX,
    R11_64, // TODO: Rename this later
    R10_64,
    AL,
    XMM0,
    XMM1,
    XMM15,
    XMM14,
    pub fn stringify(register: Reg, allocator: std.mem.Allocator) ast.CodegenError![]u8 {
        switch (register) {
            .AL => {
                return (try std.fmt.allocPrint(allocator, "%al", .{}));
            },
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
            .RDI => {
                return (try std.fmt.allocPrint(allocator, "%rdi", .{}));
            },
            .RSI => {
                return (try std.fmt.allocPrint(allocator, "%rsi", .{}));
            },
            .RDX => {
                return (try std.fmt.allocPrint(allocator, "%rdx", .{}));
            },
            .RCX => {
                return (try std.fmt.allocPrint(allocator, "%rcx", .{}));
            },
            .R8 => {
                return (try std.fmt.allocPrint(allocator, "%r8d", .{}));
            },
            .R9 => {
                return (try std.fmt.allocPrint(allocator, "%r9d", .{}));
            },
            .R8_64 => {
                return (try std.fmt.allocPrint(allocator, "%r8", .{}));
            },
            .R9_64 => {
                return (try std.fmt.allocPrint(allocator, "%r9", .{}));
            },
            .EAX => {
                return (try std.fmt.allocPrint(allocator, "%eax", .{}));
            },
            .RAX => {
                return (try std.fmt.allocPrint(allocator, "%rax", .{}));
            },
            .XMM0 => {
                return (try std.fmt.allocPrint(allocator, "%xmm0", .{}));
            },
            .XMM1 => {
                return (try std.fmt.allocPrint(allocator, "%xmm1", .{}));
            },
            .XMM14 => {
                return (try std.fmt.allocPrint(allocator, "%xmm14", .{}));
            },
            .XMM15 => {
                return (try std.fmt.allocPrint(allocator, "%xmm15", .{}));
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
    A,
    AE,
    B,
    BE,

    const Self = @This();

    pub fn stringify(self: Self, allocator: std.mem.Allocator) ![]u8 {
        return switch (self) {
            .E => try std.fmt.allocPrint(allocator, "e", .{}),
            .NE => try std.fmt.allocPrint(allocator, "ne", .{}),
            .G => try std.fmt.allocPrint(allocator, "ne", .{}),
            .GE => try std.fmt.allocPrint(allocator, "ge", .{}),
            .L => try std.fmt.allocPrint(allocator, "l", .{}),
            .LE => try std.fmt.allocPrint(allocator, "le", .{}),
            .A => try std.fmt.allocPrint(allocator, "a", .{}),
            .B => try std.fmt.allocPrint(allocator, "b", .{}),
            .AE => try std.fmt.allocPrint(allocator, "ae", .{}),
            .BE => try std.fmt.allocPrint(allocator, "be", .{}),
        };
    }

    pub fn getFromTacOp(op: tac.BinaryOp, signed: bool, isDouble: bool) Self {
        return if (signed or !isDouble)
            switch (op) {
                .EQ => CondCode.E,
                .NOT_EQ => CondCode.NE,
                .LT => CondCode.L,
                .LT_EQ => CondCode.LE,
                .GT => CondCode.G,
                .GT_EQ => CondCode.GE,
                else => unreachable,
            }
        else switch (op) {
            .EQ => CondCode.E,
            .NOT_EQ => CondCode.NE,
            .LT => CondCode.B,
            .LT_EQ => CondCode.BE,
            .GT => CondCode.A,
            .GT_EQ => CondCode.AE,
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
    Shr,
};

pub const BinaryOp = enum {
    Add,
    Subtract,
    Multiply,
    Divide,
    Xor,
    pub fn stringify(self: BinaryOp, asmType: AsmType, allocator: std.mem.Allocator) ast.CodegenError![]u8 {
        const suffix = switch (asmType) {
            .LongWord => "l",
            .QuadWord => "q",
            .Float => "sd",
        };
        return if (self == .Xor) try std.fmt.allocPrint(allocator, "xorpd", .{}) else (try std.fmt.allocPrint(allocator, "{s}{s}", .{
            switch (self) {
                .Add => "add",
                .Subtract => "sub",
                .Multiply => if (asmType == .Float) "mul" else "imul",
                .Divide => "div",
                else => unreachable,
            },
            suffix.ptr,
        }));
    }
};

pub const InstructionType = enum {
    Mov,
    Unary,
    AllocateStack,
    Ret,
    Binary,
    Idiv,
    Div,
    Cdq,
    Cmp,
    Jmp,
    JmpCC,
    SetCC,
    Label,
    FnCall,
    Movsx,
    Movzx, // move zero extend
    Cvttsd2si,
    Cvtsi2sd,
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

pub const Movzx = struct {
    src: Operand,
    dest: Operand,
};

pub const Cvttsd2si = struct {
    src: Operand,
    dest: Operand,
    type: AsmType,
};

pub const Cvtsi2sd = struct {
    src: Operand,
    dest: Operand,
    type: AsmType,
};

pub const Instruction = union(InstructionType) {
    Mov: MovInst,
    Unary: UnaryInst,
    AllocateStack: u32,
    Ret: void,
    Binary: BinaryInst,
    Idiv: Idiv,
    Div: Idiv,
    Cdq: AsmType,
    Cmp: Cmp,
    Jmp: []u8,
    JmpCC: JmpCC,
    SetCC: SetCC,
    Label: []u8,
    FnCall: FnCall,
    Movsx: Movsx,
    Movzx: Movzx,
    Cvttsd2si: Cvttsd2si,
    Cvtsi2sd: Cvtsi2sd,

    pub fn stringify(instruction: *Instruction, allocator: std.mem.Allocator) ast.CodegenError![]u8 {
        switch (instruction.*) {
            .Mov => |mov| {
                std.log.warn("Mov stringify: {any}\n", .{mov});
                return (try std.fmt.allocPrint(allocator, "mov{s} {s},{s}", .{ mov.type.suffix(), try Operand.stringify(@constCast(&mov.src), allocator), try Operand.stringify(@constCast(&mov.dest), allocator) }));
            },
            .Unary => |unary| {
                switch (unary.op) {
                    .Neg => {
                        return (try std.fmt.allocPrint(allocator, "neg{s} {s}", .{ unary.type.suffix(), try @constCast(&unary.rhs).stringify(allocator) }));
                    },
                    .Not => {
                        return (try std.fmt.allocPrint(allocator, "notl {s}", .{try @constCast(&unary.rhs).stringify(allocator)}));
                    },
                    .Shr => {
                        return (try std.fmt.allocPrint(allocator, "shr{s} {s}", .{ unary.type.suffix(), try @constCast(&unary.rhs).stringify(allocator) }));
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
                const instrString = try binary.op.stringify(binary.type, allocator);
                return (try std.fmt.allocPrint(allocator, "{s} {s},{s}", .{ instrString, rhsOperand, lhsOperand }));
            },
            .Cdq => {
                return (try std.fmt.allocPrint(allocator, "cdq", .{}));
            },
            .Idiv => |idiv| {
                const operandStringified = try @constCast(&idiv.src).stringify(allocator);
                return (try std.fmt.allocPrint(allocator, "idiv {s}", .{operandStringified}));
            },
            .Div => |div| {
                const operandStringified = try @constCast(&div.src).stringify(allocator);
                return (try std.fmt.allocPrint(allocator, "div {s}", .{operandStringified}));
            },
            .Cmp => |cmp| {
                const op1Stringified = try @constCast(&cmp.op1).stringify(allocator);
                const op2Stringified = try @constCast(&cmp.op2).stringify(allocator);
                return (try std.fmt.allocPrint(allocator, "{s} {s},{s}", .{ cmp.type.getCmpInst(), op1Stringified, op2Stringified }));
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
                return (try std.fmt.allocPrint(allocator, "set{s} %al\nmovzbl %al,%eax\n movl %eax,{s}", .{ code, destStringified }));
            },
            .Label => |label| {
                return (try std.fmt.allocPrint(allocator, ".L{s}:", .{label}));
            },
            .FnCall => |fnCall| {
                return (try std.fmt.allocPrint(allocator, "call {s}", .{fnCall.name}));
            },
            .Movsx => |movsx| {
                return (try std.fmt.allocPrint(allocator, "movslq {s},{s}", .{ try Operand.stringify(@constCast(&movsx.src), allocator), try Operand.stringify(@constCast(&movsx.dest), allocator) }));
            },
            .Movzx => {
                @panic("Never hit this, movzx should be replaced by two mov instructions");
            },
            .Cvttsd2si => |cvttsd2si| {
                const srcString = try @constCast(&cvttsd2si.src).stringify(allocator);
                const destString = try @constCast(&cvttsd2si.dest).stringify(allocator);
                return (try std.fmt.allocPrint(allocator, "cvttsd2si{s} {s},{s}", .{
                    cvttsd2si.type.suffix(),
                    srcString,
                    destString,
                }));
            },
            .Cvtsi2sd => |cvtsi2sd| {
                const srcString = try @constCast(&cvtsi2sd.src).stringify(allocator);
                const destString = try @constCast(&cvtsi2sd.dest).stringify(allocator);
                return (try std.fmt.allocPrint(allocator, "cvtsi2sd{s} {s},{s}", .{
                    cvtsi2sd.type.suffix(),
                    srcString,
                    destString,
                }));
            },
        }
    }
};

inline fn fixMultiply(inst: *Instruction, allocator: std.mem.Allocator, fixedInstructions: *std.ArrayList(*Instruction)) ast.CodegenError!void {
    if (inst.Binary.lhs.isOfKind(.Stack)) {
        const movLeftToR11 = try allocator.create(Instruction);
        movLeftToR11.* = Instruction{
            .Mov = MovInst{
                .dest = Operand{
                    .Reg = inst.Binary.type.getR11Variety(),
                },
                .src = inst.Binary.lhs,
                .type = inst.Binary.type,
            },
        };
        const movR11ToLeft = try allocator.create(Instruction);
        movR11ToLeft.* = Instruction{ .Mov = MovInst{
            .dest = inst.Binary.lhs,
            .src = Operand{
                .Reg = inst.Binary.type.getR11Variety(),
            },
            .type = inst.Binary.type,
        } };
        inst.Binary.lhs = Operand{
            .Reg = inst.Binary.type.getR11Variety(),
        };
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
                // in case of quad word mov instructions, a quadword immediate
                // cannot be moved directly into memory, hence require an
                // intermediate register move
                const quadWordImmMov = (mov.type == .QuadWord) and mov.src.isOfKind(.Imm);
                if ((isSrcMem and isDestMem) or quadWordImmMov) {
                    //_ = instructions.orderedRemove(i);
                    const movInstSrc = try allocator.create(Instruction);
                    const movInstDest = try allocator.create(Instruction);
                    movInstSrc.* = Instruction{ .Mov = MovInst{
                        .src = mov.src,
                        .dest = Operand{
                            .Reg = mov.type.getR10Variety(),
                        },
                        .type = mov.type,
                    } };
                    movInstDest.* = Instruction{ .Mov = MovInst{
                        .src = Operand{ .Reg = mov.type.getR10Variety() },
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
            .Movzx => |movzx| {
                const movSrcToEax = try allocator.create(Instruction);
                movSrcToEax.* = .{ .Mov = .{
                    .src = movzx.src,
                    .dest = .{ .Reg = Reg.EAX },
                    .type = .LongWord,
                } };
                const movRaxToDest = try allocator.create(Instruction);
                movRaxToDest.* = .{ .Mov = .{
                    .src = .{ .Reg = Reg.RAX },
                    .dest = movzx.dest,
                    .type = .QuadWord,
                } };
                try fixedInstructions.append(movSrcToEax);
                try fixedInstructions.append(movRaxToDest);
            },
            .Cvttsd2si => |cvttsd2si| {
                const movSrcToEax = try allocator.create(Instruction);
                const cvt = try allocator.create(Instruction);
                const movXmmToDest = try allocator.create(Instruction);
                movSrcToEax.* = .{
                    .Mov = .{ .src = cvttsd2si.src, .dest = Operand{ .Reg = Reg.XMM0 }, .type = .QuadWord },
                };
                cvt.* = .{
                    .Cvttsd2si = .{
                        .dest = Operand{ .Reg = cvttsd2si.type.getAXVariety() },
                        .src = Operand{ .Reg = Reg.XMM0 },
                        .type = cvttsd2si.type,
                    },
                };
                movXmmToDest.* = .{
                    .Mov = .{
                        .src = Operand{ .Reg = cvttsd2si.type.getAXVariety() },
                        .dest = cvttsd2si.dest,
                        .type = cvttsd2si.type,
                    },
                };
                try fixedInstructions.append(movSrcToEax);
                try fixedInstructions.append(cvt);
                try fixedInstructions.append(movXmmToDest);
            },
            .Cvtsi2sd => |cvtsi2sd| {
                const movSrcToEax = try allocator.create(Instruction);
                const cvt = try allocator.create(Instruction);
                const movXmmToDest = try allocator.create(Instruction);
                movSrcToEax.* = .{
                    .Mov = .{
                        .src = cvtsi2sd.src,
                        .dest = Operand{ .Reg = cvtsi2sd.type.getAXVariety() },
                        .type = cvtsi2sd.type,
                    },
                };
                cvt.* = .{
                    .Cvtsi2sd = .{
                        .dest = Operand{ .Reg = Reg.XMM0 },
                        .src = Operand{ .Reg = cvtsi2sd.type.getAXVariety() },
                        .type = cvtsi2sd.type,
                    },
                };
                movXmmToDest.* = .{
                    .Mov = .{
                        .src = Operand{ .Reg = Reg.XMM0 },
                        .dest = cvtsi2sd.dest,
                        .type = .QuadWord,
                    },
                };
                try fixedInstructions.append(movSrcToEax);
                try fixedInstructions.append(cvt);
                try fixedInstructions.append(movXmmToDest);
            },
            .Binary => |binary| {
                if (binary.op == BinaryOp.Multiply) {
                    try fixMultiply(inst, allocator, &fixedInstructions);
                    continue;
                }

                const isSrcMem = binary.lhs.isOfKind(.Data) or binary.lhs.isOfKind(.Stack);
                const isDestMem = binary.rhs.isOfKind(.Data) or binary.rhs.isOfKind(.Stack);

                if (isSrcMem and isDestMem) {
                    inst.Binary.lhs = Operand{ .Reg = binary.type.getR10Variety() };
                    try fixedInstructions.appendSlice(&[_]*Instruction{
                        try createInst(.Mov, MovInst{
                            .dest = Operand{ .Reg = binary.type.getR10Variety() },
                            .src = binary.lhs,
                            .type = binary.type,
                        }, allocator),
                        inst,
                        try createInst(.Mov, MovInst{
                            .src = Operand{ .Reg = binary.type.getR10Variety() },
                            .dest = binary.lhs,
                            .type = binary.type,
                        }, allocator),
                    });
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
                            .dest = Operand{
                                .Reg = cmp.type.getR10Variety(),
                            },
                            .type = cmp.type,
                        },
                    };
                    inst.Cmp.op1 = Operand{ .Reg = cmp.type.getR10Variety() };

                    // If cmp is of the comisd variant then the rhs can't be a memory location
                    if (inst.Cmp.type == .Float) {
                        try fixedInstructions.append(try createInst(.Mov, MovInst{
                            .src = cmp.op2,
                            .dest = .{ .Reg = cmp.type.getR11Variety() },
                            .type = cmp.type,
                        }, allocator));
                        inst.Cmp.op2 = .{ .Reg = cmp.type.getR11Variety() };
                    }
                    try fixedInstructions.append(movInst);
                    try fixedInstructions.append(inst);
                    continue;
                } else {
                    // INFO: cmpq doesn't support 64 bit immediates
                    if (inst.Cmp.op2.is(.Imm) and inst.Cmp.type == .QuadWord) {
                        const movRhsToReg = try allocator.create(Instruction);
                        movRhsToReg.* = .{
                            .Mov = .{
                                .src = cmp.op2,
                                .dest = .{ .Reg = cmp.type.getR11Variety() },
                                .type = cmp.type,
                            },
                        };
                        try fixedInstructions.append(movRhsToReg);
                        inst.Cmp.op2 = Operand{ .Reg = cmp.type.getR11Variety() };
                    }

                    if (inst.Cmp.op1.is(.Imm) and inst.Cmp.type == .QuadWord) {
                        const movRhsToReg = try allocator.create(Instruction);
                        movRhsToReg.* = .{
                            .Mov = .{
                                .src = cmp.op1,
                                .dest = .{ .Reg = cmp.type.getR11Variety() },
                                .type = cmp.type,
                            },
                        };
                        try fixedInstructions.append(movRhsToReg);
                        inst.Cmp.op1 = Operand{ .Reg = cmp.type.getR11Variety() };
                    }

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
            .Div => {
                try fixupDiv(
                    inst,
                    allocator,
                    &fixedInstructions,
                );
            },
            // .Unary => |unary| {
            //     if ((unary.op == .Neg) and (unary.type == .Float)) {
            //         try fixedInstructions.appendSlice(&[_]*Instruction{
            //             try createInst(.Mov, MovInst{
            //                 .src = .{ .Imm = 0 },
            //                 .dest = .{.Reg = unary.type.getR10Variety()},
            //                 .type = unary.type,
            //             }, allocator),

            //             try createInst(.Binary, BinaryInst{
            //                 .rhs = unary.rhs,
            //                 .lhs = .{ .Reg = unary.type.getR10Variety()},
            //                 .type = unary.type,
            //                 .op = .Subtract,
            //             }, allocator),
            //             try createInst(.Mov, MovInst{
            //                 .src = .{ .Reg = unary.type.getR10Variety()},
            //                 .dest = unary.rhs,
            //             }, allocator);
            //         });
            //     } else {
            //         try fixedInstructions.append(inst);
            //     }
            // },
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
    switch (instruction.Idiv.src) {
        .Reg => {
            const movIdivArgToR10 = try allocator.create(Instruction);
            movIdivArgToR10.* = Instruction{ .Mov = MovInst{
                .src = instruction.Idiv.src,
                .dest = Operand{ .Reg = Reg.R10 },
                .type = AsmType.LongWord,
            } };
            try fixedInstructions.append(
                movIdivArgToR10,
            );
            const divInstruction = try allocator.create(Instruction);
            divInstruction.* = .{
                .Idiv = .{
                    .src = Operand{ .Reg = Reg.R10 },
                    .type = instruction.Idiv.type,
                },
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
                .Idiv = .{
                    .src = Operand{ .Reg = Reg.R10 },
                    .type = instruction.Idiv.type,
                },
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
                .Idiv = .{
                    .src = Operand{ .Reg = Reg.R10 },
                    .type = instruction.Idiv.type,
                },
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
                .Idiv = .{
                    .src = Operand{ .Reg = Reg.R10 },
                    .type = instruction.Idiv.type,
                },
            };
            try fixedInstructions.append(divInstruction);
        },
    }
}

inline fn fixupDiv(
    instruction: *Instruction,
    allocator: std.mem.Allocator,
    fixedInstructions: *std.ArrayList(*Instruction),
) ast.CodegenError!void {
    switch (instruction.Div.src) {
        .Reg => {
            const movDivArgToR10 = try allocator.create(Instruction);
            movDivArgToR10.* = Instruction{ .Mov = MovInst{
                .src = instruction.Div.src,
                .dest = Operand{ .Reg = Reg.R10 },
                .type = AsmType.LongWord,
            } };
            try fixedInstructions.append(
                movDivArgToR10,
            );
            const divInstruction = try allocator.create(Instruction);
            divInstruction.* = .{
                .Div = .{
                    .src = Operand{ .Reg = Reg.R10 },
                    .type = instruction.Div.type,
                },
            };
            try fixedInstructions.append(divInstruction);
        },
        .Pseudo => {
            unreachable;
        },
        .Imm => |imm| {
            const movDivArgToR10 = try allocator.create(Instruction);
            movDivArgToR10.* = Instruction{ .Mov = MovInst{
                .src = Operand{ .Imm = imm },
                .dest = Operand{ .Reg = Reg.R10 },
                .type = AsmType.LongWord,
            } };
            try fixedInstructions.append(
                movDivArgToR10,
            );
            const divInstruction = try allocator.create(Instruction);
            divInstruction.* = .{
                .Div = .{
                    .src = Operand{ .Reg = Reg.R10 },
                    .type = instruction.Div.type,
                },
            };
            try fixedInstructions.append(divInstruction);
        },
        .Data => |data| {
            const movDivArgToR10 = try allocator.create(Instruction);
            movDivArgToR10.* = Instruction{ .Mov = MovInst{
                .src = Operand{ .Data = data },
                .dest = Operand{ .Reg = Reg.R10 },
                .type = AsmType.LongWord,
            } };
            try fixedInstructions.append(
                movDivArgToR10,
            );
            const divInstruction = try allocator.create(Instruction);
            divInstruction.* = .{
                .Div = .{
                    .src = Operand{ .Reg = Reg.R10 },
                    .type = instruction.Div.type,
                },
            };
            try fixedInstructions.append(divInstruction);
        },
        .Stack => |stack| {
            const movDivArgToR10 = try allocator.create(Instruction);
            movDivArgToR10.* = Instruction{ .Mov = MovInst{
                .src = Operand{ .Stack = stack },
                .dest = Operand{ .Reg = Reg.R10 },
                .type = AsmType.LongWord,
            } };
            try fixedInstructions.append(
                movDivArgToR10,
            );
            const divInstruction = try allocator.create(Instruction);
            divInstruction.* = .{
                .Div = .{
                    .src = Operand{ .Reg = Reg.R10 },
                    .type = instruction.Div.type,
                },
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
            .Cvttsd2si => |cvttsd2si| {
                if (cvttsd2si.src.isOfKind(.Pseudo)) {
                    try replacePseudo(
                        &inst.Cvttsd2si.src,
                        &lookup,
                        @constCast(&asmSymbolTable),
                        &topOfStack,
                    );
                }
                if (cvttsd2si.dest.isOfKind(.Pseudo)) {
                    try replacePseudo(
                        &inst.Cvttsd2si.dest,
                        &lookup,
                        @constCast(&asmSymbolTable),
                        &topOfStack,
                    );
                }
            },
            .Cvtsi2sd => |cvtsi2sd| {
                if (cvtsi2sd.src.isOfKind(.Pseudo)) {
                    try replacePseudo(
                        &inst.Cvtsi2sd.src,
                        &lookup,
                        @constCast(&asmSymbolTable),
                        &topOfStack,
                    );
                }
                if (cvtsi2sd.dest.isOfKind(.Pseudo)) {
                    try replacePseudo(
                        &inst.Cvtsi2sd.dest,
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
                if (idiv.src.isOfKind(.Pseudo)) {
                    try replacePseudo(
                        &inst.Idiv.src,
                        &lookup,
                        @constCast(&asmSymbolTable),
                        &topOfStack,
                    );
                }
            },
            .Div => |div| {
                if (div.src.isOfKind(.Pseudo)) {
                    try replacePseudo(
                        &inst.Div.src,
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
            else => {},
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
            .Float => 8,
        };
        topOfStack.* = topOfStack.* - offset;
        try lookup.put(
            operand.Pseudo,
            topOfStack.*,
        );
        operand.* = Operand{ .Stack = topOfStack.* };
    }
}
