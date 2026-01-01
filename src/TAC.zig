const std = @import("std");
const ast = @import("./AST.zig");
const assembly = @import("./Assembly.zig");
const lexer = @import("./lexer.zig");
const parser = @import("./parser.zig");
const semantic = @import("./semantic.zig");

pub const FnArg = struct {
    name: []u8,
    type: ConstantType,
};
pub const FunctionDef = struct {
    name: []u8,
    global: bool,
    args: std.ArrayList([]u8),
    //TODO: should I be putting type information here? We only have ints for now, so names are fine (FOR NOW)
    instructions: std.ArrayList(*Instruction),
};

pub const StaticVarInit = union(ConstantType) {
    Integer: i32,
    Long: i64,
    UInt: u32,
    ULong: u64,
    Float: f64,
};

pub const StaticVar = struct {
    name: []u8,
    global: bool,
    init: []const StaticVarInit,
    type: []const ConstantType,

    pub fn alignment(self: StaticVar) u8 {
        return switch (self.type[0]) {
            .Integer, .UInt => 4,
            .Long, .ULong, .Float => 8,
        };
    }
};

pub fn astSymTabToTacSymTab(allocator: std.mem.Allocator, astSymTab: std.StringHashMap(*semantic.Symbol)) ast.CodegenError!std.StringHashMap(*assembly.Symbol) {
    var asmSymTab = std.StringHashMap(*assembly.Symbol).init(astSymTab.allocator);
    var astSymTabIter = astSymTab.iterator();
    while (astSymTabIter.next()) |entry| {
        const symName = entry.key_ptr.*;
        const sym = entry.value_ptr.*;
        const asmSymbol = try allocator.create(assembly.Symbol);
        asmSymbol.* = switch (sym.typeInfo) {
            .Integer => .{ .Obj = .{
                .type = .LongWord,
                .static = std.meta.activeTag(sym.attributes) == .StaticAttr,
                .signed = true,
            } },
            .UInteger => .{ .Obj = .{
                .type = .LongWord,
                .static = std.meta.activeTag(sym.attributes) == .StaticAttr,
                .signed = false,
            } },
            .Long => .{ .Obj = .{
                .type = .QuadWord,
                .static = std.meta.activeTag(sym.attributes) == .StaticAttr,
                .signed = true,
            } },
            .ULong => .{ .Obj = .{
                .type = .QuadWord,
                .static = std.meta.activeTag(sym.attributes) == .StaticAttr,
                .signed = false,
            } },
            .Function => .{ .Function = .{
                .defined = sym.attributes.FunctionAttr.defined,
            } },
            .Float => .{ .Obj = .{
                .type = .Float,
                .static = std.meta.activeTag(sym.attributes) == .StaticAttr,
                .signed = false,
            } },
            .Pointer => .{ .Obj = .{
                .type = .QuadWord,
                .static = std.meta.activeTag(sym.attributes) == .StaticAttr,
                .signed = false,
            } },
            .Array => |arrayTy| .{
                .Obj = .{
                    .type = .{
                        .ByteArray = .{
                            // Reserve actual bytes, not element count
                            .size = arrayTy.getNestedSize() * arrayTy.getScalarType().size(),
                            .alignment = arrayTy.getScalarType().alignment(),
                        },
                    },
                    .static = std.meta.activeTag(sym.attributes) == .StaticAttr,
                    .signed = arrayTy.getScalarType().signed(),
                },
            },
            .Void => unreachable,
        };
        try asmSymTab.put(
            @constCast(symName),
            asmSymbol,
        );
    }
    return asmSymTab;
}
pub const AsmRenderer = struct {
    asmSymbolTable: std.StringHashMap(*assembly.Symbol),
    allocator: std.mem.Allocator,
    pub fn init(
        allocator: std.mem.Allocator,
        tacSymTab: std.StringHashMap(*assembly.Symbol),
    ) ast.CodegenError!*AsmRenderer {
        const asmRenderer = try allocator.create(AsmRenderer);
        asmRenderer.* = .{
            .asmSymbolTable = tacSymTab,
            .allocator = allocator,
        };
        return asmRenderer;
    }
    pub fn render(self: *AsmRenderer, program: *Program) !*assembly.Program {
        const topLevelDecls = std.ArrayList(*assembly.TopLevelDecl).init(self.allocator);
        const asmProgram = try self.allocator.create(assembly.Program);
        asmProgram.* = assembly.Program{ .topLevelDecls = topLevelDecls };
        for (program.topLevelDecls.items) |topLevelDecl| {
            const asmTopLevelDecl = try self.allocator.create(assembly.TopLevelDecl);
            switch (topLevelDecl.*) {
                .StaticVar => |statItem| {
                    var staticInitArray = try std.ArrayList(assembly.StaticInit).initCapacity(self.allocator, statItem.init.len);
                    for (statItem.init) |staticVarInit| {
                        staticInitArray.appendAssumeCapacity(switch (staticVarInit) {
                            .Integer => .{ .Integer = staticVarInit.Integer },
                            .Long => .{ .Long = staticVarInit.Long },
                            .UInt => .{ .Integer = @intCast(staticVarInit.UInt) },
                            .ULong => .{ .Long = @intCast(staticVarInit.ULong) },
                            .Float => .{ .Float = staticVarInit.Float },
                        });
                    }
                    const staticVar = try self.allocator.create(assembly.StaticVar);
                    staticVar.* = .{
                        .name = statItem.name,
                        .global = statItem.global,
                        .init = try staticInitArray.toOwnedSlice(),
                        .alignment = statItem.alignment(),
                    };
                    asmTopLevelDecl.* = .{
                        .StaticVar = staticVar,
                    };
                    try asmProgram.topLevelDecls.append(asmTopLevelDecl);
                },
                .Function => |fnItem| {
                    const func = try self.allocator.create(assembly.Function);
                    func.* = .{
                        .name = fnItem.name,
                        .instructions = std.ArrayList(*assembly.Instruction).init(self.allocator),
                        .args = fnItem.args,
                    };
                    const registers32 = [_]assembly.Reg{ assembly.Reg.EDI, assembly.Reg.ESI, assembly.Reg.EDX, assembly.Reg.ECX, assembly.Reg.R8, assembly.Reg.R9 };
                    const registers64 = [_]assembly.Reg{ assembly.Reg.RDI, assembly.Reg.RSI, assembly.Reg.RDX, assembly.Reg.RCX, assembly.Reg.R8, assembly.Reg.R9 };
                    const registersFloat = [_]assembly.Reg{ assembly.Reg.XMM0, assembly.Reg.XMM1 };
                    for (fnItem.args.items, 0..) |arg, i| {
                        const movInstructoin = try self.allocator.create(assembly.Instruction);
                        const resolvedArgSym = self.asmSymbolTable.get(arg).?;
                        movInstructoin.* = assembly.Instruction{
                            .Mov = assembly.MovInst{
                                .type = switch (resolvedArgSym.*) {
                                    .Obj => |obj| obj.type,
                                    else => unreachable,
                                },
                                .src = switch (resolvedArgSym.Obj.type) {
                                    .LongWord => .{ .Reg = registers32[i] },
                                    .QuadWord, .ByteArray => .{ .Reg = registers64[i] },
                                    .Float => .{ .Reg = registersFloat[i] },
                                },
                                .dest = assembly.Operand{
                                    .Pseudo = arg,
                                },
                            },
                        };
                        try func.instructions.append(movInstructoin);
                    }
                    for (fnItem.instructions.items) |instruction| {
                        try instruction.codegen(&self.asmSymbolTable, &func.instructions, self.allocator);
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

pub const Program = struct {
    topLevelDecls: std.ArrayList(*TopLevel),
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
    AddPtr,
    CopyIntoOffset,
    Copy,
    Jump,
    JumpIfZero,
    JumpIfNotZero,
    Label,
    FunctionCall,
    SignExtend,
    Truncate,
    ZeroExtend,
    FloatToUInt,
    FloatToInt,
    IntToFloat,
    UIntToFloat,
    GetAddress,
    Store,
    Load,
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

pub const AddPtr = struct {
    dest: *Val,
    src: *Val,
    index: *Val,
    scale: i64,
};

pub const CopyIntoOffset = struct {
    src: *Val,
    dest: []u8,
    offset: i64,
};

pub fn tempVal() void {}

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

pub const SignExtend = struct {
    src: *Val,
    dest: *Val,
};

pub const Truncate = struct {
    src: *Val,
    dest: *Val,
};

pub const ZeroExtend = struct {
    src: *Val,
    dest: *Val,
};
pub const FloatToUInt = struct {
    src: *Val,
    dest: *Val,
};

pub const FloatToInt = struct {
    src: *Val,
    dest: *Val,
};

pub const IntToFloat = struct {
    src: *Val,
    dest: *Val,
};

pub const UIntToFloat = struct {
    src: *Val,
    dest: *Val,
};

pub inline fn createInst(comptime kind: InstructionType, contents: anytype, allocator: std.mem.Allocator) ast.CodegenError!*Instruction {
    inline for (std.meta.tags(InstructionType)) |tag| {
        if (tag == kind) {
            const inst = try allocator.create(Instruction);
            inst.* = @unionInit(
                Instruction,
                @tagName(kind),
                contents,
            );
            return inst;
        }
    }
    unreachable;
}

pub const GetAddress = struct {
    src: *Val,
    dest: *Val,
};

pub const Store = struct {
    src: *Val,
    destPointer: *Val,
};
pub const Load = struct {
    srcPointer: *Val,
    dest: *Val,
};

pub const Instruction = union(InstructionType) {
    Return: Return,
    Unary: Unary,
    Binary: Binary,
    AddPtr: AddPtr,
    CopyIntoOffset: CopyIntoOffset,
    Copy: Copy,
    Jump: []u8,
    JumpIfZero: Jmp,
    JumpIfNotZero: Jmp,
    Label: []u8,
    FunctionCall: FunctionCall,
    SignExtend: SignExtend,
    Truncate: Truncate,
    ZeroExtend: ZeroExtend,
    FloatToUInt: FloatToUInt,
    FloatToInt: FloatToInt,
    IntToFloat: IntToFloat,
    UIntToFloat: UIntToFloat,
    GetAddress: GetAddress,
    Store: Store,
    Load: Load,

    pub fn format(
        self: Instruction,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        switch (self) {
            .Return => |ret| try writer.print("return {}", .{ret.val.*}),

            .Unary => |unary| try writer.print("{any} = {s}{any}", .{
                unary.dest.*,
                switch (unary.op) {
                    .NEGATE => "-",
                    .COMPLEMENT => "~",
                },
                unary.src.*,
            }),

            .Binary => |binary| try writer.print("{any} = {any} {s} {any}", .{
                binary.dest.*,
                binary.left.*,
                switch (binary.op) {
                    .ADD => "+",
                    .SUBTRACT => "-",
                    .MULTIPLY => "*",
                    .DIVIDE => "/",
                    .REMAINDER => "%",
                    .EQ => "==",
                    .NOT_EQ => "!=",
                    .LT => "<",
                    .LT_EQ => "<=",
                    .GT => ">",
                    .GT_EQ => ">=",
                    .OR => "||",
                    .AND => "&&",
                },
                binary.right.*,
            }),

            .AddPtr => |addPtr| {
                try writer.print("{any} = {any} + {any}*{d}", .{ addPtr.dest.*, addPtr.src.*, addPtr.index.*, addPtr.scale });
            },
            .CopyIntoOffset => |copyIntoOffset| {
                try writer.print("{s}[{any}] = {any}", .{
                    copyIntoOffset.dest,
                    copyIntoOffset.offset,
                    copyIntoOffset.src.*,
                });
            },

            .Copy => |cp| try writer.print("{any} = {any}", .{ cp.dest.*, cp.src.* }),

            .Jump => |label| try writer.print("jmp {s}", .{label}),

            .JumpIfZero => |jmp| try writer.print("jz {s} if {any}", .{ jmp.target, jmp.condition.* }),

            .JumpIfNotZero => |jmp| try writer.print("jnz {s} if {any}", .{ jmp.target, jmp.condition.* }),

            .Label => |label| try writer.print("{s}:", .{label}),

            .FunctionCall => |call| {
                try writer.print("{any} = {s}(", .{ call.dest.*, call.name });
                for (call.args.items, 0..) |arg, i| {
                    if (i > 0) try writer.writeAll(", ");
                    try writer.print("{any}", .{arg.*});
                }
                try writer.writeAll(")");
            },

            .SignExtend => |ext| try writer.print("{any} = sext {any}", .{ ext.dest.*, ext.src.* }),

            .Truncate => |trunc| try writer.print("{any} = trunc {any}", .{ trunc.dest.*, trunc.src.* }),

            .ZeroExtend => |ext| try writer.print("{any} = zext {any}", .{ ext.dest.*, ext.src.* }),

            .FloatToUInt => |conv| try writer.print("{any} = f2u {any}", .{ conv.dest.*, conv.src.* }),

            .FloatToInt => |conv| try writer.print("{any} = f2i {any}", .{ conv.dest.*, conv.src.* }),

            .IntToFloat => |conv| try writer.print("{any} = i2f {any}", .{ conv.dest.*, conv.src.* }),

            .UIntToFloat => |conv| try writer.print("{any} = u2f {any}", .{ conv.dest.*, conv.src.* }),

            .GetAddress => |addr| try writer.print("{any} = &{any}", .{ addr.dest.*, addr.src.* }),

            .Store => |store| try writer.print("*{any} = {any}", .{ store.destPointer.*, store.src.* }),

            .Load => |load| try writer.print("{any} = *{any}", .{ load.dest.*, load.srcPointer.* }),
        }
    }

    pub fn codegen(
        instruction: *Instruction,
        symbolTable: *std.StringHashMap(*assembly.Symbol),
        instructions: *std.ArrayList(*assembly.Instruction),
        allocator: std.mem.Allocator,
    ) ast.CodegenError!void {
        switch (instruction.*) {
            .SignExtend => |signExt| {
                const src = try signExt.src.codegen(symbolTable, allocator);
                const dest = try signExt.dest.codegen(symbolTable, allocator);
                var signExtend = [_]*assembly.Instruction{
                    try assembly.createInst(
                        .Mov,
                        assembly.MovInst{
                            .src = src,
                            .dest = assembly.Operand{ .Reg = assembly.Reg.R10 },
                            .type = signExt.src.getAsmTypeFromSymTab(symbolTable).?,
                        },
                        allocator,
                    ),
                    try assembly.createInst(
                        .Movsx,
                        assembly.Movsx{
                            .src = assembly.Operand{ .Reg = assembly.Reg.R10 },
                            .dest = assembly.Operand{ .Reg = assembly.Reg.R11_64 },
                        },
                        allocator,
                    ),
                    try assembly.createInst(
                        .Mov,
                        assembly.MovInst{
                            .src = assembly.Operand{ .Reg = assembly.Reg.R11_64 },
                            .dest = dest,
                            .type = signExt.dest.getAsmTypeFromSymTab(symbolTable).?,
                        },
                        allocator,
                    ),
                };
                try instructions.appendSlice(&signExtend);
            },
            .Truncate => |trunc| {
                const src = try trunc.src.codegen(symbolTable, allocator);
                const dest = try trunc.dest.codegen(symbolTable, allocator);
                try instructions.append(try assembly.createInst(
                    .Mov,
                    assembly.MovInst{
                        .src = src,
                        .dest = dest,
                        .type = trunc.dest.getAsmTypeFromSymTab(symbolTable).?,
                    },
                    allocator,
                ));
            },
            .ZeroExtend => |zeroExt| {
                const src = try zeroExt.src.codegen(symbolTable, allocator);
                const dest = try zeroExt.dest.codegen(symbolTable, allocator);
                try instructions.append(try assembly.createInst(
                    assembly.InstructionType.Movzx,
                    assembly.Movzx{ .src = src, .dest = dest },
                    allocator,
                ));
            },
            .FloatToUInt => |floatToUInt| {
                const src = try floatToUInt.src.codegen(symbolTable, allocator);
                const dest = try floatToUInt.dest.codegen(symbolTable, allocator);
                const id = ast.tempGen.genId();
                const outOfRangeLabel = try std.fmt.allocPrint(allocator, "outOfRange{}", .{id});
                const longUpperBound = try std.fmt.allocPrint(allocator, "longUpperBound", .{});
                const endLabel = try std.fmt.allocPrint(allocator, "endLabel{}", .{id});
                var floatToUIntInst = [_]*assembly.Instruction{
                    try assembly.createInst(.Cmp, assembly.Cmp{
                        .op2 = src,
                        .op1 = .{ .Data = longUpperBound },
                        .type = .Float,
                    }, allocator),
                    try assembly.createInst(.JmpCC, assembly.JmpCC{
                        .code = .AE,
                        .label = outOfRangeLabel,
                    }, allocator),
                    try assembly.createInst(
                        .Cvttsd2si,
                        assembly.Cvttsd2si{ .dest = dest, .src = src, .type = .QuadWord },
                        allocator,
                    ),
                    try assembly.createInst(.Jmp, endLabel, allocator),
                    try assembly.createInst(.Label, outOfRangeLabel, allocator),
                    try assembly.createInst(.Mov, assembly.MovInst{
                        .src = src,
                        .dest = .{ .Reg = .XMM1 },
                        .type = .Float,
                    }, allocator),
                    try assembly.createInst(.Binary, assembly.BinaryInst{
                        .op = .Subtract,
                        .lhs = .{ .Reg = .XMM1 },
                        .rhs = .{ .Data = longUpperBound },
                        .type = .Float,
                    }, allocator),
                    try assembly.createInst(.Cvttsd2si, assembly.Cvttsd2si{
                        .src = .{ .Reg = .XMM1 },
                        .dest = dest,
                        .type = .QuadWord,
                    }, allocator),
                    try assembly.createInst(.Mov, assembly.MovInst{
                        .type = .QuadWord,
                        .dest = .{ .Reg = .RDX },
                        .src = .{ .Imm = std.math.maxInt(i64) },
                    }, allocator),
                    try assembly.createInst(.Binary, assembly.BinaryInst{
                        .type = .QuadWord,
                        .op = .Add,
                        .lhs = dest,
                        .rhs = .{ .Reg = .RDX },
                    }, allocator),
                    try assembly.createInst(.Label, endLabel, allocator),
                };

                try instructions.appendSlice(&floatToUIntInst);
            },
            .IntToFloat => |intToFloat| {
                std.log.warn("Int to float hit maybe\n", .{});
                const src = try intToFloat.src.codegen(symbolTable, allocator);
                const dest = try intToFloat.dest.codegen(symbolTable, allocator);
                try instructions.append(try assembly.createInst(.Cvtsi2sd, assembly.Cvtsi2sd{
                    .src = src,
                    .dest = dest,
                    .type = intToFloat.src.getAsmTypeFromSymTab(symbolTable).?,
                }, allocator));
            },
            .UIntToFloat => |uIntToFloat| {
                const src = try uIntToFloat.src.codegen(symbolTable, allocator);
                const dest = try uIntToFloat.dest.codegen(symbolTable, allocator);
                const id = ast.tempGen.genId();
                const outOfRangeLabel = try std.fmt.allocPrint(allocator, "outOfRange{}", .{id});
                const endLabel = try std.fmt.allocPrint(allocator, "endLabel{}", .{id});
                const convertInstructions = &[_]*assembly.Instruction{
                    try assembly.createInst(.Cmp, assembly.Cmp{
                        .type = uIntToFloat.src.getAsmTypeFromSymTab(symbolTable).?,
                        .op1 = .{ .Imm = 0 },
                        .op2 = src,
                    }, allocator),
                    try assembly.createInst(.JmpCC, assembly.JmpCC{
                        .code = .L,
                        .label = outOfRangeLabel,
                    }, allocator),
                    try assembly.createInst(.Cvtsi2sd, assembly.Cvtsi2sd{
                        .src = src,
                        .dest = dest,
                        .type = uIntToFloat.src.getAsmTypeFromSymTab(symbolTable).?,
                    }, allocator),
                    try assembly.createInst(.Jmp, endLabel, allocator),
                    try assembly.createInst(.Label, outOfRangeLabel, allocator),
                    try assembly.createInst(.Mov, assembly.MovInst{
                        .src = src,
                        .dest = .{ .Reg = .RAX },
                        .type = .QuadWord,
                    }, allocator),
                    try assembly.createInst(.Unary, assembly.UnaryInst{
                        .rhs = src,
                        .op = .Shr,
                        .type = .QuadWord,
                    }, allocator),
                    try assembly.createInst(.Cvtsi2sd, assembly.Cvtsi2sd{
                        .dest = .{ .Reg = .XMM0 },
                        .src = .{ .Reg = .RAX },
                        .type = .QuadWord,
                    }, allocator),
                    try assembly.createInst(.Binary, assembly.BinaryInst{
                        .lhs = .{ .Reg = .XMM0 },
                        .rhs = .{ .Reg = .XMM0 },
                        .op = .Add,
                        .type = .Float,
                    }, allocator),
                    try assembly.createInst(.Mov, assembly.MovInst{
                        .src = .{ .Reg = .XMM0 },
                        .dest = dest,
                        .type = .Float,
                    }, allocator),
                    try assembly.createInst(.Label, endLabel, allocator),
                };
                try instructions.appendSlice(convertInstructions);
            },
            .FloatToInt => |floatToInt| {
                const src = try floatToInt.src.codegen(symbolTable, allocator);
                const dest = try floatToInt.dest.codegen(symbolTable, allocator);
                try instructions.append(try assembly.createInst(.Cvttsd2si, assembly.Cvttsd2si{
                    .src = src,
                    .dest = dest,
                    .type = floatToInt.dest.getAsmTypeFromSymTab(symbolTable).?,
                }, allocator));
            },

            .Return => |ret| {
                const val = try ret.val.codegen(symbolTable, allocator);
                const instType = ret.val.getAsmTypeFromSymTab(symbolTable).?;
                var returnInstructions = [_]*assembly.Instruction{
                    try assembly.createInst(.Mov, assembly.MovInst{
                        .type = instType,
                        .src = val,
                        .dest = assembly.Operand{
                            .Reg = instType.getAXVariety(),
                        },
                    }, allocator),
                    try assembly.createInst(.Ret, {}, allocator),
                };
                try instructions.appendSlice(&returnInstructions);
            },
            .Unary => |unary| {
                const dest = try unary.dest.codegen(symbolTable, allocator);
                const src = try unary.src.codegen(symbolTable, allocator);
                const destType = unary.dest.getAsmTypeFromSymTab(symbolTable).?;
                if (destType == .Float) {
                    const negZero = "negZero";
                    try instructions.appendSlice(&[_]*assembly.Instruction{
                        try assembly.createInst(.Mov, assembly.MovInst{
                            .src = src,
                            .dest = dest,
                            .type = destType,
                        }, allocator),
                        try assembly.createInst(.Binary, assembly.BinaryInst{
                            .lhs = dest,
                            .rhs = .{ .Data = @constCast(negZero) },
                            .type = destType,
                            .op = .Xor,
                        }, allocator),
                    });
                    return;
                }
                var unaryInstructions = [_]*assembly.Instruction{
                    try assembly.createInst(.Mov, assembly.MovInst{
                        .type = destType,
                        .src = src,
                        .dest = dest,
                    }, allocator),
                    switch (unary.op) {
                        .NEGATE => try assembly.createInst(.Unary, assembly.UnaryInst{
                            .op = assembly.UnaryOp.Neg,
                            .rhs = dest,
                            .type = destType,
                        }, allocator),
                        .COMPLEMENT => try assembly.createInst(.Unary, assembly.UnaryInst{
                            .op = assembly.UnaryOp.Not,
                            .rhs = dest,
                            .type = destType,
                        }, allocator),
                    },
                };
                try instructions.appendSlice(&unaryInstructions);
            },
            .Binary => |binary| {
                const storeDest = try binary.dest.codegen(symbolTable, allocator);
                const storeDestType = binary.dest.getAsmTypeFromSymTab(symbolTable).?;
                const left = try binary.left.codegen(symbolTable, allocator);
                const right = try binary.right.codegen(symbolTable, allocator);
                switch (binary.op) {
                    .ADD, .SUBTRACT, .MULTIPLY => {
                        var binInstructions = [_]*assembly.Instruction{
                            try assembly.createInst(.Mov, assembly.MovInst{
                                .src = left,
                                .dest = storeDest,
                                .type = storeDestType,
                            }, allocator),
                            try assembly.createInst(.Binary, assembly.BinaryInst{
                                .lhs = storeDest,
                                .rhs = right,
                                .op = tacOpToAssemblyOp(binary.op),
                                .type = storeDestType,
                            }, allocator),
                        };
                        try instructions.appendSlice(&binInstructions);
                    },
                    .DIVIDE => {
                        if (storeDestType == .Float) {
                            try instructions.appendSlice(&[_]*assembly.Instruction{ try assembly.createInst(.Mov, assembly.MovInst{
                                .dest = storeDest,
                                .src = left,
                                .type = storeDestType,
                            }, allocator), try assembly.createInst(.Binary, assembly.BinaryInst{
                                .lhs = storeDest,
                                .rhs = right,
                                .type = storeDestType,
                                .op = .Divide,
                            }, allocator) });
                            return;
                        }
                        //INFO: non double divs
                        try instructions.append(try assembly.createInst(.Mov, assembly.MovInst{
                            .src = left,
                            .dest = assembly.Operand{
                                .Reg = binary.left.getAsmTypeFromSymTab(symbolTable).?.getAXVariety(),
                            },
                            .type = binary.left.getAsmTypeFromSymTab(symbolTable).?,
                        }, allocator));

                        const signed = binary.left.isSignedFromSymTab(symbolTable) and binary.right.isSignedFromSymTab(symbolTable);

                        if (signed) {
                            var signedDivide = [_]*assembly.Instruction{
                                try assembly.createInst(
                                    .Cdq,
                                    storeDestType,
                                    allocator,
                                ),
                                try assembly.createInst(.Idiv, assembly.Idiv{
                                    .src = right,
                                    .type = storeDestType,
                                }, allocator),
                            };
                            try instructions.appendSlice(&signedDivide);
                        } else {
                            var unsignedDivide = [_]*assembly.Instruction{
                                try assembly.createInst(.Mov, assembly.MovInst{
                                    .src = .{ .Imm = 0 },
                                    .dest = .{
                                        .Reg = binary.left.getAsmTypeFromSymTab(symbolTable).?.getDXVariety(),
                                    },
                                    .type = binary.left.getAsmTypeFromSymTab(symbolTable).?,
                                }, allocator),
                                try assembly.createInst(.Div, assembly.Idiv{
                                    .src = right,
                                    .type = storeDestType,
                                }, allocator),
                            };
                            try instructions.appendSlice(&unsignedDivide);
                        }
                        try instructions.append(try assembly.createInst(.Mov, assembly.MovInst{
                            .src = assembly.Operand{
                                .Reg = storeDestType.getAXVariety(),
                            },
                            .dest = storeDest,
                            .type = storeDestType,
                        }, allocator));
                    },
                    .REMAINDER => {
                        var remainderInst = [_]*assembly.Instruction{
                            try assembly.createInst(.Mov, assembly.MovInst{
                                .src = left,
                                .dest = assembly.Operand{
                                    .Reg = binary.left.getAsmTypeFromSymTab(symbolTable).?.getAXVariety(),
                                },
                                .type = binary.left.getAsmTypeFromSymTab(symbolTable).?,
                            }, allocator),
                            try assembly.createInst(.Cdq, storeDestType, allocator),
                            try assembly.createInst(.Idiv, assembly.Idiv{ .src = right, .type = storeDestType }, allocator),
                            try assembly.createInst(.Mov, assembly.MovInst{
                                .src = assembly.Operand{ .Reg = storeDestType.getDXVariety() },
                                .dest = storeDest,
                                .type = storeDestType,
                            }, allocator),
                        };
                        try instructions.appendSlice(&remainderInst);
                    },
                    .EQ, .NOT_EQ, .LT, .LT_EQ, .GT, .GT_EQ => {
                        const lhsSigned = binary.left.isSignedFromSymTab(symbolTable);
                        //const rhsSigned = binary.right.isSignedFromSymTab(symbolTable);
                        //if (lhsSigned != rhsSigned) {
                        //    // This branch gets hit when comparing a signed and
                        //    // unsigned number, in that case we can just
                        //    // copmare them normally
                        //    std.log.warn("lhsSigned: {any}\n", .{lhsSigned});
                        //    std.log.warn("rhsSigned: {any}\n", .{rhsSigned});
                        //    std.log.err("binary: {any}\n", .{binary});
                        //    unreachable;
                        //}
                        // FIX: This should be changed in the instruction fixup pass and not here
                        var comparisionInst = [_]*assembly.Instruction{
                            try assembly.createInst(.Mov, assembly.MovInst{
                                .src = left,
                                .dest = .{ .Reg = binary.left.getAsmTypeFromSymTab(symbolTable).?.getR10Variety() },
                                .type = binary.left.getAsmTypeFromSymTab(symbolTable).?,
                            }, allocator),
                            try assembly.createInst(.Cmp, assembly.Cmp{
                                .op1 = right,
                                .op2 = .{ .Reg = binary.left.getAsmTypeFromSymTab(symbolTable).?.getR10Variety() },
                                .type = binary.left.getAsmTypeFromSymTab(symbolTable).?,
                            }, allocator),
                            try assembly.createInst(.SetCC, assembly.SetCC{
                                .code = assembly.CondCode.getFromTacOp(binary.op, lhsSigned, binary.left.getAsmTypeFromSymTab(symbolTable).? == .Float),
                                .dest = storeDest,
                            }, allocator),
                        };
                        try instructions.appendSlice(&comparisionInst);
                    },
                    .OR => {
                        unreachable;
                    },
                    .AND => {
                        unreachable;
                    },
                }
            },
            .AddPtr => |addPtr| {
                const src = try addPtr.src.codegen(symbolTable, allocator);
                const dest = try addPtr.dest.codegen(symbolTable, allocator);
                const index = try addPtr.index.codegen(symbolTable, allocator);
                const indexType = addPtr.index.getAsmTypeFromSymTab(symbolTable).?;

                const destType = addPtr.dest.getAsmTypeFromSymTab(symbolTable).?;
                const r8 = try allocator.create(assembly.Operand);
                r8.* = .{ .Reg = .R8_64 };
                const r9 = try allocator.create(assembly.Operand);
                r9.* = .{ .Reg = .R9_64 };

                // Choose LEA to load the base only when it's a data label; for
                // stack/pseudo or register values, MOV loads the pointer value.
                if (std.meta.activeTag(src) == .Data) {
                    try instructions.append(try assembly.createInst(.Lea, assembly.Lea{
                        .src = src,
                        .dest = r9.*,
                        .type = .QuadWord,
                    }, allocator));
                } else {
                    try instructions.append(try assembly.createInst(.Mov, assembly.MovInst{
                        .src = src,
                        .dest = r9.*,
                        .type = .QuadWord,
                    }, allocator));
                }

                // Load index into r8 of appropriate width
                try instructions.append(try assembly.createInst(.Mov, assembly.MovInst{
                    .src = index,
                    .dest = .{ .Reg = indexType.getR8Variety() },
                    .type = indexType,
                }, allocator));

                // If scale is supported directly by x86-64 addressing (1,2,4,8),
                // use a single LEA. Otherwise, decompose the scale into
                // multiple LEAs with allowed factors and accumulate into a temp
                // register before storing to dest.
                const scale_val: i64 = addPtr.scale;
                if (scale_val == 1 or scale_val == 2 or scale_val == 4 or scale_val == 8) {
                    try instructions.append(try assembly.createInst(.Lea, assembly.Lea{
                        .src = .{ .Indexed = .{ .base = .R9_64, .index = indexType.getR8Variety(), .scale = scale_val } },
                        .dest = dest,
                        .type = destType,
                    }, allocator));
                } else {
                    // Accumulate address into R10 to avoid mem-dest LEA and scale>8 issues
                    const r10op = assembly.Operand{ .Reg = .R10_64 };
                    // r10 = base
                    try instructions.append(try assembly.createInst(.Mov, assembly.MovInst{
                        .src = .{ .Reg = .R9_64 },
                        .dest = r10op,
                        .type = .QuadWord,
                    }, allocator));

                    var remaining: i64 = scale_val;
                    inline for (.{ 8, 4, 2, 1 }) |piece| {
                        while (remaining >= piece) {
                            try instructions.append(try assembly.createInst(.Lea, assembly.Lea{
                                .src = .{ .Indexed = .{ .base = .R10_64, .index = indexType.getR8Variety(), .scale = piece } },
                                .dest = r10op,
                                .type = .QuadWord,
                            }, allocator));
                            remaining -= piece;
                        }
                    }

                    // Finally move r10 into destination location
                    try instructions.append(try assembly.createInst(.Mov, assembly.MovInst{
                        .src = r10op,
                        .dest = dest,
                        .type = .QuadWord,
                    }, allocator));
                }
            },
            .CopyIntoOffset => |copyIntoOffset| {
                const src = try copyIntoOffset.src.codegen(symbolTable, allocator);

                const srcType = copyIntoOffset.src.getAsmTypeFromSymTab(symbolTable).?;
                //const r8 = try allocator.create(assembly.Operand);
                //r8.* = .{ .Reg = .R8_64 };
                //const r9 = try allocator.create(assembly.Operand);
                //r9.* = .{ .Reg = .R9_64 };
                // INFO:
                // 1. First move src to r8
                // 2. Move the (dest, index, offset) to r9
                // 3. Move r8 to &r9

                try instructions.append(
                    try assembly.createInst(
                        .Mov,
                        assembly.MovInst{
                            .src = src,
                            .dest = .{ .PseudoMem = .{
                                .name = copyIntoOffset.dest,
                                .offset = copyIntoOffset.offset,
                            } },
                            .type = srcType,
                        },
                        allocator,
                    ),
                );
            },
            .Copy => |cp| {
                const src = try cp.src.codegen(symbolTable, allocator);
                const dest = try cp.dest.codegen(symbolTable, allocator);
                const destType = cp.dest.getAsmTypeFromSymTab(symbolTable).?;
                try instructions.append(try assembly.createInst(.Mov, assembly.MovInst{
                    .src = src,
                    .dest = dest,
                    .type = destType,
                }, allocator));
            },
            .Jump => |jmp| {
                try instructions.append(try assembly.createInst(.Jmp, jmp, allocator));
            },
            .JumpIfZero => |jmp| {
                const val = try jmp.condition.codegen(symbolTable, allocator);
                var jmpIfZeroInst = [_]*assembly.Instruction{
                    try assembly.createInst(.Mov, assembly.MovInst{
                        .src = val,
                        .dest = assembly.Operand{
                            .Reg = jmp.condition.getAsmTypeFromSymTab(symbolTable).?.getR10Variety(),
                        },
                        .type = jmp.condition.getAsmTypeFromSymTab(symbolTable).?,
                    }, allocator),
                    try assembly.createInst(.Mov, assembly.MovInst{
                        .src = val,
                        .dest = assembly.Operand{
                            .Reg = jmp.condition.getAsmTypeFromSymTab(symbolTable).?.getR10Variety(),
                        },
                        .type = jmp.condition.getAsmTypeFromSymTab(symbolTable).?,
                    }, allocator),
                    try assembly.createInst(.Cmp, assembly.Cmp{
                        .op2 = assembly.Operand{
                            .Reg = jmp.condition.getAsmTypeFromSymTab(symbolTable).?.getR10Variety(),
                        },
                        .op1 = assembly.Operand{ .Imm = 0 },
                        .type = jmp.condition.getAsmTypeFromSymTab(symbolTable).?,
                    }, allocator),
                    try assembly.createInst(.JmpCC, assembly.JmpCC{
                        .code = assembly.CondCode.E,
                        .label = jmp.target,
                    }, allocator),
                };
                try instructions.appendSlice(&jmpIfZeroInst);
            },
            .JumpIfNotZero => |jmp| {
                const val = try jmp.condition.codegen(symbolTable, allocator);
                var jmpIfNotZero = [_]*assembly.Instruction{
                    try assembly.createInst(.Mov, assembly.MovInst{
                        .src = val,
                        .dest = assembly.Operand{
                            .Reg = jmp.condition.getAsmTypeFromSymTab(symbolTable).?.getR10Variety(),
                        },
                        .type = jmp.condition.getAsmTypeFromSymTab(symbolTable).?,
                    }, allocator),
                    try assembly.createInst(.Cmp, assembly.Cmp{
                        .op2 = assembly.Operand{
                            .Reg = jmp.condition.getAsmTypeFromSymTab(symbolTable).?.getR10Variety(),
                        },
                        .op1 = assembly.Operand{
                            .Imm = 0,
                        },
                        .type = jmp.condition.getAsmTypeFromSymTab(symbolTable).?,
                    }, allocator),
                    try assembly.createInst(.JmpCC, assembly.JmpCC{
                        .code = assembly.CondCode.NE,
                        .label = jmp.target,
                    }, allocator),
                };
                try instructions.appendSlice(&jmpIfNotZero);
            },
            .Label => |labelName| {
                try instructions.append(try assembly.createInst(.Label, labelName, allocator));
            },
            .GetAddress => |getAddr| {
                const src = try getAddr.src.codegen(symbolTable, allocator);
                const dest = try getAddr.dest.codegen(symbolTable, allocator);
                try instructions.append(try assembly.createInst(.Lea, assembly.Lea{
                    .type = .QuadWord,
                    .src = src,
                    .dest = dest,
                }, allocator));
            },
            .FunctionCall => |fnCall| {
                const registers32 = [_]assembly.Reg{
                    assembly.Reg.EDI,
                    assembly.Reg.ESI,
                    assembly.Reg.EDX,
                    assembly.Reg.ECX,
                    assembly.Reg.R8,
                    assembly.Reg.R9,
                };
                const registers64 = [_]assembly.Reg{
                    assembly.Reg.RDI,
                    assembly.Reg.RSI,
                    assembly.Reg.RDX,
                    assembly.Reg.RCX,
                    assembly.Reg.R8_64,
                    assembly.Reg.R9_64,
                };
                const registersFloat = [_]assembly.Reg{ assembly.Reg.XMM0, assembly.Reg.XMM1 };
                var floatArgsCount: u64 = 0;
                if (fnCall.args.items.len < 6) {
                    for (fnCall.args.items, 0..) |arg, i| {
                        const assemblyArg = try arg.codegen(symbolTable, allocator);
                        const assemblyArgType = arg.getAsmTypeFromSymTab(symbolTable).?;
                        try instructions.append(try assembly.createInst(.Mov, assembly.MovInst{
                            .src = assemblyArg,
                            .dest = switch (assemblyArgType) {
                                .LongWord => .{ .Reg = registers32[i] },
                                .QuadWord => .{ .Reg = registers64[i] },
                                .Float => blk: {
                                    floatArgsCount += 1;
                                    break :blk .{ .Reg = registersFloat[i] };
                                },
                                .ByteArray => unreachable,
                            },
                            .type = assemblyArgType,
                        }, allocator));
                    }
                    const asmDest = try fnCall.dest.codegen(symbolTable, allocator);
                    try instructions.append(try assembly.createInst(
                        .Mov,
                        assembly.MovInst{
                            .src = .{ .Imm = floatArgsCount },
                            .type = .LongWord,
                            .dest = .{ .Reg = .EAX },
                        },
                        allocator,
                    ));
                    try instructions.append(try assembly.createInst(.FnCall, assembly.FnCall{ .name = fnCall.name }, allocator));
                    try instructions.append(try assembly.createInst(.Mov, assembly.MovInst{
                        .dest = asmDest,
                        .src = assembly.Operand{
                            .Reg = fnCall.dest.getAsmTypeFromSymTab(symbolTable).?.getAXVariety(),
                        },
                        .type = fnCall.dest.getAsmTypeFromSymTab(symbolTable).?,
                    }, allocator));
                } else {
                    // TODO: People should be taxed for using more than six
                    // arguments
                    unreachable();
                }
            },
            .Store => |store| {
                const src = try store.src.codegen(symbolTable, allocator);
                const destPointer = try store.destPointer.codegen(symbolTable, allocator);
                const moveToRAX = try allocator.create(assembly.Instruction);
                const derefMove = try allocator.create(assembly.Instruction);
                const rax = try allocator.create(assembly.Operand);
                rax.* = .{ .Reg = .RAX };
                moveToRAX.* = .{ .Mov = .{
                    .type = .QuadWord,
                    .src = destPointer,
                    .dest = rax.*,
                } };
                derefMove.* = .{ .Mov = .{
                    .type = store.src.getAsmTypeFromSymTab(symbolTable).?,
                    .dest = .{
                        .Memory = .{
                            .base = rax,
                            .index = 0,
                        },
                    },
                    .src = src,
                } };
                try instructions.appendSlice(&[_]*assembly.Instruction{
                    moveToRAX,
                    derefMove,
                });
            },
            .Load => |load| {
                const srcPointer = try load.srcPointer.codegen(symbolTable, allocator);
                const dest = try load.dest.codegen(symbolTable, allocator);
                const moveToRAX = try allocator.create(assembly.Instruction);
                const derefMove = try allocator.create(assembly.Instruction);
                const rax = try allocator.create(assembly.Operand);
                rax.* = .{ .Reg = .RAX };
                moveToRAX.* = .{ .Mov = .{
                    .type = .QuadWord,
                    .src = srcPointer,
                    .dest = rax.*,
                } };
                derefMove.* = .{ .Mov = .{
                    .type = load.dest.getAsmTypeFromSymTab(symbolTable).?,
                    .dest = dest,
                    .src = .{ .Memory = .{ .base = rax, .index = 0 } },
                } };
                try instructions.appendSlice(&[_]*assembly.Instruction{
                    moveToRAX,
                    derefMove,
                });
            },
        }
    }
};

pub const ValType = enum {
    Constant,
    Variable,
};

pub const ConstantType = enum {
    Integer,
    Long,
    UInt,
    ULong,
    Float,
};

pub const Constant = union(ConstantType) {
    Integer: i32,
    Long: i64,
    UInt: u32,
    ULong: u64,
    Float: f64,
};

pub const BoxedVal = union(enum) {
    // INFO: With pointers, there are some edge cases during assignment, see
    // notes.md
    PlainVal: *Val,
    DerefedVal: *Val,
};

pub const Val = union(ValType) {
    Constant: Constant,
    Variable: []u8,

    const Self = @This();

    pub fn format(
        self: Val,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        // Format values with clear type indicators
        switch (self) {
            .Constant => |constant| switch (constant) {
                .Integer => |i| try writer.print("{d}", .{i}),
                .Long => |l| try writer.print("{d}L", .{l}),
                .UInt => |u| try writer.print("{d}u", .{u}),
                .ULong => |ul| try writer.print("{d}uL", .{ul}),
                .Float => |f| try writer.print("{d:.6}f", .{f}),
            },
            .Variable => |name| try writer.print("%{s}", .{name}),
        }
    }

    pub fn isSignedFromSymTab(self: *Self, symbolTable: *std.StringHashMap(*assembly.Symbol)) bool {
        return switch (self.*) {
            .Constant => |constant| switch (constant) {
                .Long, .Integer => true,
                .ULong, .UInt => false,
                .Float => false,
            },
            .Variable => |varName| if (symbolTable.get(varName)) |sym| sym.Obj.signed else {
                std.log.warn("Not found: {s}\n", .{varName});
                unreachable;
            },
        };
    }
    pub fn getAsmTypeFromSymTab(self: *Self, symbolTable: *std.StringHashMap(*assembly.Symbol)) ?assembly.AsmType {
        return switch (self.*) {
            .Constant => |constant| switch (constant) {
                .Long, .ULong => assembly.AsmType.QuadWord,
                .Integer, .UInt => assembly.AsmType.LongWord,
                .Float => assembly.AsmType.Float,
            },
            .Variable => |varName| {
                return if (symbolTable.get(varName)) |sym| sym.Obj.type else blk: {
                    std.log.warn("Not found: {s}\n", .{varName});
                    break :blk null;
                };
            },
        };
    }
    pub fn codegen(val: *Val, symbolTable: *std.StringHashMap(*assembly.Symbol), allocator: std.mem.Allocator) ast.CodegenError!assembly.Operand {
        switch (val.*) {
            .Constant => |constant| {
                const operand = try allocator.create(assembly.Operand);
                operand.* = .{
                    .Imm = (switch (constant) {
                        .Integer => @intCast(constant.Integer),
                        .Long => @intCast(constant.Long),
                        .ULong => @intCast(constant.ULong),
                        .UInt => @intCast(constant.UInt),
                        else => unreachable,
                    }),
                };
                return operand.*;
            },
            .Variable => |variable| {
                const operand = try allocator.create(assembly.Operand);
                if (symbolTable.get(variable)) |sym| {
                    switch (sym.*) {
                        .Obj => |obj| {
                            return (if (obj.static) ifblk: {
                                operand.* = .{ .Data = variable };
                                break :ifblk operand.*;
                            } else elseblk: {
                                operand.* = .{
                                    .Pseudo = variable,
                                };
                                break :elseblk operand.*;
                            });
                        },
                        else => unreachable,
                    }
                }
                operand.* = assembly.Operand{
                    .Pseudo = variable,
                };
                return operand.*;
            },
        }
    }
};
