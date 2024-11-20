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

pub const StaticVar = struct {
    name: []u8,
    global: bool,
    init: union(ConstantType) {
        Integer: i32,
        Long: i64,
        UInt: u32,
        ULong: u64,
        Float: f64,
    },
    type: ConstantType,
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
    pub fn init(allocator: std.mem.Allocator, tacSymTab: std.StringHashMap(*assembly.Symbol)) ast.CodegenError!*AsmRenderer {
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
                    const staticVar = try self.allocator.create(assembly.StaticVar);
                    staticVar.* = .{
                        .name = statItem.name,
                        .global = statItem.global,
                        .init = (switch (statItem.type) {
                            .Integer => .{ .Integer = statItem.init.Integer },
                            .Long => .{ .Long = statItem.init.Long },
                            .UInt => .{ .Integer = @intCast(statItem.init.UInt) },
                            .ULong => .{ .Long = @intCast(statItem.init.ULong) },
                            .Float => .{ .Float = statItem.init.Float },
                        }),
                        .alignment = switch (statItem.type) {
                            .Integer, .UInt => 4,
                            .Long, .ULong, .Float => 8,
                        },
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
                    const registers = [_]assembly.Reg{ assembly.Reg.EDI, assembly.Reg.ESI, assembly.Reg.EDX, assembly.Reg.ECX, assembly.Reg.R8, assembly.Reg.R9 };
                    for (fnItem.args.items, 0..) |arg, i| {
                        const movInstructoin = try self.allocator.create(assembly.Instruction);
                        const resolvedArgSym = self.asmSymbolTable.get(arg).?;
                        movInstructoin.* = assembly.Instruction{ .Mov = assembly.MovInst{
                            .type = switch (resolvedArgSym.*) {
                                .Obj => |obj| obj.type,
                                else => unreachable,
                            },
                            .src = assembly.Operand{ .Reg = registers[i] },
                            .dest = assembly.Operand{ .Pseudo = arg },
                        } };
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
    SignExtend: SignExtend,
    Truncate: Truncate,
    ZeroExtend: ZeroExtend,
    FloatToUInt: FloatToUInt,
    FloatToInt: FloatToInt,
    IntToFloat: IntToFloat,
    UIntToFloat: UIntToFloat,

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
                const movsxInst = try allocator.create(assembly.Instruction);
                const movSrcToR10 = try allocator.create(assembly.Instruction);
                movSrcToR10.* = assembly.Instruction{ .Mov = assembly.MovInst{
                    .src = src,
                    .dest = assembly.Operand{ .Reg = assembly.Reg.R10 },
                    .type = signExt.src.getAsmTypeFromSymTab(symbolTable).?,
                } };
                const movR11ToDest = try allocator.create(assembly.Instruction);
                try instructions.append(movSrcToR10);
                movsxInst.* = assembly.Instruction{ .Movsx = assembly.Movsx{
                    .src = assembly.Operand{ .Reg = assembly.Reg.R10 },
                    .dest = assembly.Operand{ .Reg = assembly.Reg.R11_64 },
                } };
                try instructions.append(movsxInst);
                movR11ToDest.* = assembly.Instruction{ .Mov = assembly.MovInst{
                    .src = assembly.Operand{ .Reg = assembly.Reg.R11_64 },
                    .dest = dest,
                    .type = signExt.dest.getAsmTypeFromSymTab(symbolTable).?,
                } };
                try instructions.append(movR11ToDest);
            },
            .Truncate => |trunc| {
                const src = try trunc.src.codegen(symbolTable, allocator);
                const dest = try trunc.dest.codegen(symbolTable, allocator);
                const movInstruction = try allocator.create(assembly.Instruction);
                movInstruction.* = assembly.Instruction{ .Mov = assembly.MovInst{
                    .src = src,
                    .dest = dest,
                    .type = trunc.dest.getAsmTypeFromSymTab(symbolTable).?,
                } };
                try instructions.append(movInstruction);
            },
            .ZeroExtend => |zeroExt| {
                const src = try zeroExt.src.codegen(symbolTable, allocator);
                const dest = try zeroExt.dest.codegen(symbolTable, allocator);
                const movZeroExtend = try allocator.create(assembly.Instruction);
                movZeroExtend.* = .{
                    .Movzx = .{
                        .src = src,
                        .dest = dest,
                    },
                };
                try instructions.append(movZeroExtend);
            },
            .FloatToUInt, .IntToFloat, .UIntToFloat => {},
            .FloatToInt => |floatToInt| {
                const src = try floatToInt.src.codegen(symbolTable, allocator);
                const dest = try floatToInt.dest.codegen(symbolTable, allocator);
                const cvttsd2si = try allocator.create(assembly.Instruction);
                cvttsd2si.* = .{
                    .Cvttsd2si = .{
                        .src = src,
                        .dest = dest,
                        .type = floatToInt.dest.getAsmTypeFromSymTab(symbolTable).?,
                    },
                };
                try instructions.append(cvttsd2si);
            },
            .Return => |ret| {
                const val = try ret.val.codegen(symbolTable, allocator);
                const movInst = try allocator.create(assembly.Instruction);
                const instType = ret.val.getAsmTypeFromSymTab(symbolTable).?;
                movInst.* = assembly.Instruction{ .Mov = assembly.MovInst{
                    .type = instType,
                    .src = val,
                    .dest = assembly.Operand{
                        .Reg = instType.getAXVariety(),
                    },
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
                const destType = unary.dest.getAsmTypeFromSymTab(symbolTable).?;
                movInst.* = assembly.Instruction{
                    .Mov = assembly.MovInst{
                        .type = destType,
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
                                .type = destType,
                            },
                        };
                    },
                    .COMPLEMENT => {
                        unaryInst.* = assembly.Instruction{
                            .Unary = assembly.UnaryInst{
                                .op = assembly.UnaryOp.Not,
                                .rhs = dest,
                                .type = destType,
                            },
                        };
                    },
                }
                try instructions.append(movInst);
                try instructions.append(unaryInst);
            },
            .Binary => |binary| {
                const storeDest = try binary.dest.codegen(symbolTable, allocator);
                const storeDestType = binary.dest.getAsmTypeFromSymTab(symbolTable).?;
                const left = try binary.left.codegen(symbolTable, allocator);
                const right = try binary.right.codegen(symbolTable, allocator);
                switch (binary.op) {
                    .ADD, .SUBTRACT, .MULTIPLY => {
                        const movLeftToDest = try allocator.create(assembly.Instruction);
                        movLeftToDest.* = assembly.Instruction{ .Mov = assembly.MovInst{
                            .src = left,
                            .dest = storeDest,
                            .type = storeDestType,
                        } };
                        const binaryInstr = try allocator.create(assembly.Instruction);
                        binaryInstr.* = assembly.Instruction{ .Binary = assembly.BinaryInst{
                            .lhs = storeDest,
                            .rhs = right,
                            .op = tacOpToAssemblyOp(binary.op),
                            .type = storeDestType,
                        } };
                        try instructions.append(movLeftToDest);
                        try instructions.append(binaryInstr);
                    },
                    .DIVIDE => {
                        const movLeftToAX = try allocator.create(assembly.Instruction);
                        const movAXToDest = try allocator.create(assembly.Instruction);
                        movLeftToAX.* = assembly.Instruction{ .Mov = assembly.MovInst{
                            .src = left,
                            .dest = assembly.Operand{
                                .Reg = binary.left.getAsmTypeFromSymTab(symbolTable).?.getAXVariety(),
                            },
                            .type = binary.left.getAsmTypeFromSymTab(symbolTable).?,
                        } };
                        try instructions.append(movLeftToAX);
                        if (binary.left.isSignedFromSymTab(symbolTable) and binary.right.isSignedFromSymTab(symbolTable)) {
                            const cdqInstr = try allocator.create(assembly.Instruction);
                            const idivWithRight = try allocator.create(assembly.Instruction);
                            cdqInstr.* = assembly.Instruction{
                                .Cdq = storeDestType,
                            };
                            idivWithRight.* = assembly.Instruction{
                                .Idiv = .{
                                    .src = right,
                                    .type = storeDestType,
                                },
                            };
                            try instructions.append(cdqInstr);
                            try instructions.append(idivWithRight);
                        } else {
                            const zeroEdx = try allocator.create(assembly.Instruction);
                            const divWithRight = try allocator.create(assembly.Instruction);
                            zeroEdx.* = .{
                                .Mov = .{
                                    .src = .{ .Imm = 0 },
                                    .dest = .{
                                        .Reg = binary.left.getAsmTypeFromSymTab(symbolTable).?.getDXVariety(),
                                    },
                                    .type = binary.left.getAsmTypeFromSymTab(symbolTable).?,
                                },
                            };
                            divWithRight.* = assembly.Instruction{
                                .Div = .{
                                    .src = right,
                                    .type = storeDestType,
                                },
                            };
                            try instructions.append(zeroEdx);
                            try instructions.append(divWithRight);
                        }
                        movAXToDest.* = assembly.Instruction{ .Mov = assembly.MovInst{
                            .src = assembly.Operand{
                                .Reg = storeDestType.getAXVariety(),
                            },
                            .dest = storeDest,
                            .type = storeDestType,
                        } };
                        try instructions.append(movAXToDest);
                    },
                    .REMAINDER => {
                        const movLeftToDX = try allocator.create(assembly.Instruction);
                        const cdqInstr = try allocator.create(assembly.Instruction);
                        const idivWithRight = try allocator.create(assembly.Instruction);
                        const movDXToDest = try allocator.create(assembly.Instruction);
                        movLeftToDX.* = assembly.Instruction{ .Mov = assembly.MovInst{
                            .src = left,
                            .dest = assembly.Operand{
                                .Reg = binary.left.getAsmTypeFromSymTab(symbolTable).?.getAXVariety(),
                            },
                            .type = binary.left.getAsmTypeFromSymTab(symbolTable).?,
                        } };
                        cdqInstr.* = assembly.Instruction{
                            .Cdq = storeDestType,
                        };
                        idivWithRight.* = assembly.Instruction{
                            .Idiv = .{ .src = right, .type = storeDestType },
                        };
                        movDXToDest.* = assembly.Instruction{ .Mov = assembly.MovInst{
                            .src = assembly.Operand{ .Reg = storeDestType.getDXVariety() },
                            .dest = storeDest,
                            .type = storeDestType,
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
                            .type = storeDestType,
                        } };
                        cmpInstr.* = assembly.Instruction{ .Cmp = assembly.Cmp{
                            .op1 = right,
                            .op2 = storeDest,
                            .type = storeDestType,
                        } };
                        const setCC = try allocator.create(assembly.Instruction);
                        const lhsSigned = binary.left.isSignedFromSymTab(symbolTable);
                        const rhsSigned = binary.right.isSignedFromSymTab(symbolTable);
                        if (lhsSigned != rhsSigned) {
                            std.log.warn("binary: {any}\n", .{binary});
                            unreachable;
                        }

                        setCC.* = assembly.Instruction{ .SetCC = assembly.SetCC{
                            .code = assembly.CondCode.getFromTacOp(binary.op, lhsSigned),
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
                const destType = cp.dest.getAsmTypeFromSymTab(symbolTable).?;
                cpInstr.* = assembly.Instruction{ .Mov = assembly.MovInst{
                    .src = src,
                    .dest = dest,
                    .type = destType,
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
                    .dest = assembly.Operand{
                        .Reg = jmp.condition.getAsmTypeFromSymTab(symbolTable).?.getR10Variety(),
                    },
                    .type = jmp.condition.getAsmTypeFromSymTab(symbolTable).?,
                } };
                checkZero.* = assembly.Instruction{ .Cmp = assembly.Cmp{
                    .op2 = assembly.Operand{ .Reg = assembly.Reg.R10 },
                    .op1 = assembly.Operand{
                        .Imm = 0,
                    },
                    .type = assembly.AsmType.LongWord,
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
                    .dest = assembly.Operand{
                        .Reg = jmp.condition.getAsmTypeFromSymTab(symbolTable).?.getR10Variety(),
                    },
                    .type = jmp.condition.getAsmTypeFromSymTab(symbolTable).?,
                } };
                checkZero.* = assembly.Instruction{ .Cmp = assembly.Cmp{
                    .op2 = assembly.Operand{ .Reg = assembly.Reg.R10 },
                    .op1 = assembly.Operand{
                        .Imm = 0,
                    },
                    .type = assembly.AsmType.LongWord,
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
                const registers32 = [_]assembly.Reg{ assembly.Reg.EDI, assembly.Reg.ESI, assembly.Reg.EDX, assembly.Reg.ECX, assembly.Reg.R8, assembly.Reg.R9 };
                const registers64 = [_]assembly.Reg{ assembly.Reg.RDI, assembly.Reg.RSI, assembly.Reg.RDX, assembly.Reg.RCX, assembly.Reg.R8_64, assembly.Reg.R9_64 };
                if (fnCall.args.items.len < 6) {
                    for (fnCall.args.items, 0..) |arg, i| {
                        const movArgToReg = try allocator.create(assembly.Instruction);
                        const assemblyArg = try arg.codegen(symbolTable, allocator);
                        const assemblyArgType = arg.getAsmTypeFromSymTab(symbolTable).?;
                        movArgToReg.* = assembly.Instruction{
                            .Mov = assembly.MovInst{
                                .src = assemblyArg,
                                .dest = assembly.Operand{ .Reg = switch (assemblyArgType) {
                                    .LongWord => registers32[i],
                                    .QuadWord => registers64[i],
                                    .Float => unreachable,
                                } },
                                .type = assemblyArgType,
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
                            .type = fnCall.dest.getAsmTypeFromSymTab(symbolTable).?,
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

pub const Val = union(ValType) {
    Constant: Constant,
    Variable: []u8,

    const Self = @This();

    pub fn isSignedFromSymTab(self: *Self, symbolTable: *std.StringHashMap(*assembly.Symbol)) bool {
        return switch (self.*) {
            .Constant => |constant| switch (constant) {
                .Long, .Integer => true,
                .ULong, .UInt => false,
                .Float => unreachable,
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
            .Variable => |varName| if (symbolTable.get(varName)) |sym| sym.Obj.type else blk: {
                std.log.warn("Not found: {s}\n", .{varName});
                break :blk null;
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
                                operand.* = .{ .Pseudo = variable };
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
