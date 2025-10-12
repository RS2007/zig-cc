const std = @import("std");
const lexer = @import("./lexer.zig");
const parser = @import("./parser.zig");
const tac = @import("./TAC.zig");
const semantic = @import("./semantic.zig");

// A different naming convention here to denote globals
var tac_ONE: tac.Val = .{ .Constant = .{ .Integer = 1 } };
var tac_ZERO: tac.Val = .{ .Constant = .{ .Integer = 0 } };

pub const MemoryError = error{
    OutOfMemory,
};

pub const TempGenerator = struct {
    state: u32,

    pub fn genTemp(self: *TempGenerator, allocator: std.mem.Allocator) CodegenError![]u8 {
        const tempPrefix = "tmp";
        const tempVar = try std.fmt.allocPrint(allocator, "{s}{d}", .{ tempPrefix, self.genId() });
        // if (std.mem.eql(u8, tempVar, "tmp10")) unreachable;
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
    ExternalDecl,
    BlockItem,
    Statement,
    Expression,
};

pub const ExternalDeclType = enum {
    FunctionDecl,
    VarDeclaration,
};

pub const TypedIdentifier = struct {
    name: []u8,
    type: ?Type = null,
};
pub const Qualifier = enum {
    STATIC,
    EXTERN,
    const Self = @This();
    pub fn from(tokType: lexer.TokenType) ?Self {
        return switch (tokType) {
            .STATIC => Qualifier.STATIC,
            .EXTERN => Qualifier.EXTERN,
            else => null,
        };
    }
};
pub const ExternalDecl = union(ExternalDeclType) {
    FunctionDecl: *FunctionDef,
    VarDeclaration: *Declaration,
    const Self = @This();
    //pub fn format(
    //    self: Self,
    //    comptime fmt: []const u8,
    //    options: std.fmt.FormatOptions,
    //    writer: anytype,
    //) !void {
    //    _ = fmt;
    //    _ = options;
    //    switch (self) {
    //        .FunctionDecl => |fnDecl| {
    //            try writer.print("{s}(", .{fnDecl.name});
    //            try writer.print("{any})\n", .{fnDecl.args.items});
    //            try writer.print("{any}\n", .{fnDecl.blockItems.items});
    //        },
    //        .VarDeclaration => |varDecl| {
    //            try writer.print("{s} = ", .{varDecl.name});
    //            try writer.print("{any}\n", .{varDecl.expression});
    //        },
    //    }
    //}
    pub fn genTAC(externalDecl: *Self, renderer: *TACRenderer, symbolTable: std.StringHashMap(*semantic.Symbol)) !?*tac.FunctionDef {
        switch (externalDecl.*) {
            .FunctionDecl => |functionDecl| {
                if (!functionDecl.isDefined()) return null;
                const tacFunctionDef = try renderer.allocator.create(tac.FunctionDef);
                var instructions = std.ArrayList(*tac.Instruction).init(renderer.allocator);
                try functionDecl.genTAC(renderer, &instructions, symbolTable);
                const functionDeclarator = try functionDecl.declarator.unwrapFuncDeclarator();
                const fnName = (try functionDeclarator.declarator.unwrapIdentDecl()).Ident;
                tacFunctionDef.* = .{
                    .name = fnName,
                    .args = std.ArrayList([]u8).init(renderer.allocator),
                    .instructions = instructions,
                    //TODO: Change this later
                    .global = true,
                };
                for (functionDeclarator.params.items) |arg| {
                    const argName = (try arg.NonVoidArg.declarator.unwrapIdentDecl()).Ident;
                    try tacFunctionDef.args.append(argName);
                }
                return tacFunctionDef;
            },
            .VarDeclaration => {
                return null;
            },
        }
    }
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

    //pub fn format(
    //    self: BlockItem,
    //    comptime fmt: []const u8,
    //    options: std.fmt.FormatOptions,
    //    writer: anytype,
    //) !void {
    //    _ = fmt;
    //    _ = options;
    //    switch (self) {
    //        .Statement => |stmt| {
    //            try writer.print("{any}\n", .{stmt});
    //        },
    //        .Declaration => |decl| {
    //            try writer.print("\n{any}", .{decl});
    //        },
    //    }
    //}

    pub fn genTAC(self: Self, renderer: *TACRenderer, instructions: *std.ArrayList(*tac.Instruction), symbolTable: *std.StringHashMap(*semantic.Symbol)) !void {
        switch (self) {
            .Statement => |stmt| {
                try stmt.genTACInstructions(renderer, instructions, symbolTable);
            },
            .Declaration => |decl| {
                try decl.genTACInstructions(renderer, instructions, symbolTable);
            },
        }
    }
};

pub const Declaration = struct {
    declarator: *Declarator,
    type: Type,
    varInitValue: ?*Initializer,
    storageClass: ?Qualifier = null,

    //pub fn format(
    //    self: Declaration,
    //    comptime fmt: []const u8,
    //    options: std.fmt.FormatOptions,
    //    writer: anytype,
    //) !void {
    //    _ = fmt;
    //    _ = options;
    //    try writer.print("{any} {s} = {any}", .{
    //        self.type,
    //        self.name,
    //        self.expression,
    //    });
    //}

    const Self = @This();
    pub fn genTACInstructions(
        self: Self,
        renderer: *TACRenderer,
        instructions: *std.ArrayList(*tac.Instruction),
        symbolTable: *std.StringHashMap(*semantic.Symbol),
    ) !void {
        const hasExpr = self.varInitValue;
        if (hasExpr) |varInitValue| {
            const symbolIdentifier = (try self.declarator.unwrapIdentDecl()).Ident;
            if (symbolTable.get(symbolIdentifier)) |sym| {
                if (sym.attributes == .StaticAttr) {
                    return;
                }
            }
            const name = (try self.declarator.unwrapIdentDecl()).Ident;
            // arrayTmp should store the array and the pointer to it must be stored as the name, since that's how the semantics of array assignment works in C
            const rhsSymbol = try renderer.allocator.create(assembly.Symbol);

            switch (varInitValue.*) {
                .Expression => |expr| {
                    try varInitValue.genTACInstructionsAndCvt(renderer, instructions, name, null);
                    rhsSymbol.* = .{
                        .Obj = .{
                            .signed = expr.getType().signed(),
                            .static = false,
                            .type = assembly.AsmType.from(Type, expr.getType()),
                        },
                    };
                    try renderer.asmSymbolTable.put(name, rhsSymbol);
                },
                .ArrayExpr => |arrayExpr| {
                    rhsSymbol.* = .{ .Obj = .{
                        .type = .{ .ByteArray = .{
                            .size = arrayExpr.type.?.size() * arrayExpr.initializers.items.len,
                            .alignment = arrayExpr.type.?.alignment(),
                        } },
                        .static = false,
                        .signed = (try arrayExpr.type.?.decrementDepth()).signed(),
                    } };
                    const rhsTemp = try renderer.allocator.create(tac.Val);
                    rhsTemp.* = .{ .Variable = try tempGen.genTemp(renderer.allocator) };
                    try varInitValue.genTACInstructionsAndCvt(renderer, instructions, rhsTemp.Variable, 0);
                    const leaInst = try renderer.allocator.create(tac.Instruction);
                    const lhsSymbol = try renderer.allocator.create(assembly.Symbol);
                    const lhsTACVal = try renderer.allocator.create(tac.Val);
                    lhsTACVal.* = .{ .Variable = name };
                    lhsSymbol.* = .{ .Obj = .{
                        .type = .QuadWord,
                        .static = false,
                        .signed = false,
                    } };
                    try renderer.asmSymbolTable.put(rhsTemp.Variable, rhsSymbol);
                    leaInst.* = .{ .GetAddress = .{
                        .src = rhsTemp,
                        .dest = lhsTACVal,
                    } };
                    try instructions.append(leaInst);
                },
            }
        }
    }
    pub fn fixReturnType(self: *Self, allocator: std.mem.Allocator) !void {
        const tentativeType = self.type;
        std.log.warn("Declarator: {}", .{self});
        switch (self.declarator.*) {
            .PointerDeclarator => |ptr| {
                var depth: usize = 1;
                var runningPtr = ptr;
                while (true) {
                    if (std.meta.activeTag(runningPtr.*) == .Ident) break;
                    if (std.meta.activeTag(runningPtr.*) == .FunDeclarator) unreachable;
                    depth += 1;
                    runningPtr = runningPtr.PointerDeclarator;
                }
                self.type = try astPointerTypeFromDepth(tentativeType, depth, allocator);
            },
            .Ident => {},
            .FunDeclarator => {
                std.log.warn("Implement function pointers\n", .{});
            },
            .ArrayDeclarator => |arrDecl| {
                var depth: usize = 0;
                var runningPtr = arrDecl.declarator;
                while (true) {
                    if (std.meta.activeTag(runningPtr.*) == .Ident) break;
                    if (std.meta.activeTag(runningPtr.*) == .FunDeclarator) unreachable;
                    if (std.meta.activeTag(runningPtr.*) == .ArrayDeclarator) unreachable;
                    depth += 1;
                    runningPtr = runningPtr.PointerDeclarator;
                }
                self.type = try astPointerTypeFromDepth(self.type, depth + arrDecl.size.items.len, allocator);
            },
        }
    }
};

pub const CodegenError = error{
    OutOfMemory,
    NoSpaceLeft,
    NoInnerFuncDeclarator,
    FunctionDeclCantUnwrapIdent,
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
    Constant,
    Unary,
    Binary,
    Ternary,
    Identifier,
    Assignment,
    FunctionCall,
    Cast,
    // INFO: Not making them unary ops, cause unary ops usually share semantics (particularly in instruction fixup)
    // If later it's found that these ops do share them, then they can be moved to unary
    AddrOf,
    Deref,
    ArrSubscript,
};

fn convertSymToTAC(tacProgram: *tac.Program, symbolTable: std.StringHashMap(*semantic.Symbol)) MemoryError!void {
    var symbolTableIter = symbolTable.iterator();
    while (symbolTableIter.next()) |iterator| {
        const key = iterator.key_ptr.*;
        const value = iterator.value_ptr.*;
        const tacTopLevelDecl = try symbolTable.allocator.create(tac.TopLevel);
        switch (value.*.attributes) {
            .StaticAttr => |staticAttr| {
                switch (staticAttr.init) {
                    .Initial => |initial| {
                        const staticVar = try symbolTable.allocator.create(tac.StaticVar);
                        staticVar.* = .{
                            .name = @constCast(key),
                            .global = staticAttr.global,
                            .init = switch (value.typeInfo) {
                                .Integer => .{ .Integer = initial.value.Integer },
                                .Long => .{ .Long = initial.value.Long },
                                .Float => .{ .Float = initial.value.Float },
                                else => unreachable,
                            },
                            .type = switch (value.typeInfo) {
                                .Integer => tac.ConstantType.Integer,
                                .Long => tac.ConstantType.Long,
                                .Float => tac.ConstantType.Float,
                                else => unreachable,
                            },
                        };
                        tacTopLevelDecl.* = .{
                            .StaticVar = staticVar,
                        };
                        try tacProgram.topLevelDecls.append(tacTopLevelDecl);
                    },
                    .NoInit => {},
                    .Tentative => {
                        const staticVar = try symbolTable.allocator.create(tac.StaticVar);
                        staticVar.* = .{
                            .name = @constCast(key),
                            .global = staticAttr.global,
                            .init = switch (value.typeInfo) {
                                .Integer => .{ .Integer = 0 },
                                .Long => .{ .Long = 0 },
                                .ULong => .{ .ULong = 0 },
                                .UInteger => .{ .UInt = 0 },
                                .Pointer => .{ .ULong = 0 },
                                else => unreachable,
                            },
                            .type = switch (value.typeInfo) {
                                .Integer => .Integer,
                                .Long => .Long,
                                .ULong => .ULong,
                                .UInteger => .UInt,
                                .Pointer => .ULong,
                                else => unreachable,
                            },
                        };
                        tacTopLevelDecl.* = .{
                            .StaticVar = staticVar,
                        };
                        try tacProgram.topLevelDecls.append(tacTopLevelDecl);
                    },
                }
            },
            else => {},
        }
    }
}

pub const Program = struct {
    externalDecls: std.ArrayList(*ExternalDecl),
};
pub const Return = struct {
    expression: *Expression,
};

pub const ArrayTy = struct {
    ty: *Type,
    size: usize,
};

pub const Type = union(enum) {
    Integer,
    Void,
    Long,
    UInteger,
    ULong,
    Float,
    Array: ArrayTy,
    Pointer: *Type,

    const Self = @This();

    pub fn from(tokenType: lexer.TokenType) Self {
        return switch (tokenType) {
            .INT_TYPE => .Integer,
            .VOID => .Void,
            .LONG_TYPE => .Long,
            .UNSIGNED_LONG => .ULong,
            .UNSIGNED_INTEGER => .UInteger,
            .FLOAT_TYPE => .Float,
            else => unreachable,
        };
    }

    pub fn fromSemType(semanticType: *semantic.TypeInfo, allocator: std.mem.Allocator) error{OutOfMemory}!Self {
        return switch (semanticType.*) {
            .Integer => .Integer,
            .Void => .Void,
            .Long => .Long,
            .ULong => .ULong,
            .UInteger => .UInteger,
            .Function => unreachable,
            .Float => .Float,
            .Pointer => |ptr| {
                const inner = try allocator.create(Type);
                inner.* = try fromSemType(ptr, allocator);
                return .{ .Pointer = inner };
            },
        };
    }

    pub inline fn alignment(self: Self) u32 {
        return switch (self) {
            .Integer, .UInteger => 4,
            .Long, .ULong, .Pointer => 8,
            .Float => 8,
            .Array => unreachable,
            else => unreachable,
        };
    }

    pub inline fn size(self: Self) usize {
        return switch (self) {
            .Integer, .UInteger => 4,
            .Pointer, .Long, .ULong, .Float => 8,
            else => {
                std.log.warn("size of {any} is not implemented\n", .{self});
                unreachable;
            },
        };
    }

    pub inline fn signed(self: Self) bool {
        return switch (self) {
            .Long, .Integer => true,
            .UInteger, .ULong => false,
            .Float => false,
            .Pointer => false,
            else => unreachable,
        };
    }
    pub fn toSemPointerTy(self: Type, allocator: std.mem.Allocator) error{OutOfMemory}!semantic.TypeInfo {
        return switch (self) {
            .Integer => .Integer,
            .Long => .Long,
            .ULong => .ULong,
            .UInteger => .UInteger,
            .Float => .Float,
            .Pointer => |ptr| {
                const inner = try allocator.create(semantic.TypeInfo);
                inner.* = try ptr.toSemPointerTy(allocator);
                return .{ .Pointer = inner };
            },
            else => unreachable,
        };
    }

    pub fn deepEql(self: Self, other: Self) bool {
        if (std.meta.activeTag(self) != std.meta.activeTag(other)) return false;
        switch (self) {
            .Pointer => |ptr| {
                return ptr.deepEql(other.Pointer.*);
            },
            else => return true,
        }
    }

    pub inline fn isNumeric(self: Self) bool {
        return switch (self) {
            .Integer, .UInteger, .Long, .ULong, .Float => true,
            else => false,
        };
    }

    pub fn format(self: Self, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;
        switch (self) {
            .Integer => try writer.print("int", .{}),
            .Void => try writer.print("void", .{}),
            .Long => try writer.print("long", .{}),
            .UInteger => try writer.print("unsigned int", .{}),
            .ULong => try writer.print("unsigned long", .{}),
            .Float => try writer.print("float", .{}),
            .Array => |arr| {
                try writer.print("{any}[{any}]", .{ arr.ty, arr.size });
            },
            .Pointer => |ptr| {
                try writer.print("*{any}", .{ptr});
            },
        }
    }
    pub inline fn decrementDepth(self: *const Self) semantic.TypeCheckerError!Type {
        if (std.meta.activeTag(self.*) != .Pointer)
            return semantic.TypeCheckerError.InvalidOperand;

        return self.*.Pointer.*;
    }
};

pub const NonVoidArg = struct {
    type: Type,
    declarator: *Declarator,
    const Self = @This();

    pub fn fixType(self: *Self, allocator: std.mem.Allocator) !void {
        switch (self.declarator.*) {
            .FunDeclarator => unreachable,
            .Ident => {},
            .PointerDeclarator => |ptr| {
                var depth: usize = 1;
                var runningPtr = ptr;
                while (true) {
                    if (std.meta.activeTag(runningPtr.*) == .Ident) break;
                    if (std.meta.activeTag(runningPtr.*) == .FunDeclarator) unreachable;
                    depth += 1;
                    runningPtr = runningPtr.PointerDeclarator;
                }
                self.type = try astPointerTypeFromDepth(self.type, depth, allocator);
            },
            .ArrayDeclarator => |arrDecl| {
                // First get the type from array and then get the internal decl and get the pointer depth
                // The sum of these two will give the actual depth
                var depth: usize = 0;
                var runningPtr = arrDecl.declarator;
                while (true) {
                    if (std.meta.activeTag(runningPtr.*) == .Ident) break;
                    if (std.meta.activeTag(runningPtr.*) == .FunDeclarator) unreachable;
                    if (std.meta.activeTag(runningPtr.*) == .ArrayDeclarator) unreachable;
                    depth += 1;
                    runningPtr = runningPtr.PointerDeclarator;
                }
                self.type = try astPointerTypeFromDepth(self.type, depth + arrDecl.size.items.len, allocator);
            },
        }
    }
};

// INFO: Might be a bad idea, maybe look into this later?
pub const ArgType = enum {
    Void,
    NonVoidArg,
};

pub const Arg = union(ArgType) {
    Void: void,
    NonVoidArg: NonVoidArg,
    //pub fn format(
    //    self: Arg,
    //    comptime fmt: []const u8,
    //    options: std.fmt.FormatOptions,
    //    writer: anytype,
    //) !void {
    //    _ = fmt;
    //    _ = options;
    //    switch (self) {
    //        .Void => {
    //            try writer.print("\nvoid", .{});
    //        },
    //        .NonVoidArg => |arg| {
    //            try writer.print("\n{any} {s}", .{ arg.type, arg.identifier });
    //        },
    //    }
    //}
};

pub fn astPointerTypeFromDepth(@"type": Type, depth: usize, allocator: std.mem.Allocator) !Type {
    if (depth == 0) return @"type";
    var fixedType = @"type";
    for (0..depth) |_| {
        const inner = try allocator.create(Type);
        inner.* = fixedType;
        fixedType = .{ .Pointer = inner };
    }
    return fixedType;
}

pub const FunctionDef = struct {
    // INFO: The functiondef types get rewritten during the typecheck stage
    declarator: *Declarator,
    blockItems: std.ArrayList(*BlockItem),
    returnType: Type,
    storageClass: ?Qualifier,

    pub fn isDefined(self: *FunctionDef) bool {
        return self.blockItems.items.len != 0;
    }

    pub fn genTAC(functionDef: FunctionDef, renderer: *TACRenderer, instructions: *std.ArrayList(*tac.Instruction), symbolTable: std.StringHashMap(*semantic.Symbol)) !void {
        for (functionDef.blockItems.items) |blockItem| {
            try blockItem.genTAC(renderer, instructions, @constCast(&symbolTable));
        }
    }
    pub fn fixReturnType(self: *FunctionDef, allocator: std.mem.Allocator) !void {
        const tentativeReturnType = self.returnType;
        switch (self.declarator.*) {
            .PointerDeclarator => |ptr| {
                // find pointer indirections till it hits the function
                // declarator
                var depth: usize = 1;
                var runningPtr = ptr;
                while (true) {
                    if (std.meta.activeTag(runningPtr.*) == .Ident) unreachable;
                    if (std.meta.activeTag(runningPtr.*) == .FunDeclarator) break;
                    depth += 1;
                    runningPtr = runningPtr.PointerDeclarator;
                }
                self.returnType = try astPointerTypeFromDepth(tentativeReturnType, depth, allocator);
            },
            .Ident => unreachable,
            .FunDeclarator => {},
            .ArrayDeclarator => unreachable,
        }
    }
};
pub const While = struct {
    condition: *Expression,
    body: *Statement,
    loopId: u32,
};

pub const ForInit = union(ForInitType) {
    Declaration: *Declaration,
    Expression: *Expression,
    pub fn genTACInstructions(forInit: *ForInit, renderer: *TACRenderer, instructions: *std.ArrayList(*tac.Instruction), symbolTable: *std.StringHashMap(*semantic.Symbol)) !void {
        switch (forInit.*) {
            .Declaration => |decl| {
                try decl.genTACInstructions(renderer, instructions, symbolTable);
            },
            .Expression => |expr| {
                _ = try expr.genTACInstructions(renderer, instructions);
            },
        }
    }
};
pub const ForInitType = enum {
    Declaration,
    Expression,
};

pub const For = struct {
    init: *ForInit,
    condition: ?*Expression,
    post: ?*Expression,
    body: *Statement,
    loopId: u32,
};

inline fn chooseFloatCastInst(inner: *tac.Val, dest: *tac.Val, toType: Type) CodegenError!tac.Instruction {
    return switch (toType) {
        .Integer, .Long => .{ .FloatToInt = .{
            .src = inner,
            .dest = dest,
        } },
        .UInteger, .ULong => .{ .FloatToUInt = .{
            .src = inner,
            .dest = dest,
        } },
        else => |failingType| {
            std.log.warn("failing type: {any}\n", .{failingType});
            unreachable;
        },
    };
}

inline fn chooseIntCastInst(inner: *tac.Val, dest: *tac.Val, toType: Type, asmSymbolTable: *std.StringHashMap(*assembly.Symbol)) CodegenError!tac.Instruction {
    return switch (toType) {
        .Integer, .UInteger => .{ .Truncate = .{
            .src = inner,
            .dest = dest,
        } },
        .Long, .ULong, .Pointer => if (toType.signed())
            .{ .SignExtend = .{
                .src = inner,
                .dest = dest,
            } }
        else
            .{ .ZeroExtend = .{
                .src = inner,
                .dest = dest,
            } },
        .Float => if (inner.isSignedFromSymTab(asmSymbolTable))
            .{ .IntToFloat = .{
                .src = inner,
                .dest = dest,
            } }
        else
            .{ .UIntToFloat = .{
                .src = inner,
                .dest = dest,
            } },
        else => unreachable,
    };
}

pub const Statement = union(StatementType) {
    Return: Return,
    If: If,
    Expression: *Expression,
    Null: void,
    Goto: []u8,
    Label: []u8,
    Compound: std.ArrayList(*BlockItem),
    Break: u32,
    Continue: u32,
    DoWhile: While,
    While: While,
    For: For,

    pub fn genTACInstructions(statement: *Statement, renderer: *TACRenderer, instructions: *std.ArrayList(*tac.Instruction), symbolTable: *std.StringHashMap(*semantic.Symbol)) semantic.TypeCheckerError!void {
        switch (statement.*) {
            .Return => |retStmt| {
                const returnSymbol = try retStmt.expression.genTACInstructionsAndCvt(renderer, instructions);
                try instructions.append(try tac.createInst(
                    .Return,
                    tac.Return{
                        .val = returnSymbol,
                    },
                    renderer.allocator,
                ));
            },
            .Expression => |expr| {
                _ = try expr.genTACInstructionsAndCvt(renderer, instructions);
            },
            .Null => {},
            .If => |ifStmt| {
                const falseLabelName = try std.fmt.allocPrint(renderer.allocator, "falseLabel_{d}", .{tempGen.genId()});
                const exitLabelName = try std.fmt.allocPrint(renderer.allocator, "exitLabel_{d}", .{tempGen.genId()});
                const condVal = try ifStmt.condition.genTACInstructionsAndCvt(renderer, instructions);
                try instructions.append(
                    try tac.createInst(
                        .JumpIfZero,
                        tac.Jmp{
                            .condition = condVal,
                            .target = falseLabelName,
                        },
                        renderer.allocator,
                    ),
                );
                try ifStmt.thenStmt.genTACInstructions(renderer, instructions, symbolTable);
                try instructions.append(try tac.createInst(.Jump, exitLabelName, renderer.allocator));
                try instructions.append(try tac.createInst(.Label, falseLabelName, renderer.allocator));
                if (ifStmt.elseStmt) |elseStmt| {
                    try elseStmt.genTACInstructions(renderer, instructions, symbolTable);
                }
                try instructions.append(try tac.createInst(.Label, exitLabelName, renderer.allocator));
            },
            .Label => |label| {
                try instructions.append(try tac.createInst(.Label, label, renderer.allocator));
            },
            .Goto => |goto| {
                try instructions.append(try tac.createInst(.Jump, goto, renderer.allocator));
            },
            .Compound => |compound| {
                for (compound.items) |compoundStatement| {
                    try compoundStatement.genTAC(renderer, instructions, symbolTable);
                }
            },
            .Continue => |cont| {
                try instructions.append(try tac.createInst(
                    .Jump,
                    try std.fmt.allocPrint(renderer.allocator, "loop_start_{d}", .{cont}),
                    renderer.allocator,
                ));
            },
            .Break => |brk| {
                try instructions.append(try tac.createInst(
                    .Jump,
                    try std.fmt.allocPrint(renderer.allocator, "loop_end_{d}", .{brk}),
                    renderer.allocator,
                ));
            },
            .DoWhile => |doWhile| {
                const doWhileStartLabel = try std.fmt.allocPrint(renderer.allocator, "loop_start_{d}", .{doWhile.loopId});
                try instructions.append(try tac.createInst(
                    .Label,
                    doWhileStartLabel,
                    renderer.allocator,
                ));
                try doWhile.body.genTACInstructions(renderer, instructions, symbolTable);
                const condition = try doWhile.condition.genTACInstructionsAndCvt(renderer, instructions);
                try instructions.append(try tac.createInst(.JumpIfNotZero, tac.Jmp{
                    .condition = condition,
                    .target = doWhileStartLabel,
                }, renderer.allocator));
                const doWhileEndLabel = try std.fmt.allocPrint(renderer.allocator, "loop_end_{d}", .{doWhile.loopId});
                try instructions.append(try tac.createInst(
                    .Label,
                    doWhileEndLabel,
                    renderer.allocator,
                ));
            },
            .While => |whileStmt| {
                // INFO: The first optimization: This should be an if condition
                // followed by the do while loop assembly to save jumps
                const conditionOfIf = try whileStmt.condition.genTACInstructionsAndCvt(renderer, instructions);
                const whileEndLabelName = try std.fmt.allocPrint(renderer.allocator, "loop_end_{d}", .{whileStmt.loopId});
                const whileStartLabelName = try std.fmt.allocPrint(renderer.allocator, "loop_start_{d}", .{whileStmt.loopId});
                try instructions.appendSlice(
                    &[_]*tac.Instruction{
                        try tac.createInst(.JumpIfZero, tac.Jmp{
                            .condition = conditionOfIf,
                            .target = whileEndLabelName,
                        }, renderer.allocator),
                        try tac.createInst(
                            .Label,
                            whileStartLabelName,
                            renderer.allocator,
                        ),
                    },
                );
                try whileStmt.body.genTACInstructions(renderer, instructions, symbolTable);
                const conditionForInnerDoWhile = try whileStmt.condition.genTACInstructionsAndCvt(renderer, instructions);
                try instructions.appendSlice(
                    &[_]*tac.Instruction{
                        try tac.createInst(.JumpIfNotZero, tac.Jmp{
                            .target = whileStartLabelName,
                            .condition = conditionForInnerDoWhile,
                        }, renderer.allocator),
                        try tac.createInst(.Label, whileEndLabelName, renderer.allocator),
                    },
                );
            },
            .For => |forStmt| {
                const forStartLabelName = try std.fmt.allocPrint(renderer.allocator, "loop_start_{d}", .{forStmt.loopId});
                const forEndLabelName = try std.fmt.allocPrint(renderer.allocator, "loop_end_{d}", .{forStmt.loopId});
                try forStmt.init.genTACInstructions(renderer, instructions, symbolTable);
                try instructions.append(try tac.createInst(
                    .Label,
                    forStartLabelName,
                    renderer.allocator,
                ));
                if (forStmt.condition) |condition| {
                    const condVal = try condition.genTACInstructionsAndCvt(renderer, instructions);
                    try instructions.append(try tac.createInst(.JumpIfZero, tac.Jmp{
                        .condition = condVal,
                        .target = forEndLabelName,
                    }, renderer.allocator));
                }
                try forStmt.body.genTACInstructions(renderer, instructions, symbolTable);
                if (forStmt.post) |post|
                    _ = try post.genTACInstructionsAndCvt(renderer, instructions);

                try instructions.appendSlice(&[_]*tac.Instruction{
                    try tac.createInst(.Jump, forStartLabelName, renderer.allocator),
                    try tac.createInst(.Label, forEndLabelName, renderer.allocator),
                });
            },
        }
    }
};

pub fn prettyPrintAST(node: Node, writer: anytype, depth: usize) !void {
    // TODO: out of the loop, support it with multiple functions
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
            try prettyPrintAST(Node{ .FunctionDef = &program.externalDecls }, writer, depth + 1);
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
                .For => try writer.print("{s} For: TODO{s}", .{ colors.cyan, colors.reset }),
                .DoWhile => try writer.print("{s} DoWhile: TODO{s}", .{ colors.cyan, colors.reset }),
                .While => try writer.print("{s} While: TODO{s}", .{ colors.cyan, colors.reset }),
                .Break => try writer.print("{s} Break: TODO{s}", .{ colors.cyan, colors.reset }),
                .Continue => try writer.print("{s} Continue: TODO{s}", .{ colors.cyan, colors.reset }),
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
                .Identifier => |ident| try writer.print("{s}Identifier: {s}{s}\n", .{ colors.green, ident.name, colors.reset }),
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

    const Self = @This();
    pub fn isCompareOp(op: Self) bool {
        return switch (op) {
            .LESS_THAN, .LESS_THAN_EQ, .GREATER_THAN, .GREATER_THAN_EQ, .EQUALS, .NOT_EQUALS => true,
            else => false,
        };
    }
};

pub const Unary = struct {
    type: ?Type = null,
    unaryOp: UnaryOp,
    exp: *Expression,
};

pub const Deref = struct {
    type: ?Type = null,
    exp: *Expression,
};

pub const AddrOf = struct {
    type: ?Type = null,
    exp: *Expression,
};

pub const Binary = struct {
    type: ?Type = null,
    op: BinOp,
    lhs: *Expression,
    rhs: *Expression,
};

pub const Ternary = struct {
    type: ?Type = null,
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
    type: ?Type = null,
    lhs: *Expression,
    rhs: *Expression,
};

pub const FunctionCall = struct {
    type: ?Type = null,
    name: []u8,
    args: std.ArrayList(*Expression),
};

pub const ConstantKind = enum {
    Integer,
    Long,
    ULong,
    UInteger,
    Float,
};
pub const Constant = struct {
    type: Type,
    value: union(ConstantKind) {
        Integer: i32,
        Long: i64,
        ULong: u64,
        UInteger: u32,
        Float: f64,
    },
    const Self = @This();
    pub inline fn to(self: *Self, comptime T: type) T {
        return switch (self.value) {
            .Integer => |integer| @intCast(integer),
            .Long => |long| @intCast(long),
            .ULong => |ulong| @intCast(ulong),
            .UInteger => |uint| @intCast(uint),
            else => unreachable,
        };
    }
    pub inline fn isNullPtr(self: *Self) bool {
        return switch (self.value) {
            .Integer => |integer| integer == 0,
            .Long => |long| long == 0,
            .ULong => |ulong| ulong == 0,
            .UInteger => |uint| uint == 0,
            .Float => |float| float == 0.0,
        };
    }
};
pub const Cast = struct {
    type: Type,
    value: *Expression,
};

const assembly = @import("./Assembly.zig");

pub const ParamInfo = struct {
    type: Type,
    declarator: *Declarator,
};

pub const FunDeclarator = struct {
    params: std.ArrayList(*Arg),
    declarator: *Declarator,
};

pub const DeclaratorError = error{
    NoInnerFuncDeclarator,
    FunctionDeclCantUnwrapIdent,
};

pub const ArrDeclarator = struct {
    declarator: *Declarator,
    size: std.ArrayList(usize),
};

pub const DeclaratorSuffix = union(enum) {
    ArgList: std.ArrayList(*Arg),
    ArraySuffix: std.ArrayList(usize),
};

pub const ArrayInitializer = struct {
    type: ?Type,
    initializers: std.ArrayList(*Initializer),

    pub fn genTACInstructions(
        arrayInit: *ArrayInitializer,
        renderer: *TACRenderer,
        instructions: *std.ArrayList(*tac.Instruction),
        name: []u8,
        offset: i64,
    ) semantic.TypeCheckerError!void {
        const arrType = arrayInit.type.?;
        const arrMemType = try arrType.decrementDepth();
        const stepSize = arrMemType.size();
        for (arrayInit.initializers.items, 0..) |initializer, index| {
            try initializer.genTACInstructionsAndCvt(renderer, instructions, name, offset + @as(i64, @intCast(index * stepSize)));
        }
    }
};

pub const Initializer = union(enum) {
    Expression: *Expression,
    ArrayExpr: *ArrayInitializer,

    pub fn genTACInstructionsAndCvt(initializer: *Initializer, renderer: *TACRenderer, instructions: *std.ArrayList(*tac.Instruction), name: []u8, offset: ?i64) !void {
        return switch (initializer.*) {
            .Expression => |expr| {
                const rhs = try expr.genTACInstructionsAndCvt(renderer, instructions);
                const instr = try renderer.allocator.create(tac.Instruction);

                if (offset) |off| {
                    instr.* = tac.Instruction{ .CopyIntoOffset = tac.CopyIntoOffset{ .src = rhs, .dest = name, .offset = off } };
                } else {
                    const destOperand = try renderer.allocator.create(tac.Val);
                    destOperand.* = .{ .Variable = name };
                    instr.* = tac.Instruction{ .Copy = tac.Copy{
                        .src = rhs,
                        .dest = destOperand,
                    } };
                }
                try instructions.append(instr);
            },
            .ArrayExpr => |arrayExpr| {
                std.debug.assert(offset != null);
                try arrayExpr.genTACInstructions(renderer, instructions, name, offset.?);
            },
        };
    }
};

pub const Declarator = union(enum) {
    Ident: []u8,
    PointerDeclarator: *Declarator,
    FunDeclarator: *FunDeclarator,
    ArrayDeclarator: *ArrDeclarator,

    const Self = @This();
    pub fn unwrapFuncDeclarator(self: *Self) DeclaratorError!*FunDeclarator {
        switch (self.*) {
            .FunDeclarator => |funDeclarator| return funDeclarator,
            .PointerDeclarator => |pointerDeclarator| return pointerDeclarator.unwrapFuncDeclarator(),
            .Ident => return error.NoInnerFuncDeclarator,
            else => unreachable,
        }
    }
    pub fn unwrapIdentDecl(self: *Self) DeclaratorError!*Declarator {
        return switch (self.*) {
            .Ident => @constCast(self),
            .PointerDeclarator => |pointerDeclarator| pointerDeclarator.unwrapIdentDecl(),
            .FunDeclarator => DeclaratorError.FunctionDeclCantUnwrapIdent,
            .ArrayDeclarator => |arrDeclarator| arrDeclarator.declarator.unwrapIdentDecl(),
        };
    }
    pub fn containsFuncDeclarator(self: *Self) bool {
        return switch (self.*) {
            .FunDeclarator => true,
            .Ident => false,
            .PointerDeclarator => |pointerDeclarator| pointerDeclarator.containsFuncDeclarator(),
            else => false,
        };
    }
    pub fn format(
        self: *Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        switch (self.*) {
            .Ident => |iden| {
                try writer.print("[ Identifier: {s} ]", .{iden});
            },
            .PointerDeclarator => |pointerDecl| {
                try writer.print("[ PointerDeclaration: {} ]", .{pointerDecl});
            },
            .FunDeclarator => |funDecl| {
                try writer.print("[ FunctionDeclaration: {}\n", .{funDecl.declarator});
                for (funDecl.params.items) |param| {
                    try writer.print("\t{}\n", .{param});
                }
                try writer.print("]\n", .{});
            },
            .ArrayDeclarator => |arrDecl| {
                try writer.print("[ Depth: {},  ArrDeclarator: {} ]", .{ arrDecl.size.items.len, arrDecl.declarator });
            },
        }
    }
};

pub const TACRenderer = struct {
    astSymbolTable: std.StringHashMap(*semantic.Symbol),
    asmSymbolTable: std.StringHashMap(*assembly.Symbol),
    allocator: std.mem.Allocator,
    const Self = @This();
    pub fn init(allocator: std.mem.Allocator, astSymbolTable: std.StringHashMap(*semantic.Symbol)) !*Self {
        const tacRenderer = try allocator.create(Self);
        tacRenderer.* = .{
            .asmSymbolTable = (try tac.astSymTabToTacSymTab(allocator, astSymbolTable)),
            .astSymbolTable = astSymbolTable,
            .allocator = allocator,
        };
        return tacRenderer;
    }
    pub fn render(self: *Self, program: *Program) !*tac.Program {
        const tacProgram = try self.allocator.create(tac.Program);
        tacProgram.* = .{
            .topLevelDecls = std.ArrayList(*tac.TopLevel).init(self.allocator),
        };
        try convertSymToTAC(tacProgram, self.astSymbolTable);
        for (program.externalDecls.items) |externalDecl| {
            const functionDef = try externalDecl.genTAC(self, self.astSymbolTable);
            if (functionDef) |resolvedFnDef| {
                const tacTopLevelDecl = try self.allocator.create(tac.TopLevel);
                tacTopLevelDecl.* = .{
                    .Function = resolvedFnDef,
                };
                try tacProgram.topLevelDecls.append(tacTopLevelDecl);
            }
        }
        return tacProgram;
    }
};

pub const ArrSubscript = struct {
    type: ?Type,
    arr: *Expression,
    index: *Expression,
};

pub const Expression = union(ExpressionType) {
    Constant: Constant,
    Unary: Unary,
    Binary: Binary,
    Ternary: Ternary,
    Identifier: TypedIdentifier,
    Assignment: Assignment,
    FunctionCall: FunctionCall,
    Cast: Cast,
    AddrOf: AddrOf,
    Deref: Deref,
    ArrSubscript: ArrSubscript,
    const Self = @This();

    pub fn getType(self: *Self) Type {
        return switch (self.*) {
            .Cast => |cast| cast.type,
            .Assignment => |assignment| assignment.type.?,
            .Binary => |binary| binary.type.?,
            .FunctionCall => |fnCall| fnCall.type.?,
            .Identifier => |identifier| identifier.type.?,
            .Constant => |constant| constant.type,
            .Unary => |unary| unary.type.?,
            .Ternary => |ternary| ternary.type.?,
            .Deref => |deref| deref.type.?,
            .AddrOf => |addrOf| addrOf.type.?,
            .ArrSubscript => |arrSubscript| arrSubscript.type.?,
        };
    }

    pub fn isNullPtr(self: *Self) bool {
        return switch (self.*) {
            .Cast => |cast| cast.value.isNullPtr(),
            .Constant => |constant| @constCast(&constant).isNullPtr(),
            else => false,
        };
    }

    pub inline fn isLvalue(self: *Self) bool {
        return switch (self.*) {
            .Identifier, .Deref, .Assignment, .ArrSubscript => true,
            else => false,
        };
    }

    pub fn genTACInstructionsAndCvt(expression: *Expression, renderer: *TACRenderer, instructions: *std.ArrayList(*tac.Instruction)) !*tac.Val {
        const boxedVal = try expression.genTACInstructions(renderer, instructions);

        return switch (boxedVal.*) {
            .PlainVal => |val| val,
            .DerefedVal => |val| try loadDerefValue(
                renderer,
                instructions,
                val,
                expression.getType(),
            ),
        };
    }
    inline fn chooseCast(renderer: *TACRenderer, inner: *tac.Val, dest: *tac.Val, castType: Type) !tac.Instruction {
        return if (inner.getAsmTypeFromSymTab(&renderer.asmSymbolTable).? == .Float) try chooseFloatCastInst(
            inner,
            dest,
            castType,
        ) else try chooseIntCastInst(
            inner,
            dest,
            castType,
            @constCast(&renderer.asmSymbolTable),
        );
    }

    inline fn loadDerefValue(
        renderer: *TACRenderer,
        instructions: *std.ArrayList(*tac.Instruction),
        ptr: *tac.Val,
        valueType: Type,
    ) !*tac.Val {
        const loadDest = try renderer.allocator.create(tac.Val);
        const loadTemp = try tempGen.genTemp(renderer.allocator);
        const loadSymbol = try renderer.allocator.create(assembly.Symbol);
        loadSymbol.* = .{
            .Obj = .{
                .type = assembly.AsmType.from(Type, valueType),
                .signed = valueType.signed(),
                .static = false,
            },
        };
        try renderer.asmSymbolTable.put(loadTemp, loadSymbol);
        loadDest.* = .{ .Variable = loadTemp };

        // Generate the load instruction
        const loadInstr = try renderer.allocator.create(tac.Instruction);
        loadInstr.* = .{ .Load = .{
            .srcPointer = ptr,
            .dest = loadDest,
        } };
        try instructions.append(loadInstr);

        return loadDest;
    }

    inline fn getASMSymFromType(
        astType: Type,
        allocator: std.mem.Allocator,
        options: struct { disableFloat: bool },
    ) !*assembly.Symbol {
        // Not sure if this is as general as it should be, just testing things out
        // for now

        const asmSymbol = try allocator.create(assembly.Symbol);
        asmSymbol.* = .{
            .Obj = .{
                .type = switch (astType) {
                    .Integer, .UInteger => assembly.AsmType.LongWord,
                    .Long, .ULong, .Pointer => assembly.AsmType.QuadWord,
                    .Float => if (!options.disableFloat) assembly.AsmType.Float else unreachable,
                    else => unreachable,
                },
                .signed = switch (astType) {
                    .Integer, .Long => true,
                    .UInteger, .ULong, .Pointer => false,
                    .Float => false,
                    else => unreachable,
                },
                .static = false,
            },
        };
        return asmSymbol;
    }

    pub fn genTACInstructions(
        expression: *Expression,
        renderer: *TACRenderer,
        instructions: *std.ArrayList(*tac.Instruction),
    ) semantic.TypeCheckerError!*tac.BoxedVal {
        switch (expression.*) {
            .Cast => |cast| {
                const inner = try cast.value.genTACInstructionsAndCvt(renderer, instructions);
                const boxed = try renderer.allocator.create(tac.BoxedVal);

                const isNoOp = struct {
                    inline fn noop(_cast: Cast) bool {
                        return (_cast.value.getType().deepEql(_cast.type)) or
                            (_cast.value.getType() == .Integer and _cast.type == .UInteger) or
                            (_cast.value.getType() == .UInteger and _cast.type == .Integer);
                    }
                }.noop(cast);

                if (isNoOp) {
                    boxed.* = .{ .PlainVal = inner };
                    return boxed;
                }

                const dest = try renderer.allocator.create(tac.Val);
                const destName = try tempGen.genTemp(renderer.allocator);
                const asmSymbol = try renderer.allocator.create(assembly.Symbol);
                asmSymbol.* = .{
                    .Obj = .{
                        .type = assembly.AsmType.from(Type, cast.type),
                        .signed = cast.type.signed(),
                        .static = false,
                    },
                };
                std.log.warn("Pushing in {s} = {any}\n", .{ destName, asmSymbol });
                try renderer.asmSymbolTable.put(destName, asmSymbol);
                dest.* = tac.Val{ .Variable = destName };
                boxed.* = tac.BoxedVal{ .PlainVal = dest };

                const castInst = try renderer.allocator.create(tac.Instruction);
                castInst.* = try chooseCast(renderer, inner, dest, cast.type);
                try instructions.append(castInst);
                return boxed;
            },
            .AddrOf => |addrOf| {
                const inner = try addrOf.exp.genTACInstructions(renderer, instructions);
                const boxed = try renderer.allocator.create(tac.BoxedVal);
                if (std.meta.activeTag(inner.*) == .DerefedVal) {
                    // If you are taking the address of a derefed value:
                    // &(*k) == k
                    boxed.* = .{ .PlainVal = inner.DerefedVal };
                    return boxed;
                }
                const addrOfInst = try renderer.allocator.create(tac.Instruction);
                const tacDest = try renderer.allocator.create(tac.Val);
                const tacDestName = try tempGen.genTemp(renderer.allocator);
                const tacDestSym = try renderer.allocator.create(assembly.Symbol);
                tacDestSym.* = .{ .Obj = .{
                    .type = .QuadWord,
                    .static = false,
                    .signed = false,
                } };
                try renderer.asmSymbolTable.put(tacDestName, tacDestSym);
                tacDest.* = .{
                    .Variable = tacDestName,
                };
                addrOfInst.* = .{ .GetAddress = .{
                    .src = inner.PlainVal,
                    .dest = tacDest,
                } };
                try instructions.append(addrOfInst);
                boxed.* = .{ .PlainVal = tacDest };
                return boxed;
            },
            .Deref => |deref| {
                const expr = try deref.exp.genTACInstructionsAndCvt(renderer, instructions);
                const loadInstruction = try renderer.allocator.create(tac.Instruction);
                const tacDestName = try tempGen.genTemp(renderer.allocator);
                const tacDest = try renderer.allocator.create(tac.Val);
                tacDest.* = tac.Val{ .Variable = tacDestName };
                const tacDestSym = try renderer.allocator.create(assembly.Symbol);
                const boxed = try renderer.allocator.create(tac.BoxedVal);
                tacDestSym.* = .{ .Obj = .{
                    .type = assembly.AsmType.from(Type, deref.type.?),
                    .signed = deref.type.?.signed(),
                    .static = false,
                } };
                try renderer.asmSymbolTable.put(tacDestName, tacDestSym);
                loadInstruction.* = .{
                    .Load = .{
                        .srcPointer = expr,
                        .dest = tacDest,
                    },
                };
                try instructions.append(loadInstruction);
                boxed.* = .{ .DerefedVal = expr };
                return boxed;
            },
            .Constant => |constant| {
                const val = try renderer.allocator.create(tac.Val);
                const boxed = try renderer.allocator.create(tac.BoxedVal);
                val.* = switch (constant.value) {
                    .Integer => |integer| tac.Val{ .Constant = .{ .Integer = integer } },
                    .Long => |long| tac.Val{ .Constant = .{ .Long = long } },
                    .UInteger => |uint| .{ .Constant = .{ .UInt = uint } },
                    .ULong => |ulong| .{ .Constant = .{ .ULong = ulong } },
                    .Float => |float| .{ .Constant = .{ .Float = float } },
                };
                boxed.* = .{ .PlainVal = val };
                return boxed;
            },
            .Unary => |unary| {
                const rhsVal = try unary.exp.genTACInstructionsAndCvt(renderer, instructions);
                const lhsVal = try renderer.allocator.create(tac.Val);
                const temp = try tempGen.genTemp(renderer.allocator);
                const asmSymbol = try renderer.allocator.create(assembly.Symbol);
                const instr = try renderer.allocator.create(tac.Instruction);
                switch (unary.unaryOp) {
                    .NEGATE => {
                        asmSymbol.* = .{
                            .Obj = .{
                                .type = assembly.AsmType.from(Type, unary.type.?),
                                .signed = unary.type.?.signed(),
                                .static = false,
                            },
                        };
                        instr.* = tac.Instruction{ .Unary = tac.Unary{
                            .op = tac.UnaryOp.NEGATE,
                            .src = rhsVal,
                            .dest = lhsVal,
                        } };
                    },
                    .COMPLEMENT => {
                        asmSymbol.* = .{
                            .Obj = .{
                                .type = switch (unary.type.?) {
                                    .Integer, .UInteger => assembly.AsmType.LongWord,
                                    .Long, .ULong => assembly.AsmType.QuadWord,
                                    else => unreachable,
                                },
                                .signed = switch (unary.type.?) {
                                    .Integer, .Long => true,
                                    .UInteger, .ULong => false,
                                    .Float => false,
                                    else => unreachable,
                                },
                                .static = false,
                            },
                        };
                        instr.* = tac.Instruction{ .Unary = tac.Unary{
                            .op = tac.UnaryOp.COMPLEMENT,
                            .src = rhsVal,
                            .dest = lhsVal,
                        } };
                    },
                }

                try renderer.asmSymbolTable.put(temp, asmSymbol);
                lhsVal.* = tac.Val{
                    .Variable = temp,
                };
                try instructions.append(instr);
                const boxed = try renderer.allocator.create(tac.BoxedVal);
                boxed.* = .{ .PlainVal = lhsVal };
                return boxed;
            },
            .Binary => |binary| {
                const one = &tac_ONE;
                const zero = &tac_ZERO;
                const storeTemp = try renderer.allocator.create(tac.Val);
                storeTemp.* = tac.Val{ .Variable = try tempGen.genTemp(renderer.allocator) };
                const storeTempName = storeTemp.Variable;
                std.log.warn("storeTempName: {s}\n", .{storeTempName});
                const boxed = try renderer.allocator.create(tac.BoxedVal);
                boxed.* = tac.BoxedVal{ .PlainVal = storeTemp };
                if (binary.op == BinOp.LOGIC_AND or binary.op == BinOp.LOGIC_OR) {
                    const asmSymbol = try getASMSymFromType(
                        binary.type.?,
                        renderer.allocator,
                        .{ .disableFloat = true },
                    );
                    try renderer.asmSymbolTable.put(storeTempName, asmSymbol);
                    const valLeft = try binary.lhs.genTACInstructionsAndCvt(renderer, instructions);

                    if (binary.op == BinOp.LOGIC_AND) {
                        const falseLabel = try std.fmt.allocPrint(renderer.allocator, "falseLabel_{d}", .{tempGen.genId()});
                        const endLabel = try std.fmt.allocPrint(renderer.allocator, "endLabel_{d}", .{tempGen.genId()});
                        try instructions.append(try tac.createInst(
                            .JumpIfZero,
                            tac.Jmp{
                                .condition = valLeft,
                                .target = falseLabel,
                            },
                            renderer.allocator,
                        ));

                        const valRight = try binary.rhs.genTACInstructionsAndCvt(renderer, instructions);
                        // After generating left, we dont evaluate right if its
                        // zero
                        const noLeftZeroInstructions = [_]*tac.Instruction{
                            try tac.createInst(.JumpIfZero, tac.Jmp{ .condition = valRight, .target = falseLabel }, renderer.allocator),
                            try tac.createInst(.Copy, tac.Copy{ .src = one, .dest = storeTemp }, renderer.allocator),
                            try tac.createInst(.Jump, endLabel, renderer.allocator),
                            try tac.createInst(.Label, falseLabel, renderer.allocator),
                            try tac.createInst(.Copy, tac.Copy{ .src = zero, .dest = storeTemp }, renderer.allocator),
                            try tac.createInst(.Label, endLabel, renderer.allocator),
                        };
                        try instructions.appendSlice(&noLeftZeroInstructions);
                        return boxed;
                    }
                    if (binary.op == BinOp.LOGIC_OR) {
                        const endLabel = try std.fmt.allocPrint(renderer.allocator, "endLabel_{d}", .{tempGen.genId()});
                        const trueLabel = try std.fmt.allocPrint(renderer.allocator, "trueLabel_{d}", .{tempGen.genId()});
                        try instructions.append(try tac.createInst(
                            .JumpIfNotZero,
                            tac.Jmp{ .condition = valLeft, .target = trueLabel },
                            renderer.allocator,
                        ));
                        const valRight = try binary.rhs.genTACInstructionsAndCvt(renderer, instructions);
                        // Left is not one
                        const leftNotOneInstructions = [_]*tac.Instruction{
                            try tac.createInst(.JumpIfNotZero, tac.Jmp{ .condition = valRight, .target = trueLabel }, renderer.allocator),
                            try tac.createInst(.Copy, tac.Copy{ .src = zero, .dest = storeTemp }, renderer.allocator),
                            try tac.createInst(.Jump, endLabel, renderer.allocator),
                            try tac.createInst(.Label, trueLabel, renderer.allocator),
                            try tac.createInst(.Copy, tac.Copy{ .src = one, .dest = storeTemp }, renderer.allocator),
                            try tac.createInst(.Label, endLabel, renderer.allocator),
                        };
                        try instructions.appendSlice(&leftNotOneInstructions);
                        return boxed;
                    }
                    unreachable;
                }
                const valLeft = try binary.lhs.genTACInstructionsAndCvt(renderer, instructions);
                const valRight = try binary.rhs.genTACInstructionsAndCvt(renderer, instructions);
                const asmSymbol = try getASMSymFromType(
                    binary.type.?,
                    renderer.allocator,
                    .{ .disableFloat = false },
                );
                try renderer.asmSymbolTable.put(storeTempName, asmSymbol);
                try instructions.append(try tac.createInst(.Binary, tac.Binary{
                    .op = tacBinOpFromASTBinOp(binary.op),
                    .left = valLeft,
                    .right = valRight,
                    .dest = storeTemp,
                }, renderer.allocator));
                return boxed;
            },
            .Identifier => |iden| {
                const tacVal = try renderer.allocator.create(tac.Val);
                const boxed = try renderer.allocator.create(tac.BoxedVal);
                tacVal.* = tac.Val{ .Variable = iden.name };
                boxed.* = .{ .PlainVal = tacVal };
                return boxed;
            },
            .Assignment => |assignment| {
                // INFO: Assignments can be copies or stores in TAC
                // If the lhs is dereferenced, then it is a store
                // Otherwise, it is a copy
                const lhs = try assignment.lhs.genTACInstructions(renderer, instructions);
                const rhs = try assignment.rhs.genTACInstructionsAndCvt(renderer, instructions);
                switch (lhs.*) {
                    .PlainVal => |val| {
                        try instructions.append(try tac.createInst(.Copy, tac.Copy{
                            .src = rhs,
                            .dest = val,
                        }, renderer.allocator));
                    },
                    .DerefedVal => |val| {
                        try instructions.append(try tac.createInst(.Store, tac.Store{
                            .src = rhs,
                            .destPointer = val,
                        }, renderer.allocator));
                    },
                }
                return lhs;
            },
            .Ternary => |ternary| {
                const comparision = try ternary.condition.genTACInstructionsAndCvt(renderer, instructions);
                const storeTemp = try renderer.allocator.create(tac.Val);
                storeTemp.* = tac.Val{
                    .Variable = try tempGen.genTemp(renderer.allocator),
                };
                const asmSymbol = try getASMSymFromType(
                    ternary.type.?,
                    renderer.allocator,
                    .{ .disableFloat = true },
                );
                try renderer.asmSymbolTable.put(storeTemp.Variable, asmSymbol);
                const boxed = try renderer.allocator.create(tac.BoxedVal);
                boxed.* = tac.BoxedVal{ .PlainVal = storeTemp };

                const falseLabelName = try std.fmt.allocPrint(renderer.allocator, "falseLabel_{d}", .{tempGen.genId()});
                const endLabelName = try std.fmt.allocPrint(renderer.allocator, "endLabel_{d}", .{tempGen.genId()});

                try instructions.append(
                    try tac.createInst(
                        .JumpIfZero,
                        tac.Jmp{ .condition = comparision, .target = falseLabelName },
                        renderer.allocator,
                    ),
                );
                const middle = try ternary.lhs.genTACInstructionsAndCvt(renderer, instructions);
                try instructions.appendSlice(&[_]*tac.Instruction{
                    try tac.createInst(.Copy, tac.Copy{ .src = middle, .dest = storeTemp }, renderer.allocator),
                    try tac.createInst(.Jump, endLabelName, renderer.allocator),
                    try tac.createInst(.Label, falseLabelName, renderer.allocator),
                });
                const end = try ternary.rhs.genTACInstructionsAndCvt(renderer, instructions);
                try instructions.appendSlice(&[_]*tac.Instruction{
                    try tac.createInst(.Copy, tac.Copy{ .src = end, .dest = storeTemp }, renderer.allocator),
                    try tac.createInst(.Label, endLabelName, renderer.allocator),
                });
                return boxed;
            },
            .FunctionCall => |fnCall| {
                const storeTemp = try renderer.allocator.create(tac.Val);
                storeTemp.* = tac.Val{
                    .Variable = try tempGen.genTemp(renderer.allocator),
                };
                const asmSymbol = try getASMSymFromType(
                    fnCall.type.?,
                    renderer.allocator,
                    .{ .disableFloat = false },
                );
                try renderer.asmSymbolTable.put(storeTemp.Variable, asmSymbol);
                const tacFnCall = try renderer.allocator.create(tac.Instruction);
                var tacFnCallArgs = std.ArrayList(*tac.Val).init(renderer.allocator);
                for (fnCall.args.items) |arg| {
                    std.log.warn("converting {any} to tac", .{arg});
                    try tacFnCallArgs.append(try arg.genTACInstructionsAndCvt(renderer, instructions));
                }
                tacFnCall.* = .{ .FunctionCall = .{
                    .name = fnCall.name,
                    .args = tacFnCallArgs,
                    .dest = storeTemp,
                } };
                try instructions.append(tacFnCall);
                const boxed = try renderer.allocator.create(tac.BoxedVal);
                boxed.* = tac.BoxedVal{ .PlainVal = storeTemp };
                return boxed;
            },
            .ArrSubscript => |arrSubscript| {
                std.log.warn("Converting to tac, arr = {any}, index = {any}\n", .{ arrSubscript.arr, arrSubscript.index });
                const arr = try arrSubscript.arr.genTACInstructionsAndCvt(renderer, instructions);
                const index = try arrSubscript.index.genTACInstructionsAndCvt(renderer, instructions);
                const arrAtIndexPtr = try renderer.allocator.create(tac.Val);
                arrAtIndexPtr.* = tac.Val{
                    .Variable = try tempGen.genTemp(renderer.allocator),
                };
                const arrAtIndexPtrType = arrSubscript.arr.getType();
                const asmSymbolForArrAtIndexPtr = try getASMSymFromType(
                    arrAtIndexPtrType,
                    renderer.allocator,
                    .{ .disableFloat = false },
                );
                try renderer.asmSymbolTable.put(arrAtIndexPtr.Variable, asmSymbolForArrAtIndexPtr);

                const storeTemp = try renderer.allocator.create(tac.Val);
                storeTemp.* = tac.Val{
                    .Variable = try tempGen.genTemp(renderer.allocator),
                };
                const storeType = arrSubscript.type.?;
                const asmSymbol = try getASMSymFromType(
                    storeType,
                    renderer.allocator,
                    .{ .disableFloat = false },
                );
                try renderer.asmSymbolTable.put(storeTemp.Variable, asmSymbol);

                try instructions.appendSlice(&[_]*tac.Instruction{
                    try tac.createInst(.AddPtr, tac.AddPtr{
                        .dest = arrAtIndexPtr,
                        .index = index,
                        .src = arr,
                        .scale = @intCast(storeType.size()),
                    }, renderer.allocator),
                });
                const boxed = try renderer.allocator.create(tac.BoxedVal);
                boxed.* = .{ .DerefedVal = arrAtIndexPtr };
                return boxed;
            },
        }
    }
};

pub fn expressionScopeVariableResolve(self: *VarResolver, expression: *Expression, currentScope: u32) VarResolveError!void {
    switch (expression.*) {
        .Constant => {},
        .Identifier => |identifier| {
            if (self.lookup(identifier.name)) |resolvedSym| {
                std.log.warn("resolved sym: {s} = {s}\n", .{ identifier.name, resolvedSym.newName });
                expression.Identifier.name = resolvedSym.newName;
            }
        },
        .Assignment => |assignment| {
            if (!assignment.lhs.isLvalue()) {
                std.log.warn("Non lvalue in assignment\n", .{});
                return VarResolveError.NonLvalue;
            }
            try expressionScopeVariableResolve(self, assignment.lhs, currentScope);
            try expressionScopeVariableResolve(self, assignment.rhs, currentScope);
        },
        .Ternary => |ternary| {
            try expressionScopeVariableResolve(self, ternary.condition, currentScope);
            try expressionScopeVariableResolve(self, ternary.lhs, currentScope);
            try expressionScopeVariableResolve(self, ternary.rhs, currentScope);
        },
        .Unary => |unary| {
            try expressionScopeVariableResolve(self, unary.exp, currentScope);
        },
        .Binary => |binary| {
            try expressionScopeVariableResolve(self, binary.lhs, currentScope);
            try expressionScopeVariableResolve(self, binary.rhs, currentScope);
        },
        .FunctionCall => |fnCall| {
            for (fnCall.args.items) |arg| {
                try expressionScopeVariableResolve(self, arg, currentScope);
            }
        },
        .Cast => |cast| {
            try expressionScopeVariableResolve(self, cast.value, currentScope);
        },
        .AddrOf => |addrOf| {
            if (!addrOf.exp.isLvalue()) {
                std.log.warn("Non lvalue in address of\n", .{});
                return VarResolveError.NonLvalue;
            }
            try expressionScopeVariableResolve(self, addrOf.exp, currentScope);
        },
        .Deref => |deref| {
            try expressionScopeVariableResolve(self, deref.exp, currentScope);
        },
        .ArrSubscript => |arrSubscript| {
            try expressionScopeVariableResolve(self, arrSubscript.arr, currentScope);
            try expressionScopeVariableResolve(self, arrSubscript.index, currentScope);
        },
    }
}

pub const VarResolveError = error{
    ConflicingVarDeclaration,
    OutOfMemory,
    NonLvalue,
} || DeclaratorError;

pub fn blockStatementScopeVariableResolve(self: *VarResolver, blockItem: *BlockItem, currentScope: u32) VarResolveError!void {
    switch (blockItem.*) {
        .Statement => |statement| {
            try statementScopeVariableResolve(self, statement, currentScope);
        },
        .Declaration => |decl| {
            // INFO: the pointer only affects the type and not the name of the
            // symbol

            const identDecl = try decl.declarator.unwrapIdentDecl();
            if (self.lookup(identDecl.Ident)) |varResolved| {
                // INFO: if there is no linkage for the resolved variable
                // If it is global and the storage class is extern, that would
                // give an error
                // when is this false?
                // 1.no linkage (local and it already exists in the map) and
                // is extern (conflicting)
                // For reference this C program:
                // int main(){
                //    int k;
                //    extern int k;
                //    return k;
                // }
                //
                // INFO: That means the earlier resolved one is also local, which is
                //  conficting
                // 2. not extern, and a variable exists with the same
                // name in the current scope
                //   This does not error in a lot of compilers, but we should
                //   throw an error here, cause they are going to be distinct
                //   variables
                //
                // INFO:
                // 3. hasLinkage before in current scope and is extern now
                // a double extern definition:
                // int main(){
                //     extern int k;
                //     extern int k;
                //     return k;
                // }
                // INFO:
                // 4. No linkage and is not extern, duplicate definitions, throw
                // an error
                // Redefinition of variable, throw error

                // zig fmt: off
                if ((varResolved.level == (self.varMap.items.len - 1))
                    and !(varResolved.hasLinkage and (decl.storageClass == Qualifier.EXTERN)))
                    return VarResolveError.ConflicingVarDeclaration;
                // zig fmt: on
            }

            if (decl.storageClass == Qualifier.EXTERN) {
                const sym = try self.allocator.create(VarResolveSymInfo);
                sym.* = .{
                    .level = @intCast(self.varMap.items.len - 1),
                    .hasLinkage = true,
                    .newName = identDecl.Ident,
                };
                try self.varMap.getLast().put(identDecl.Ident, sym);
                return;
            }
            const sym = try self.allocator.create(VarResolveSymInfo);
            sym.* = .{
                .level = @intCast(self.varMap.items.len - 1),
                .hasLinkage = false,
                .newName = (try std.fmt.allocPrint(self.allocator, "{s}{d}", .{ identDecl.Ident, currentScope })),
            };

            std.log.warn("Pushing in {s} = {any}\n", .{ identDecl.Ident, sym });
            try self.varMap.getLast().put(identDecl.Ident, sym);
            identDecl.Ident = sym.newName;
            if (decl.varInitValue) |varInitValue| {
                try varInitValueScopeVariableResolve(self, varInitValue, currentScope);
            }
        },
    }
}

pub fn varInitValueScopeVariableResolve(self: *VarResolver, varInitValue: *Initializer, currentScope: u32) VarResolveError!void {
    switch (varInitValue.*) {
        .ArrayExpr => |arrayExpr| {
            for (arrayExpr.initializers.items) |initValue| {
                try varInitValueScopeVariableResolve(self, initValue, currentScope);
            }
        },
        .Expression => |expression| {
            try expressionScopeVariableResolve(self, expression, currentScope);
        },
    }
}

pub fn statementScopeVariableResolve(self: *VarResolver, statement: *Statement, currentScope: u32) VarResolveError!void {
    switch (statement.*) {
        .Compound => |compound| {
            var newScope = std.StringHashMap(*VarResolveSymInfo).init(self.allocator);
            try self.varMap.append(&newScope);
            for (compound.items) |blockItemCompound| {
                try blockStatementScopeVariableResolve(self, blockItemCompound, tempGen.genId());
            }
            _ = self.varMap.pop();
        },
        .Null, .Label, .Goto => {},
        .If => |ifStmt| {
            try expressionScopeVariableResolve(self, ifStmt.condition, currentScope);
            try statementScopeVariableResolve(self, ifStmt.thenStmt, currentScope);
            if (ifStmt.elseStmt) |elseStmt| {
                try statementScopeVariableResolve(self, elseStmt, currentScope);
            }
        },
        .Return => |ret| {
            try expressionScopeVariableResolve(self, ret.expression, currentScope);
        },
        .Expression => |expression| {
            try expressionScopeVariableResolve(self, expression, currentScope);
        },
        .DoWhile => |doWhile| {
            try expressionScopeVariableResolve(self, doWhile.condition, currentScope);
            try statementScopeVariableResolve(self, doWhile.body, currentScope);
        },
        .While => |whileStmt| {
            try expressionScopeVariableResolve(self, whileStmt.condition, currentScope);
            try statementScopeVariableResolve(self, whileStmt.body, currentScope);
        },
        .For => |forStmt| {
            if (std.meta.activeTag(forStmt.init.*) == .Expression) {
                try expressionScopeVariableResolve(self, forStmt.init.Expression, currentScope);
            }
            if (forStmt.condition) |condition|
                try expressionScopeVariableResolve(self, condition, currentScope);
            if (forStmt.post) |post|
                try expressionScopeVariableResolve(self, post, currentScope);
            try statementScopeVariableResolve(self, forStmt.body, currentScope);
        },
        .Break => {},
        .Continue => {},
    }
}

pub const VarResolveSymInfo = struct {
    level: i32 = 0,
    newName: []u8,
    hasLinkage: bool,
};

pub const VarResolver = struct {
    varMap: std.ArrayList(*std.StringHashMap(*VarResolveSymInfo)),
    allocator: std.mem.Allocator,
    const Self = @This();
    pub fn init(allocator: std.mem.Allocator) MemoryError!*VarResolver {
        const self = try allocator.create(VarResolver);
        self.* = .{
            .varMap = std.ArrayList(*std.StringHashMap(*VarResolveSymInfo)).init(allocator),
            .allocator = allocator,
        };
        return self;
    }

    pub fn lookup(self: *Self, name: []u8) ?*VarResolveSymInfo {
        var i: usize = self.varMap.items.len - 1;
        while (i >= 0) {
            if (self.varMap.items[i].get(name)) |resolved| {
                return resolved;
            }
            if (i == 0) break; // This is required cause integer overflows
            i -= 1;
        }
        return null;
    }

    fn resolveDeclarator(self: *VarResolver, declarator: *Declarator, currentScope: u32) VarResolveError!void {
        // Modifies the declarator in place, renamed as per the current scope
        // passed in
        switch (declarator.*) {
            .Ident => |ident| {
                const argVarResolveSym = try self.allocator.create(VarResolveSymInfo);
                argVarResolveSym.* = .{
                    .level = @intCast(self.varMap.items.len - 1),
                    .newName = (try std.fmt.allocPrint(self.allocator, "{s}{d}", .{ ident, currentScope })),
                    .hasLinkage = false,
                };
                try self.varMap.getLast().put(ident, argVarResolveSym);
                declarator.Ident = argVarResolveSym.newName;
            },
            .PointerDeclarator => |pointerDeclarator| {
                try self.resolveDeclarator(pointerDeclarator, currentScope);
            },
            .FunDeclarator => |funcDeclarator| {
                try self.resolveDeclarator(funcDeclarator.declarator, currentScope);
            },
            .ArrayDeclarator => |arrDeclarator| {
                try self.resolveDeclarator(arrDeclarator.declarator, currentScope);
            },
        }
    }

    pub fn resolve(self: *VarResolver, program: *Program) VarResolveError!void {
        var globalScope = std.StringHashMap(*VarResolveSymInfo).init(self.allocator);
        try self.varMap.append(&globalScope);
        const id = tempGen.genId();
        for (program.externalDecls.items) |externalDecl| {
            switch (externalDecl.*) {
                .FunctionDecl => |functionDecl| {
                    var scope = std.StringHashMap(*VarResolveSymInfo).init(self.allocator);
                    try self.varMap.append(&scope);
                    const funDeclarator = try functionDecl.declarator.unwrapFuncDeclarator();
                    for (funDeclarator.params.items) |arg| {
                        std.debug.assert(std.meta.activeTag(arg.*) == .NonVoidArg);
                        try self.resolveDeclarator(arg.NonVoidArg.declarator, id);
                    }
                    for (functionDecl.blockItems.items) |blockItem| {
                        try blockStatementScopeVariableResolve(
                            self,
                            blockItem,
                            id,
                        );
                    }
                    _ = self.varMap.pop();
                },
                .VarDeclaration => |decl| {
                    const varSymInfo = try self.allocator.create(VarResolveSymInfo);
                    const identDecl = try decl.declarator.unwrapIdentDecl();
                    varSymInfo.* = VarResolveSymInfo{
                        .level = @intCast(self.varMap.items.len - 1),
                        .newName = identDecl.Ident,
                        .hasLinkage = true,
                    };
                    try self.varMap.getLast().put(identDecl.Ident, varSymInfo);
                },
            }
        }
    }
};

pub fn statementLoopLabelPass(statement: *Statement, loopId: u32, allocator: std.mem.Allocator) MemoryError!void {
    switch (statement.*) {
        .For => |forStmt| {
            const newLoopId = tempGen.genId();
            statement.For.loopId = newLoopId;
            try statementLoopLabelPass(forStmt.body, loopId, allocator);
        },
        .Compound => |blockItems| {
            for (blockItems.items) |blockItem| {
                try blockItemLoopLabelPass(blockItem, loopId, allocator);
            }
        },
        .While => |whileStmt| {
            const newLoopId = tempGen.genId();
            statement.While.loopId = newLoopId;
            try statementLoopLabelPass(whileStmt.body, newLoopId, allocator);
        },
        .DoWhile => |doWhile| {
            const newLoopId = tempGen.genId();
            statement.DoWhile.loopId = newLoopId;
            try statementLoopLabelPass(doWhile.body, newLoopId, allocator);
        },
        .Break => {
            statement.Break = loopId;
        },
        .Continue => {
            statement.Continue = loopId;
        },
        .If => |ifStmt| {
            try statementLoopLabelPass(ifStmt.thenStmt, loopId, allocator);
            if (ifStmt.elseStmt) |elseStmt| {
                try statementLoopLabelPass(elseStmt, loopId, allocator);
            }
        },
        else => {},
    }
}

pub fn blockItemLoopLabelPass(blockItem: *BlockItem, loopId: u32, allocator: std.mem.Allocator) MemoryError!void {
    switch (blockItem.*) {
        .Statement => |statement| {
            try statementLoopLabelPass(statement, loopId, allocator);
        },
        .Declaration => {},
    }
}

pub fn loopLabelPass(program: *Program, allocator: std.mem.Allocator) MemoryError!void {
    for (program.externalDecls.items) |externalDecl| {
        switch (externalDecl.*) {
            .VarDeclaration => {},
            .FunctionDecl => |functionDecl| {
                for (functionDecl.blockItems.items) |blockItem| {
                    try blockItemLoopLabelPass(blockItem, tempGen.genId(), allocator);
                }
            },
        }
    }
}

pub const Node = union(enum) {
    Program: *Program,
    Declaration: *Declaration,
    FunctionDef: *FunctionDef,
    Statement: *Statement,
    Expression: *Expression,
};
