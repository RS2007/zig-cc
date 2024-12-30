const std = @import("std");
const lexer = @import("./lexer.zig");
const parser = @import("./parser.zig");
const tac = @import("./TAC.zig");
const semantic = @import("./semantic.zig");

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
    pub fn genTAC(externalDecl: *Self, renderer: *TACRenderer, symbolTable: std.StringHashMap(*semantic.Symbol), allocator: std.mem.Allocator) CodegenError!?*tac.FunctionDef {
        switch (externalDecl.*) {
            .FunctionDecl => |functionDecl| {
                if (!functionDecl.isDefined()) return null;
                const tacFunctionDef = try allocator.create(tac.FunctionDef);
                var instructions = std.ArrayList(*tac.Instruction).init(allocator);
                try functionDecl.genTAC(renderer, &instructions, symbolTable, allocator);
                tacFunctionDef.* = .{
                    .name = functionDecl.declarator.FunDeclarator.declarator.Ident,
                    .args = std.ArrayList([]u8).init(allocator),
                    .instructions = instructions,
                    //TODO: Change this later
                    .global = true,
                };
                for (functionDecl.declarator.FunDeclarator.params.items) |arg| {
                    std.debug.assert(arg.NonVoidArg.declarator.* == .Ident);
                    try tacFunctionDef.args.append(arg.NonVoidArg.declarator.Ident);
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

    pub fn genTAC(self: Self, renderer: *TACRenderer, instructions: *std.ArrayList(*tac.Instruction), symbolTable: *std.StringHashMap(*semantic.Symbol), allocator: std.mem.Allocator) CodegenError!void {
        switch (self) {
            .Statement => |stmt| {
                try stmt.genTACInstructions(renderer, instructions, symbolTable, allocator);
            },
            .Declaration => |decl| {
                try decl.genTACInstructions(renderer, instructions, symbolTable, allocator);
            },
        }
    }
};

pub const Declaration = struct {
    declarator: *Declarator,
    type: Type,
    expression: ?*Expression,
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
    pub fn genTACInstructions(self: Self, renderer: *TACRenderer, instructions: *std.ArrayList(*tac.Instruction), symbolTable: *std.StringHashMap(*semantic.Symbol), allocator: std.mem.Allocator) CodegenError!void {
        const hasExpr = self.expression;
        if (hasExpr) |expression| {
            if (symbolTable.get(self.declarator.Ident)) |sym| {
                if (sym.attributes == .StaticAttr) {
                    return;
                }
            }
            const rhs = try expression.genTACInstructions(renderer, instructions, allocator);
            const instr = try allocator.create(tac.Instruction);
            const lhs = try allocator.create(tac.Val);
            lhs.* = tac.Val{ .Variable = self.declarator.Ident };
            instr.* = tac.Instruction{ .Copy = tac.Copy{ .src = rhs, .dest = lhs } };
            try instructions.append(instr);
        }
    }
    pub fn fixReturnType(self: *Self, allocator: std.mem.Allocator) !void {
        const tentativeType = self.type;
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
        }
    }
};

pub const CodegenError = error{
    OutOfMemory,
    NoSpaceLeft,
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
                                else => unreachable,
                            },
                            .type = switch (value.typeInfo) {
                                .Integer => .Integer,
                                .Long => .Long,
                                .ULong => .ULong,
                                .UInteger => .UInt,
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

pub const Type = union(enum) {
    Integer,
    Void,
    Long,
    UInteger,
    ULong,
    Float,
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

    pub inline fn size(self: Self) usize {
        return switch (self) {
            .Integer, .UInteger => 4,
            .Pointer, .Long, .ULong => 8,
            else => unreachable,
        };
    }

    pub inline fn signed(self: Self) bool {
        return switch (self) {
            .Long, .Integer => true,
            .UInteger, .ULong => false,
            .Float => false,
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
            .Pointer => |ptr| {
                try writer.print("*{any}", .{ptr});
            },
        }
    }
};

pub const NonVoidArg = struct {
    type: Type,
    declarator: *Declarator,
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

    pub fn genTAC(functionDef: FunctionDef, renderer: *TACRenderer, instructions: *std.ArrayList(*tac.Instruction), symbolTable: std.StringHashMap(*semantic.Symbol), allocator: std.mem.Allocator) CodegenError!void {
        for (functionDef.blockItems.items) |blockItem| {
            try blockItem.genTAC(renderer, instructions, @constCast(&symbolTable), allocator);
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
    pub fn genTACInstructions(forInit: *ForInit, renderer: *TACRenderer, instructions: *std.ArrayList(*tac.Instruction), symbolTable: *std.StringHashMap(*semantic.Symbol), allocator: std.mem.Allocator) CodegenError!void {
        switch (forInit.*) {
            .Declaration => |decl| {
                try decl.genTACInstructions(renderer, instructions, symbolTable, allocator);
            },
            .Expression => |expr| {
                _ = try expr.genTACInstructions(renderer, instructions, allocator);
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
        else => unreachable,
    };
}

inline fn chooseIntCastInst(inner: *tac.Val, dest: *tac.Val, toType: Type, asmSymbolTable: *std.StringHashMap(*assembly.Symbol)) CodegenError!tac.Instruction {
    return switch (toType) {
        .Integer, .UInteger => .{ .Truncate = .{
            .src = inner,
            .dest = dest,
        } },
        .Long, .ULong => if (toType.signed())
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

    //pub fn format(
    //    self: Statement,
    //    comptime fmt: []const u8,
    //    options: std.fmt.FormatOptions,
    //    writer: anytype,
    //) !void {
    //    _ = fmt;
    //    _ = options;
    //    switch (self) {
    //        .Return => |ret| {
    //            try writer.print("\n return {any}", .{ret.expression});
    //        },
    //        else => |captured| {
    //            try writer.print("\n{any}", .{captured});
    //        },
    //    }
    //}

    pub fn genTACInstructions(statement: *Statement, renderer: *TACRenderer, instructions: *std.ArrayList(*tac.Instruction), symbolTable: *std.StringHashMap(*semantic.Symbol), allocator: std.mem.Allocator) CodegenError!void {
        switch (statement.*) {
            .Return => |retStmt| {
                const returnSymbol = try retStmt.expression.genTACInstructions(renderer, instructions, allocator);
                try instructions.append(try tac.createInst(
                    .Return,
                    tac.Return{
                        .val = returnSymbol,
                    },
                    allocator,
                ));
            },
            .Expression => |expr| {
                _ = try expr.genTACInstructions(renderer, instructions, allocator);
            },
            .Null => {},
            .If => |ifStmt| {
                const falseLabelName = try std.fmt.allocPrint(allocator, "falseLabel_{d}", .{tempGen.genId()});
                const exitLabelName = try std.fmt.allocPrint(allocator, "exitLabel_{d}", .{tempGen.genId()});
                const condVal = try ifStmt.condition.genTACInstructions(renderer, instructions, allocator);
                try instructions.append(
                    try tac.createInst(
                        .JumpIfZero,
                        tac.Jmp{
                            .condition = condVal,
                            .target = falseLabelName,
                        },
                        allocator,
                    ),
                );
                try ifStmt.thenStmt.genTACInstructions(renderer, instructions, symbolTable, allocator);
                try instructions.append(try tac.createInst(.Jump, exitLabelName, allocator));
                try instructions.append(try tac.createInst(.Label, falseLabelName, allocator));
                if (ifStmt.elseStmt) |elseStmt| {
                    try elseStmt.genTACInstructions(renderer, instructions, symbolTable, allocator);
                }
                try instructions.append(try tac.createInst(.Label, exitLabelName, allocator));
            },
            .Label => |label| {
                try instructions.append(try tac.createInst(.Label, label, allocator));
            },
            .Goto => |goto| {
                try instructions.append(try tac.createInst(.Jump, goto, allocator));
            },
            .Compound => |compound| {
                for (compound.items) |compoundStatement| {
                    try compoundStatement.genTAC(renderer, instructions, symbolTable, allocator);
                }
            },
            .Continue => |cont| {
                try instructions.append(try tac.createInst(
                    .Jump,
                    try std.fmt.allocPrint(allocator, "loop_start_{d}", .{cont}),
                    allocator,
                ));
            },
            .Break => |brk| {
                try instructions.append(try tac.createInst(
                    .Jump,
                    try std.fmt.allocPrint(allocator, "loop_end_{d}", .{brk}),
                    allocator,
                ));
            },
            .DoWhile => |doWhile| {
                const doWhileStartLabel = try std.fmt.allocPrint(allocator, "loop_start_{d}", .{doWhile.loopId});
                try instructions.append(try tac.createInst(
                    .Label,
                    doWhileStartLabel,
                    allocator,
                ));
                try doWhile.body.genTACInstructions(renderer, instructions, symbolTable, allocator);
                const condition = try doWhile.condition.genTACInstructions(renderer, instructions, allocator);
                try instructions.append(try tac.createInst(.JumpIfNotZero, tac.Jmp{
                    .condition = condition,
                    .target = doWhileStartLabel,
                }, allocator));
                const doWhileEndLabel = try std.fmt.allocPrint(allocator, "loop_end_{d}", .{doWhile.loopId});
                try instructions.append(try tac.createInst(
                    .Label,
                    doWhileEndLabel,
                    allocator,
                ));
            },
            .While => |whileStmt| {
                // INFO: The first optimization: This should be an if condition
                // followed by the do while loop assembly to save jumps
                const conditionOfIf = try whileStmt.condition.genTACInstructions(renderer, instructions, allocator);
                const whileEndLabelName = try std.fmt.allocPrint(allocator, "loop_end_{d}", .{whileStmt.loopId});
                const whileStartLabelName = try std.fmt.allocPrint(allocator, "loop_start_{d}", .{whileStmt.loopId});
                try instructions.appendSlice(
                    &[_]*tac.Instruction{
                        try tac.createInst(.JumpIfZero, tac.Jmp{
                            .condition = conditionOfIf,
                            .target = whileEndLabelName,
                        }, allocator),
                        try tac.createInst(
                            .Label,
                            whileStartLabelName,
                            allocator,
                        ),
                    },
                );
                try whileStmt.body.genTACInstructions(renderer, instructions, symbolTable, allocator);
                const conditionForInnerDoWhile = try whileStmt.condition.genTACInstructions(renderer, instructions, allocator);
                try instructions.appendSlice(
                    &[_]*tac.Instruction{
                        try tac.createInst(.JumpIfNotZero, tac.Jmp{
                            .target = whileStartLabelName,
                            .condition = conditionForInnerDoWhile,
                        }, allocator),
                        try tac.createInst(.Label, whileEndLabelName, allocator),
                    },
                );
            },
            .For => |forStmt| {
                const forStartLabelName = try std.fmt.allocPrint(allocator, "loop_start_{d}", .{forStmt.loopId});
                const forEndLabelName = try std.fmt.allocPrint(allocator, "loop_end_{d}", .{forStmt.loopId});
                try forStmt.init.genTACInstructions(renderer, instructions, symbolTable, allocator);
                try instructions.append(try tac.createInst(
                    .Label,
                    forStartLabelName,
                    allocator,
                ));
                if (forStmt.condition) |condition| {
                    const condVal = try condition.genTACInstructions(renderer, instructions, allocator);
                    try instructions.append(try tac.createInst(.JumpIfZero, tac.Jmp{
                        .condition = condVal,
                        .target = forEndLabelName,
                    }, allocator));
                }
                try forStmt.body.genTACInstructions(renderer, instructions, symbolTable, allocator);
                if (forStmt.post) |post|
                    _ = try post.genTACInstructions(renderer, instructions, allocator);

                try instructions.appendSlice(&[_]*tac.Instruction{
                    try tac.createInst(.Jump, forStartLabelName, allocator),
                    try tac.createInst(.Label, forEndLabelName, allocator),
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

pub const Declarator = union(enum) {
    Ident: []u8,
    PointerDeclarator: *Declarator,
    FunDeclarator: *FunDeclarator,

    const Self = @This();
    pub fn unwrapFuncDeclarator(self: *Self) DeclaratorError!*FunDeclarator {
        switch (self.*) {
            .FunDeclarator => |funDeclarator| return funDeclarator,
            .PointerDeclarator => |pointerDeclarator| return pointerDeclarator.unwrapFuncDeclarator(),
            .Ident => return error.NoInnerFuncDeclarator,
        }
    }
    pub fn unwrapIdentDecl(self: *Self) DeclaratorError!*Declarator {
        return switch (self.*) {
            .Ident => @constCast(self),
            .PointerDeclarator => |pointerDeclarator| pointerDeclarator.unwrapIdentDecl(),
            .FunDeclarator => DeclaratorError.FunctionDeclCantUnwrapIdent,
        };
    }
    pub fn containsFuncDeclarator(self: *Self) bool {
        return switch (self.*) {
            .FunDeclarator => true,
            .Ident => false,
            .PointerDeclarator => |pointerDeclarator| pointerDeclarator.containsFuncDeclarator(),
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
            const functionDef = try externalDecl.genTAC(self, self.astSymbolTable, self.allocator);
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
        };
    }

    pub inline fn isNullPtr(self: *Self) bool {
        return switch (self.*) {
            .Constant => |constant| @constCast(&constant).isNullPtr(),
            else => false,
        };
    }

    pub fn isLvalue(self: *Self) bool {
        return switch (self.*) {
            .Identifier, .Deref, .AddrOf, .Assignment => true,
            else => false,
        };
    }

    pub fn genTACInstructions(expression: *Expression, renderer: *TACRenderer, instructions: *std.ArrayList(*tac.Instruction), allocator: std.mem.Allocator) CodegenError!*tac.Val {
        switch (expression.*) {
            .Cast => |cast| {
                const inner = try cast.value.genTACInstructions(renderer, instructions, allocator);
                if (std.meta.activeTag(cast.value.getType()) == std.meta.activeTag(cast.type)) {
                    return inner;
                }
                const dest = try allocator.create(tac.Val);
                const destName = try tempGen.genTemp(allocator);
                const asmSymbol = try renderer.allocator.create(assembly.Symbol);
                asmSymbol.* = .{
                    .Obj = .{
                        .type = assembly.AsmType.from(Type, cast.type),
                        .signed = cast.type.signed(),
                        .static = false,
                    },
                };
                try renderer.asmSymbolTable.put(destName, asmSymbol);
                dest.* = tac.Val{ .Variable = destName };

                if (inner.getAsmTypeFromSymTab(&renderer.asmSymbolTable).? == .Float) {
                    //INFO: floats rouned to integers and longs
                    const castInst = try allocator.create(tac.Instruction);
                    castInst.* = try chooseFloatCastInst(inner, dest, cast.type);
                    try instructions.append(castInst);
                } else {
                    //INFO: cast and truncate
                    const castInst = try allocator.create(tac.Instruction);
                    castInst.* = try chooseIntCastInst(
                        inner,
                        dest,
                        cast.type,
                        @constCast(&renderer.asmSymbolTable),
                    );
                    try instructions.append(castInst);
                }
                return dest;
            },
            .AddrOf => |addrOf| {
                _ = addrOf;
                unreachable;
            },
            .Deref => |deref| {
                _ = deref;
                unreachable;
            },
            .Constant => |constant| {
                switch (constant.value) {
                    .Integer => |integer| {
                        const val = try allocator.create(tac.Val);
                        val.* = tac.Val{ .Constant = .{ .Integer = integer } };
                        return val;
                    },
                    .Long => |long| {
                        const val = try allocator.create(tac.Val);
                        val.* = tac.Val{ .Constant = .{ .Long = long } };
                        return val;
                    },
                    .UInteger => |uint| {
                        const val = try allocator.create(tac.Val);
                        val.* = .{ .Constant = .{ .UInt = uint } };
                        return val;
                    },
                    .ULong => |ulong| {
                        const val = try allocator.create(tac.Val);
                        val.* = .{ .Constant = .{ .ULong = ulong } };
                        return val;
                    },
                    .Float => |float| {
                        const val = try allocator.create(tac.Val);
                        val.* = .{ .Constant = .{ .Float = float } };
                        return val;
                    },
                }
            },
            .Unary => |unary| {
                switch (unary.unaryOp) {
                    .NEGATE => {
                        const rhsVal = try unary.exp.genTACInstructions(renderer, instructions, allocator);
                        const lhsVal = try allocator.create(tac.Val);
                        const temp = try tempGen.genTemp(allocator);
                        const asmSymbol = try renderer.allocator.create(assembly.Symbol);
                        asmSymbol.* = .{
                            .Obj = .{
                                .type = switch (unary.type.?) {
                                    .Integer, .UInteger => assembly.AsmType.LongWord,
                                    .Long, .ULong => assembly.AsmType.QuadWord,
                                    .Float => .Float,
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
                        try renderer.asmSymbolTable.put(temp, asmSymbol);
                        lhsVal.* = tac.Val{
                            .Variable = temp,
                        };
                        const instr = try allocator.create(tac.Instruction);
                        instr.* = tac.Instruction{ .Unary = tac.Unary{
                            .op = tac.UnaryOp.NEGATE,
                            .src = rhsVal,
                            .dest = lhsVal,
                        } };
                        try instructions.append(instr);
                        return lhsVal;
                    },
                    .COMPLEMENT => {
                        const rhsVal = try unary.exp.genTACInstructions(renderer, instructions, allocator);
                        const lhsVal = try allocator.create(tac.Val);
                        const temp = try tempGen.genTemp(allocator);
                        const asmSymbol = try renderer.allocator.create(assembly.Symbol);
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
                        try renderer.asmSymbolTable.put(temp, asmSymbol);
                        lhsVal.* = tac.Val{
                            .Variable = temp,
                        };
                        const instr = try allocator.create(tac.Instruction);
                        instr.* = tac.Instruction{ .Unary = tac.Unary{
                            .op = tac.UnaryOp.COMPLEMENT,
                            .src = rhsVal,
                            .dest = lhsVal,
                        } };
                        try instructions.append(instr);
                        return lhsVal;
                    },
                }
            },
            .Binary => |binary| {
                const one = try allocator.create(tac.Val);
                one.* = tac.Val{ .Constant = .{ .Integer = 1 } };
                const zero = try allocator.create(tac.Val);
                zero.* = tac.Val{ .Constant = .{ .Integer = 0 } };
                if (binary.op == BinOp.LOGIC_AND or binary.op == BinOp.LOGIC_OR) {
                    const valLeft = try binary.lhs.genTACInstructions(renderer, instructions, allocator);
                    const storeTemp = try allocator.create(tac.Val);
                    const storeTempName = try tempGen.genTemp(allocator);
                    const asmSymbol = try renderer.allocator.create(assembly.Symbol);
                    asmSymbol.* = .{
                        .Obj = .{
                            .type = switch (binary.type.?) {
                                .Integer, .UInteger => assembly.AsmType.LongWord,
                                .Long, .ULong => assembly.AsmType.QuadWord,
                                else => unreachable,
                            },
                            .signed = switch (binary.type.?) {
                                .Integer, .Long => true,
                                .UInteger, .ULong => false,
                                .Float => false,
                                else => unreachable,
                            },
                            .static = false,
                        },
                    };
                    try renderer.asmSymbolTable.put(storeTempName, asmSymbol);
                    storeTemp.* = tac.Val{ .Variable = storeTempName };
                    if (binary.op == BinOp.LOGIC_AND) {
                        const falseLabel = try std.fmt.allocPrint(allocator, "falseLabel_{d}", .{tempGen.genId()});
                        const endLabel = try std.fmt.allocPrint(allocator, "endLabel_{d}", .{tempGen.genId()});
                        const falseLabelInstr = try allocator.create(tac.Instruction);
                        falseLabelInstr.* = tac.Instruction{
                            .Label = falseLabel,
                        };
                        const jmpIfZeroLeft = try allocator.create(tac.Instruction);
                        jmpIfZeroLeft.* = tac.Instruction{ .JumpIfZero = tac.Jmp{
                            .condition = valLeft,
                            .target = falseLabel,
                        } };
                        try instructions.append(jmpIfZeroLeft);
                        const valRight = try binary.rhs.genTACInstructions(renderer, instructions, allocator);
                        const jmpIfZeroRight = try allocator.create(tac.Instruction);
                        jmpIfZeroRight.* = tac.Instruction{ .JumpIfZero = tac.Jmp{
                            .condition = valRight,
                            .target = falseLabel,
                        } };
                        try instructions.append(jmpIfZeroRight);
                        const cpOneToDest = try allocator.create(tac.Instruction);
                        cpOneToDest.* = tac.Instruction{ .Copy = tac.Copy{
                            .src = one,
                            .dest = storeTemp,
                        } };
                        try instructions.append(cpOneToDest);
                        const jmpToEnd = try allocator.create(tac.Instruction);
                        jmpToEnd.* = tac.Instruction{ .Jump = endLabel };
                        try instructions.append(jmpToEnd);
                        try instructions.append(falseLabelInstr);
                        const cpZeroToDest = try allocator.create(tac.Instruction);
                        cpZeroToDest.* = tac.Instruction{ .Copy = tac.Copy{
                            .src = zero,
                            .dest = storeTemp,
                        } };
                        try instructions.append(cpZeroToDest);
                        const endLabelInst = try allocator.create(tac.Instruction);
                        endLabelInst.* = tac.Instruction{ .Label = endLabel };
                        try instructions.append(endLabelInst);
                        return storeTemp;
                    }
                    if (binary.op == BinOp.LOGIC_OR) {
                        const endLabel = try std.fmt.allocPrint(allocator, "endLabel_{d}", .{tempGen.genId()});
                        const trueLabel = try std.fmt.allocPrint(allocator, "trueLabel_{d}", .{tempGen.genId()});
                        const trueLabelInst = try allocator.create(tac.Instruction);
                        trueLabelInst.* = tac.Instruction{
                            .Label = trueLabel,
                        };
                        const jmpIfNotZero = try allocator.create(tac.Instruction);
                        jmpIfNotZero.* = tac.Instruction{ .JumpIfNotZero = tac.Jmp{
                            .condition = valLeft,
                            .target = trueLabel,
                        } };
                        try instructions.append(jmpIfNotZero);
                        const valRight = try binary.rhs.genTACInstructions(renderer, instructions, allocator);
                        const jmpIfNotZeroRight = try allocator.create(tac.Instruction);
                        jmpIfNotZeroRight.* = tac.Instruction{ .JumpIfNotZero = tac.Jmp{
                            .condition = valRight,
                            .target = trueLabel,
                        } };
                        try instructions.append(jmpIfNotZeroRight);
                        const cpZeroToDest = try allocator.create(tac.Instruction);
                        cpZeroToDest.* = tac.Instruction{ .Copy = tac.Copy{
                            .src = zero,
                            .dest = storeTemp,
                        } };
                        try instructions.append(cpZeroToDest);
                        const jmpToEnd = try allocator.create(tac.Instruction);
                        jmpToEnd.* = tac.Instruction{ .Jump = endLabel };
                        try instructions.append(jmpToEnd);
                        try instructions.append(trueLabelInst);
                        const cpOneToDest = try allocator.create(tac.Instruction);
                        cpOneToDest.* = tac.Instruction{ .Copy = tac.Copy{
                            .src = one,
                            .dest = storeTemp,
                        } };
                        try instructions.append(cpOneToDest);
                        const endLabelInst = try allocator.create(tac.Instruction);
                        endLabelInst.* = tac.Instruction{ .Label = endLabel };
                        try instructions.append(endLabelInst);
                        return storeTemp;
                    }
                    unreachable;
                }
                const valLeft = try binary.lhs.genTACInstructions(renderer, instructions, allocator);
                const valRight = try binary.rhs.genTACInstructions(renderer, instructions, allocator);
                const storeTemp = try allocator.create(tac.Val);
                const storeTempName = try tempGen.genTemp(allocator);
                const asmSymbol = try renderer.allocator.create(assembly.Symbol);
                asmSymbol.* = .{
                    .Obj = .{
                        .type = switch (binary.type.?) {
                            .Integer, .UInteger => assembly.AsmType.LongWord,
                            .Long, .ULong => assembly.AsmType.QuadWord,
                            .Float => assembly.AsmType.Float,
                            else => unreachable,
                        },
                        .signed = switch (binary.type.?) {
                            .Integer, .Long => true,
                            .UInteger, .ULong => false,
                            .Float => false,
                            else => unreachable,
                        },
                        .static = false,
                    },
                };
                try renderer.asmSymbolTable.put(storeTempName, asmSymbol);
                storeTemp.* = tac.Val{ .Variable = storeTempName };
                const instr = try allocator.create(tac.Instruction);
                instr.* = tac.Instruction{ .Binary = tac.Binary{
                    .op = tacBinOpFromASTBinOp(binary.op),
                    .left = valLeft,
                    .right = valRight,
                    .dest = storeTemp,
                } };
                try instructions.append(instr);
                return storeTemp;
            },
            .Identifier => |iden| {
                const tacVal = try allocator.create(tac.Val);
                tacVal.* = tac.Val{
                    .Variable = iden.name,
                };
                return tacVal;
            },
            .Assignment => |assignment| {
                const lhs = try assignment.lhs.genTACInstructions(renderer, instructions, allocator);
                const rhs = try assignment.rhs.genTACInstructions(renderer, instructions, allocator);
                const cpInstr = try allocator.create(tac.Instruction);
                cpInstr.* = tac.Instruction{ .Copy = tac.Copy{
                    .src = rhs,
                    .dest = lhs,
                } };
                try instructions.append(cpInstr);
                return lhs;
            },
            .Ternary => |ternary| {
                const comparision = try ternary.condition.genTACInstructions(renderer, instructions, allocator);
                const storeTemp = try allocator.create(tac.Val);
                const storeTempName = try tempGen.genTemp(allocator);
                const asmSymbol = try renderer.allocator.create(assembly.Symbol);
                asmSymbol.* = .{
                    .Obj = .{
                        .type = switch (ternary.type.?) {
                            .Integer, .UInteger => assembly.AsmType.LongWord,
                            .Long, .ULong => assembly.AsmType.QuadWord,
                            else => unreachable,
                        },
                        .signed = switch (ternary.type.?) {
                            .Integer, .Long => true,
                            .UInteger, .ULong => false,
                            .Float => false,
                            else => unreachable,
                        },
                        .static = false,
                    },
                };
                try renderer.asmSymbolTable.put(storeTempName, asmSymbol);
                storeTemp.* = tac.Val{
                    .Variable = storeTempName,
                };
                const jmpIfZeroInst = try allocator.create(tac.Instruction);
                const endLabel = try allocator.create(tac.Instruction);
                const falseLabel = try allocator.create(tac.Instruction);
                const falseLabelName = try std.fmt.allocPrint(allocator, "falseLabel_{d}", .{tempGen.genId()});
                const endLabelName = try std.fmt.allocPrint(allocator, "endLabel_{d}", .{tempGen.genId()});
                falseLabel.* = tac.Instruction{
                    .Label = falseLabelName,
                };
                endLabel.* = tac.Instruction{
                    .Label = endLabelName,
                };
                jmpIfZeroInst.* = tac.Instruction{
                    .JumpIfZero = .{
                        .condition = comparision,
                        .target = falseLabelName,
                    },
                };
                try instructions.append(jmpIfZeroInst);
                const middle = try ternary.lhs.genTACInstructions(renderer, instructions, allocator);
                const cpMiddleToDest = try allocator.create(tac.Instruction);
                cpMiddleToDest.* = tac.Instruction{
                    .Copy = .{
                        .dest = storeTemp,
                        .src = middle,
                    },
                };
                try instructions.append(cpMiddleToDest);
                const uncondJumpFromTrue = try allocator.create(tac.Instruction);
                uncondJumpFromTrue.* = tac.Instruction{
                    .Jump = endLabelName,
                };
                try instructions.append(uncondJumpFromTrue);
                try instructions.append(falseLabel);
                const end = try ternary.rhs.genTACInstructions(renderer, instructions, allocator);
                const cpEndToDest = try allocator.create(tac.Instruction);
                cpEndToDest.* = tac.Instruction{
                    .Copy = .{
                        .src = end,
                        .dest = storeTemp,
                    },
                };
                try instructions.append(cpEndToDest);
                try instructions.append(endLabel);
                return storeTemp;
            },
            .FunctionCall => |fnCall| {
                const storeTemp = try allocator.create(tac.Val);
                const storeTempName = try tempGen.genTemp(allocator);
                const asmSymbol = try renderer.allocator.create(assembly.Symbol);
                asmSymbol.* = .{
                    .Obj = .{
                        .type = switch (fnCall.type.?) {
                            .Integer, .UInteger => .LongWord,
                            .Long, .ULong => .QuadWord,
                            .Float => .Float,
                            else => unreachable,
                        },
                        .signed = switch (fnCall.type.?) {
                            .Integer, .Long => true,
                            .UInteger, .ULong => false,
                            .Float => false,
                            else => unreachable,
                        },
                        .static = false,
                    },
                };
                try renderer.asmSymbolTable.put(storeTempName, asmSymbol);
                storeTemp.* = tac.Val{ .Variable = storeTempName };
                const tacFnCall = try allocator.create(tac.Instruction);
                var tacFnCallArgs = std.ArrayList(*tac.Val).init(allocator);
                for (fnCall.args.items) |arg| {
                    try tacFnCallArgs.append(try arg.genTACInstructions(renderer, instructions, allocator));
                }
                tacFnCall.* = .{ .FunctionCall = .{
                    .name = fnCall.name,
                    .args = tacFnCallArgs,
                    .dest = storeTemp,
                } };
                try instructions.append(tacFnCall);
                return storeTemp;
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
            if (decl.expression) |expression| {
                try expressionScopeVariableResolve(self, expression, currentScope);
            }
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
            if (std.mem.eql(u8, @tagName(forStmt.init.*), "Expression")) {
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
