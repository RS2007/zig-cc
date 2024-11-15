// Contains the semantic passes

// Do stack resolution first
// All variables have unique names
// How to do error recovery?

const std = @import("std");
const AST = @import("./AST.zig");
//const logz = @import("logz");

pub const SymAttributeKind = enum {
    FunctionAttr,
    StaticAttr,
    LocalAttr,
};

pub const InitValueKind = enum {
    Tentative, // Maybe initialized by a lter declaration
    Initial, // Initiialized
    NoInit, // No information can be made about the initialization
};

pub const ConstantKind = enum {
    Integer,
    Long,
};

pub const Constant = struct {
    type: AST.Type,
    value: union(enum) {
        Integer: u32,
        Long: u64,
    },
};

pub const InitValue = union(InitValueKind) {
    Tentative: void,
    Initial: Constant,
    NoInit: void,
};

pub const SymAttribute = union(SymAttributeKind) {
    FunctionAttr: struct { defined: bool, global: bool },
    StaticAttr: struct { init: InitValue, global: bool },
    LocalAttr: void,
};

const SemanticError = error{
    VarNotDeclared,
    OutOfMemory,
    UnknownLabel,
    NoSpaceLeft,
};

pub const TypeError = error{
    TypeMismatch,
    OutOfMemory,
    UnknownFunction,
    UnknownIdentifier,
    GlobVarRedeclaredAsFn,
    LinkageMismatch,
    FnRedefined,
    FnArgNumMismatch,
    GlobalDeclarationNotInteger,
    ExternVarDeclared,
    FnRedeclaredAsVar,
    ConflictingDeclarations,
    FnPrevDeclArgMismatch,
};

const TypeCheckerError = error{OutOfMemory} || TypeError;

pub const TypeErrorStruct = struct {
    errorType: ?TypeError,
    errorPayload: []u8,

    const Self = @This();
    inline fn typeError(allocator: std.mem.Allocator, errorType: TypeError, errorPayload: []u8) TypeCheckerError!*Self {
        const typeErrorStruct = try allocator.create(Self);
        typeErrorStruct.* = .{
            .errorType = errorType,
            .errorPayload = errorPayload,
        };
        return typeErrorStruct;
    }
};

pub const TypeKind = enum {
    Function,
    Void,
    Integer,
    Long,

    const Self = @This();
    pub fn from(self: AST.Type) Self {
        return switch (self) {
            .Integer => .Integer,
            .Void => .Void,
            .Long => .Long,
        };
    }
};
pub const FnSymbol = struct {
    args: std.ArrayList(AST.Type),
    returnType: AST.Type,
};

pub const TypeInfo = union(TypeKind) {
    Function: FnSymbol,
    //INFO: For now we just keep the count,
    // later we will keep track of the types of the args
    Void,
    Integer,
    Long,

    const Self = @This();
    pub inline fn isOfKind(self: *Self, other: TypeKind) bool {
        return std.mem.eql(u8, @tagName(self.*), std.enums.tagName(TypeKind, other).?);
    }
};

pub const Symbol = struct {
    typeInfo: TypeInfo,
    attributes: SymAttribute,
    const Self = @This();
    pub fn isDefined(self: *const Self) bool {
        if (std.meta.activeTag(self.attributes) == .FunctionAttr) {
            return self.attributes.FunctionAttr.defined;
        }
        return true;
    }
};

pub const Typechecker = struct {
    // A struct cause typecheckers requires you to store some state
    symbolTable: std.StringHashMap(*Symbol),
    allocator: std.mem.Allocator,
    const Self = @This();
    pub fn init(allocator: std.mem.Allocator) !*Self {
        const typechecker = try allocator.create(Typechecker);
        typechecker.* = .{
            .symbolTable = std.StringHashMap(*Symbol).init(allocator),
            .allocator = allocator,
        };
        return typechecker;
    }
    pub fn check(self: *Self, program: *AST.Program) TypeCheckerError!?[]u8 {
        // INFO: Typechecks a program and constructs a table
        // Then it resolves the return labels
        const typeErrorStruct = try typecheckProgram(self, program);
        if (typeErrorStruct != null and typeErrorStruct.?.errorType != null) {
            return typeErrorStruct.?.errorPayload;
        }
        try self.resolveReturns(program);
        return null;
    }

    pub fn resolveReturns(self: *Self, program: *AST.Program) !void {
        for (program.externalDecls.items) |externalDecl| {
            if (std.meta.activeTag(externalDecl.*) == .FunctionDecl) {
                for (externalDecl.FunctionDecl.blockItems.items) |blkItem| {
                    try resolveBlockReturns(self, blkItem, externalDecl.FunctionDecl.returnType);
                }
            }
        }
    }
};

pub fn resolveBlockReturns(self: *Typechecker, blockItem: *AST.BlockItem, fnReturnType: AST.Type) !void {
    if (std.meta.activeTag(blockItem.*) == .Statement and std.meta.activeTag(blockItem.Statement.*) == .Return) {
        _ = try convert(self.allocator, blockItem.Statement.Return.expression, fnReturnType);
    }
}

//1. Break typechecker down into smaller functions
pub fn typecheckProgram(self: *Typechecker, program: *AST.Program) TypeCheckerError!?*TypeErrorStruct {
    for (program.externalDecls.items) |externalDecl| {
        const hasTypeError = try typecheckExternalDecl(self, externalDecl);
        if (hasTypeError) |typeError| {
            return typeError;
        }
    }
    return null;
}

pub fn typecheckExternalDecl(self: *Typechecker, externalDecl: *AST.ExternalDecl) TypeCheckerError!?*TypeErrorStruct {
    switch (externalDecl.*) {
        // INFO: Function declarations should have the following checks:
        // 1. Return statements should be same as fn return type
        //     - This will be implemented later
        // 2. Arguments length should be the same as fn args length
        // 3. Function if declared earlier as static should not be
        // redeclared as non-static (vice versa) ✅
        // 4. Function if defined earlier, should not be redefined ✅
        // 5. Global variables should not have the same name as the function ✅
        // 6. Return type of the earlier declaration should not be
        // different, and so is the arg length
        //     - This will be implemented later
        .FunctionDecl => |functionDecl| {
            if (self.symbolTable.get(functionDecl.name)) |sym| {
                if (!sym.typeInfo.isOfKind(.Function)) {
                    return try TypeErrorStruct.typeError(
                        self.allocator,
                        TypeError.GlobVarRedeclaredAsFn,
                        (try std.fmt.allocPrint(self.allocator, "Global variable {s} redeclared as function\n", .{functionDecl.name})),
                    );
                }
                const nonStaticLinkageMismatch = !sym.attributes.FunctionAttr.global and (functionDecl.storageClass != AST.Qualifier.STATIC);
                const staticLinkageMismatch = sym.attributes.FunctionAttr.global and (functionDecl.storageClass == AST.Qualifier.STATIC);
                if (nonStaticLinkageMismatch or staticLinkageMismatch) {
                    return TypeErrorStruct.typeError(
                        self.allocator,
                        TypeError.LinkageMismatch,
                        try std.fmt.allocPrint(self.allocator, "Global variable {s} redeclared as non-static\n", .{functionDecl.name}),
                    );
                }

                if (functionDecl.isDefined() and sym.isDefined()) {
                    return TypeErrorStruct.typeError(
                        self.allocator,
                        TypeError.FnRedefined,
                        try std.fmt.allocPrint(
                            self.allocator,
                            "Function redefinition of {s}\n",
                            .{functionDecl.name},
                        ),
                    );
                }

                for (0..functionDecl.args.items.len) |i| {
                    if (functionDecl.args.items[i].NonVoidArg.type != sym.typeInfo.Function.args.items[i]) {
                        return TypeErrorStruct.typeError(
                            self.allocator,
                            TypeError.FnPrevDeclArgMismatch,
                            try std.fmt.allocPrint(self.allocator, "Argument mismatch in function redeclaration: function name = {s}, function arg mismatch between {any} and {any}\n", .{
                                functionDecl.name,
                                functionDecl.args.items[i].NonVoidArg.type,
                                sym.typeInfo.Function.args.items[i],
                            }),
                        );
                    }
                }

                if (functionDecl.args.items.len != sym.typeInfo.Function.args.items.len) {
                    return TypeErrorStruct.typeError(
                        self.allocator,
                        TypeError.FnArgNumMismatch,
                        try std.fmt.allocPrint(
                            self.allocator,
                            "Function {s} has mismatching declarations\n",
                            .{functionDecl.name},
                        ),
                    );
                }
            }
            const fnSym = try self.allocator.create(Symbol);
            var fnArgsList = std.ArrayList(AST.Type).init(self.allocator);
            for (functionDecl.args.items) |arg| {
                try fnArgsList.append(arg.NonVoidArg.type);
            }
            fnSym.* = .{
                .typeInfo = .{
                    .Function = .{
                        .args = fnArgsList,
                        .returnType = functionDecl.returnType,
                    },
                },
                .attributes = .{
                    .FunctionAttr = .{
                        .defined = (functionDecl.isDefined()),
                        .global = (functionDecl.storageClass != AST.Qualifier.STATIC),
                    },
                },
            };
            try self.symbolTable.put(functionDecl.name, fnSym);

            for (functionDecl.args.items) |arg| {
                const argSym = try self.allocator.create(Symbol);
                argSym.* = .{
                    .typeInfo = switch (TypeKind.from(arg.NonVoidArg.type)) {
                        .Void => unreachable,
                        .Integer => .Integer,
                        .Long => .Long,
                        .Function => unreachable,
                    },
                    .attributes = .LocalAttr,
                };
                std.log.warn("Pushing in {any}\n", .{arg.NonVoidArg.identifier});
                try self.symbolTable.put(arg.NonVoidArg.identifier, argSym);
            }

            for (functionDecl.blockItems.items) |blk| {
                const hasTypeErr = try typecheckBlkItem(self, blk);
                if (hasTypeErr) |typeErr| {
                    return typeErr;
                }
            }
        },
        .VarDeclaration => |varDecl| {
            // INFO: Global/File scope variables
            // 1. Check if the rhs expression is a number (reject everything)
            //    - Later implement a global evaluator pass (AST -> glob
            //    evaluator -> var resolve)
            // 2. Check the storage class:
            //    - Extern should not have an initialization expression, and it
            //    should be assigned as `NoInitializer`
            //    - Static,  if it does not have an init, should init to a
            //    default value
            // 3. If there is already a declaration with this name
            //    - Not a function
            //    - Change the attribute status if its extern
            //    - if its static and the other extern, then throw an
            //    error(check if global changed)
            // 4. If all of this passes, and if the earlier decl had an
            // initialization, throw an error, telling that there are
            // conflicting global definitions.
            // 5. If there is no initiliazer for the current case and the
            // earlier value was not a constant, then assign tentative

            var initializer: InitValue = .NoInit;
            var global = false;

            // INFO: Has expression
            if (varDecl.expression) |expression| {
                // INFO: Should be constant initialized

                if (std.meta.activeTag(expression.*) != .Constant) {
                    return TypeErrorStruct.typeError(
                        self.allocator,
                        TypeError.GlobalDeclarationNotInteger,
                        try std.fmt.allocPrint(
                            self.allocator,
                            "Global declarations only support integers for declaration of {s}\n",
                            .{varDecl.name},
                        ),
                    );
                }

                // INFO: Assign the expression value to the decl
                if (varDecl.type != expression.getType()) {
                    const castExpr = try self.allocator.create(AST.Expression);
                    castExpr.* = AST.Expression{ .Cast = .{
                        .type = varDecl.type,
                        .value = expression,
                    } };
                    varDecl.expression = castExpr;
                    const unwrappedConst: u64 = switch (expression.Constant.value) {
                        .Integer => |integer| @intCast(integer),

                        .Long => |long| long,
                    };
                    initializer = .{ .Initial = switch (varDecl.type) {
                        .Integer => .{ .type = .Integer, .value = .{ .Integer = @intCast(unwrappedConst) } },
                        .Long => .{ .type = .Long, .value = .{ .Long = @intCast(unwrappedConst) } },
                        else => unreachable,
                    } };
                } else {
                    initializer = .{ .Initial = switch (varDecl.type) {
                        .Integer => .{ .type = .Integer, .value = .{ .Integer = expression.Constant.value.Integer } },
                        .Long => .{ .type = .Long, .value = .{ .Long = expression.Constant.value.Long } },
                        else => unreachable,
                    } };
                }

                // INFO: Extern decls no assignment check
                if (varDecl.storageClass == AST.Qualifier.EXTERN) {
                    return TypeErrorStruct.typeError(
                        self.allocator,
                        TypeError.ExternVarDeclared,
                        try std.fmt.allocPrint(
                            self.allocator,
                            "Extern declarations cant have an assignment: {s}\n",
                            .{varDecl.name},
                        ),
                    );
                }
            } else {
                // INFO: Extern is not initialized (a case exists where there is
                // an older declaration, but that is handled later when the
                // symbol is found in the symbol table)
                // INFO: Tentative is given to others (later get initialized to a value by a declaration)
                // or is declared as 0 (if persists as tentative)
                initializer = if (varDecl.storageClass == AST.Qualifier.EXTERN) .NoInit else .Tentative;
            }

            global = varDecl.storageClass != AST.Qualifier.STATIC;
            if (self.symbolTable.get(varDecl.name)) |varSym| {
                //INFO: Function redeclaration as variable handling
                if (varSym.typeInfo.isOfKind(.Function)) {
                    return TypeErrorStruct.typeError(
                        self.allocator,
                        TypeError.FnRedeclaredAsVar,
                        try std.fmt.allocPrint(
                            self.allocator,
                            "Function redeclared as var: {s}\n",
                            .{varDecl.name},
                        ),
                    );
                }

                //INFO: extern inherit scope (global/static)
                if (varDecl.storageClass == AST.Qualifier.EXTERN) {
                    // inherit global attr
                    // inherit initial value
                    // TODO: straighten this switch out once the asserts have
                    // not been hit for some test cases
                    switch (varSym.attributes) {
                        .StaticAttr => |staticAttr| {
                            global = staticAttr.global;
                        },
                        else => {
                            unreachable;
                        },
                    }

                    // INFO: repeated cause extern requires this (should be
                    // a function at some point)
                    if (!varSym.typeInfo.isOfKind(TypeKind.from(varDecl.type))) {
                        return TypeErrorStruct.typeError(
                            self.allocator,
                            TypeError.TypeMismatch,
                            try std.fmt.allocPrint(
                                self.allocator,
                                "Type mismatch for declaration of {s}\n",
                                .{varDecl.name},
                            ),
                        );
                    }
                } else {
                    //INFO: if no extern, check whether earlier decl was
                    //global/static. check if the new one is the same, else
                    //throw
                    // TODO: straighten this switch out once the asserts have
                    // not been hit for some test cases
                    switch (varSym.attributes) {
                        .StaticAttr => |staticAttr| {
                            if (staticAttr.global != global) {
                                const typeErrorStruct = try self.allocator.create(TypeErrorStruct);
                                typeErrorStruct.* = .{
                                    .errorType = TypeError.ConflictingDeclarations,
                                    .errorPayload = (try std.fmt.allocPrint(
                                        self.allocator,
                                        "Static and non-static(conflicting) declarations for {s}\n",
                                        .{varDecl.name},
                                    )),
                                };
                                return typeErrorStruct;
                            }
                        },
                        else => {
                            unreachable;
                        },
                    }
                }

                if (!varSym.typeInfo.isOfKind(TypeKind.from(varDecl.type))) {
                    const typeErrorStruct = try self.allocator.create(TypeErrorStruct);
                    typeErrorStruct.* = .{
                        .errorType = TypeError.TypeMismatch,
                        .errorPayload = (try std.fmt.allocPrint(
                            self.allocator,
                            "Type mismatch for declaration of {s}\n",
                            .{varDecl.name},
                        )),
                    };
                    return typeErrorStruct;
                }

                // zig fmt: off
                if (std.mem.eql(u8, @tagName(varSym.attributes.StaticAttr.init), "Initial") 
                    and !std.mem.eql(u8, @tagName(initializer), "Initial")) {
                // zig fmt: on
                    // INFO: If initialized again choose that value, by exiting out of
                    // this block
                    initializer = varSym.attributes.StaticAttr.init;
                }
            }

            const sym = try self.allocator.create(Symbol);
            sym.* = .{
                .typeInfo = switch (varDecl.type) {
                    .Integer => .Integer,
                    .Long => .Long,
                    else => unreachable,
                },
                .attributes = .{
                    .StaticAttr = .{
                        .global = global,
                        .init = initializer,
                    },
                },
            };
            try self.symbolTable.put(varDecl.name, sym);
        },
    }
    return null;
}
fn typecheckBlkItem(self: *Typechecker, blkItem: *AST.BlockItem) TypeCheckerError!?*TypeErrorStruct {
    switch (blkItem.*) {
        .Declaration => |decl| {
            // INFO: cases when var is extern, static or local
            // Only locals are localAttrs
            // 1. if var is static: static variables live beyond their scope, since
            // they are in bss. There is an initialization, if the var is
            // already seen in a call, they are not reinitialized.
            // Non static initializers are not allowed for static local
            // variables
            // An example program:
            // void recurse(int n){
            //    static int k = 0;
            //    printf("%d\n",n);
            //    if (k < n){
            //            recurse(n-1);
            //    }}
            //  int main() {
            //        recurse(10);}
            // Output: prints from 10 to 0
            // 2.if var is extern: look if there is a declaration outside

            // Typecheck the expression
            if (decl.expression) |declExpression| {
                // INFO: Type error witin expression
                const exprType = typecheckExpr(self, declExpression) catch |err| {
                    const typeErrorStruct = try self.allocator.create(TypeErrorStruct);
                    typeErrorStruct.* = .{
                        .errorType = err,
                        .errorPayload = (try std.fmt.allocPrint(self.allocator, "Type error at for init\n", .{})),
                    };
                    return typeErrorStruct;
                };
                std.log.warn("Conversion from {any} to {any}\n", .{ exprType, decl.type });
                std.log.warn("Expression: {any}\n", .{declExpression});
                decl.expression = try convert(self.allocator, decl.expression.?, decl.type);
                std.log.warn("Expression post conversion: {any}\n", .{declExpression});
                std.debug.assert(if (decl.expression == null) true else decl.expression.?.getType() == decl.type);

                //INFO: Checking type equality
                //if (decl.type != exprType) {
                //    const typeErrorStruct = try self.allocator.create(TypeErrorStruct);
                //    typeErrorStruct.* = .{
                //        .errorType = TypeError.TypeMismatch,
                //        .errorPayload = (try std.fmt.allocPrint(
                //            self.allocator,
                //            "Type mismatch at declaration of {s}, got lhs to be {any} and rhs to be {any}\n",
                //            .{ decl.name, decl.type, exprType },
                //        )),
                //    };
                //    return typeErrorStruct;
                //}
            }

            // handle qualifiers
            if (decl.storageClass) |storageClass| {
                switch (storageClass) {
                    .EXTERN => {
                        if (decl.expression) |_| {
                            const typeErrorStruct = try self.allocator.create(TypeErrorStruct);
                            typeErrorStruct.* = .{ .errorType = TypeError.ExternVarDeclared, .errorPayload = (try std.fmt.allocPrint(
                                self.allocator,
                                "Extern variable {s} defined\n",
                                .{decl.name},
                            )) };
                            return typeErrorStruct;
                        }
                        if (self.symbolTable.get(decl.name)) |olderSym| {
                            std.log.warn("This: {any} and {any}\n", .{ olderSym.typeInfo, TypeKind.from(decl.type) });
                            if (!olderSym.typeInfo.isOfKind(TypeKind.from(decl.type))) {
                                const typeErrorStruct = try self.allocator.create(TypeErrorStruct);
                                typeErrorStruct.* = .{
                                    .errorType = TypeError.FnRedeclaredAsVar,
                                    .errorPayload = (try std.fmt.allocPrint(
                                        self.allocator,
                                        "Extern variable type error for symbol {s}\n",
                                        .{decl.name},
                                    )),
                                };
                                return typeErrorStruct;
                            }
                            if (std.mem.eql(u8, @tagName(olderSym.typeInfo), "Function")) {
                                const typeErrorStruct = try self.allocator.create(TypeErrorStruct);
                                typeErrorStruct.* = .{
                                    .errorType = TypeError.FnRedeclaredAsVar,
                                    .errorPayload = (try std.fmt.allocPrint(
                                        self.allocator,
                                        "Function {s} redefined as a variable\n",
                                        .{decl.name},
                                    )),
                                };
                            }
                        } else {
                            const sym = try self.allocator.create(Symbol);
                            sym.* = .{ .typeInfo = .Integer, .attributes = .{ .StaticAttr = .{
                                .global = true,
                                .init = .NoInit,
                            } } };
                            try self.symbolTable.put(decl.name, sym);
                        }
                    },
                    .STATIC => {
                        if (decl.expression) |expr| {
                            //@compileLog("Typename: " ++ @typeName(@TypeOf(decl.name)) ++ "\n");
                            // std.log.err("Pushing in this decl: {any}\n", .{decl.name});
                            if (!std.mem.eql(u8, @tagName(expr.*), "Constant")) {
                                const typeErrorStruct = try self.allocator.create(TypeErrorStruct);
                                typeErrorStruct.* = .{
                                    .errorType = TypeError.GlobalDeclarationNotInteger,
                                    .errorPayload = (try std.fmt.allocPrint(
                                        self.allocator,
                                        "Static variable {s} must have a constant initializer\n",
                                        .{decl.name},
                                    )),
                                };
                                return typeErrorStruct;
                            }
                            const sym = try self.allocator.create(Symbol);
                            // TODO: accomodate longs
                            sym.* = .{
                                .typeInfo = .Integer,
                                .attributes = .{ .StaticAttr = .{
                                    .init = .{ .Initial = switch (decl.type) {
                                        .Integer => .{ .type = .Integer, .value = .{ .Integer = expr.Constant.value.Integer } },
                                        .Long => .{ .type = .Long, .value = .{ .Long = expr.Constant.value.Long } },
                                        else => unreachable,
                                    } },
                                    .global = true,
                                } },
                            };
                            try self.symbolTable.put(decl.name, sym);
                        } else {
                            const sym = try self.allocator.create(Symbol);
                            sym.* = .{
                                .typeInfo = .Integer,
                                .attributes = .{ .StaticAttr = .{
                                    .init = .{ .Initial = .{ .type = .Integer, .value = .{ .Integer = 0 } } },
                                    .global = false,
                                } },
                            };
                            try self.symbolTable.put(decl.name, sym);
                        }
                    },
                }
            } else {
                // No decl
                const sym = try self.allocator.create(Symbol);
                sym.* = .{
                    .typeInfo = switch (decl.type) {
                        .Integer => .Integer,
                        .Long => .Long,
                        else => unreachable,
                    },
                    .attributes = .LocalAttr,
                };
                try self.symbolTable.put(decl.name, sym);
            }
        },
        .Statement => |stmt| {
            const stmtTypeError = try typecheckStmt(self, stmt);
            if (stmtTypeError != null) {
                return stmtTypeError.?;
            }
        },
    }
    return null;
}

fn typecheckStmt(self: *Typechecker, stmt: *AST.Statement) TypeCheckerError!?*TypeErrorStruct {
    switch (stmt.*) {
        .Return => |ret| {
            _ = typecheckExpr(self, ret.expression) catch |err| {
                const typeErrorStruct = try self.allocator.create(TypeErrorStruct);
                typeErrorStruct.* = .{
                    .errorType = err,
                    .errorPayload = (try std.fmt.allocPrint(self.allocator, "Type error at return statement\n", .{})),
                };
                return typeErrorStruct;
            };
        },
        .Expression => |expr| {
            _ = typecheckExpr(self, expr) catch |err| {
                const typeErrorStruct = try self.allocator.create(TypeErrorStruct);
                typeErrorStruct.* = .{
                    .errorType = err,
                    .errorPayload = (try std.fmt.allocPrint(self.allocator, "Type error at return statement\n", .{})),
                };
                return typeErrorStruct;
            };
        },
        .If => |ifStmt| {
            _ = typecheckExpr(self, ifStmt.condition) catch |err| {
                const typeErrorStruct = try self.allocator.create(TypeErrorStruct);
                typeErrorStruct.* = .{
                    .errorType = err,
                    .errorPayload = (try std.fmt.allocPrint(self.allocator, "Type error at if condition\n", .{})),
                };
                return typeErrorStruct;
            };
            const thenStmtErr = try typecheckStmt(self, ifStmt.thenStmt);
            if (thenStmtErr != null) {
                return thenStmtErr.?;
            }
            if (ifStmt.elseStmt != null) {
                const elseStmtErr = try typecheckStmt(self, ifStmt.elseStmt.?);
                if (elseStmtErr != null) {
                    return elseStmtErr.?;
                }
            }
        },
        .Compound => |compound| {
            for (compound.items) |blkItem| {
                const blkItemTypeError = try typecheckBlkItem(self, blkItem);
                if (blkItemTypeError != null) {
                    return blkItemTypeError.?;
                }
            }
        },
        .For => |forStmt| {
            if ((try typecheckForInit(self, forStmt.init))) |typeError| {
                return typeError;
            }
            if (forStmt.condition) |condition| {
                _ = typecheckExpr(self, condition) catch |err| {
                    const typeErrorStruct = try self.allocator.create(TypeErrorStruct);
                    typeErrorStruct.* = .{
                        .errorType = err,
                        .errorPayload = (try std.fmt.allocPrint(self.allocator, "Type error at for condition\n", .{})),
                    };
                    return typeErrorStruct;
                };
            }
            if (forStmt.post) |post| {
                _ = typecheckExpr(self, post) catch |err| {
                    const typeErrorStruct = try self.allocator.create(TypeErrorStruct);
                    typeErrorStruct.* = .{
                        .errorType = err,
                        .errorPayload = (try std.fmt.allocPrint(self.allocator, "Type error at for post\n", .{})),
                    };
                    return typeErrorStruct;
                };
            }
            const bodyErr = try typecheckStmt(self, forStmt.body);
            if (bodyErr != null) {
                return bodyErr.?;
            }
        },
        .DoWhile => |doWhileStmt| {
            _ = typecheckExpr(self, doWhileStmt.condition) catch |err| {
                const typeErrorStruct = try self.allocator.create(TypeErrorStruct);
                typeErrorStruct.* = .{
                    .errorType = err,
                    .errorPayload = (try std.fmt.allocPrint(self.allocator, "Type error at do while condition\n", .{})),
                };
                return typeErrorStruct;
            };
            const bodyErr = try typecheckStmt(self, doWhileStmt.body);
            if (bodyErr != null) {
                return bodyErr.?;
            }
        },
        .While => |whileStmt| {
            _ = typecheckExpr(self, whileStmt.condition) catch |err| {
                const typeErrorStruct = try self.allocator.create(TypeErrorStruct);
                typeErrorStruct.* = .{
                    .errorType = err,
                    .errorPayload = (try std.fmt.allocPrint(self.allocator, "Type error at while condition\n", .{})),
                };
                return typeErrorStruct;
            };
            const bodyErr = try typecheckStmt(self, whileStmt.body);
            if (bodyErr != null) {
                return bodyErr.?;
            }
        },
        .Break => {},
        .Continue => {},
        .Label => {},
        .Goto => {},
        .Null => {},
    }
    return null;
}

fn typecheckForInit(self: *Typechecker, forInit: *AST.ForInit) TypeCheckerError!?*TypeErrorStruct {
    switch (forInit.*) {
        .Declaration => |decl| {
            const sym = try self.allocator.create(Symbol);
            sym.* = .{
                .typeInfo = .Integer,
                .attributes = .LocalAttr,
            };
            try self.symbolTable.put(decl.name, sym);
            if (decl.expression) |expression| {
                _ = typecheckExpr(self, expression) catch |err| {
                    return try TypeErrorStruct.typeError(
                        self.allocator,
                        err,
                        try std.fmt.allocPrint(self.allocator, "Type error at for init\n", .{}),
                    );
                };
            }
        },
        .Expression => |expr| {
            _ = typecheckExpr(self, expr) catch |err| {
                return try TypeErrorStruct.typeError(
                    self.allocator,
                    err,
                    try std.fmt.allocPrint(self.allocator, "Type error at for init\n", .{}),
                );
            };
        },
    }
    return null;
}

// TODO: A design change can make this easier
// Or I can use some metaprogramming to generate this
inline fn convert(allocator: std.mem.Allocator, expr: *AST.Expression, toType: AST.Type) !*AST.Expression {
    switch (expr.*) {
        .Cast => {
            return expr;
        },
        .Assignment => {
            if (expr.Assignment.type != toType) {
                const castExpr = try allocator.create(AST.Expression);
                castExpr.* = AST.Expression{ .Cast = .{
                    .type = toType,
                    .value = expr,
                } };
                return castExpr;
            }
            expr.Assignment.type = toType;
            return expr;
        },
        .Binary => {
            if (expr.Binary.type != toType) {
                const castExpr = try allocator.create(AST.Expression);
                castExpr.* = AST.Expression{ .Cast = .{
                    .type = toType,
                    .value = expr,
                } };
                return castExpr;
            }
            expr.Binary.type = toType;
            return expr;
        },
        .FunctionCall => {
            if (expr.FunctionCall.type != toType) {
                const castExpr = try allocator.create(AST.Expression);
                castExpr.* = AST.Expression{ .Cast = .{
                    .type = toType,
                    .value = expr,
                } };
                return castExpr;
            }
            expr.FunctionCall.type = toType;
            return expr;
        },
        .Identifier => {
            if (expr.Identifier.type != toType) {
                const castExpr = try allocator.create(AST.Expression);
                castExpr.* = AST.Expression{ .Cast = .{
                    .type = toType,
                    .value = expr,
                } };
                return castExpr;
            }
            expr.Identifier.type = toType;
            return expr;
        },
        .Constant => {
            if (expr.Constant.type != toType) {
                const castExpr = try allocator.create(AST.Expression);
                castExpr.* = AST.Expression{ .Cast = .{
                    .type = toType,
                    .value = expr,
                } };
                return castExpr;
            }
            expr.Constant.type = toType;
            return expr;
        },
        .Unary => {
            if (expr.Unary.type != toType) {
                const castExpr = try allocator.create(AST.Expression);
                castExpr.* = AST.Expression{ .Cast = .{
                    .type = toType,
                    .value = expr,
                } };
                return castExpr;
            }
            expr.Unary.type = toType;
            return expr;
        },
        .Ternary => {
            if (expr.Ternary.type != toType) {
                const castExpr = try allocator.create(AST.Expression);
                castExpr.* = AST.Expression{ .Cast = .{
                    .type = toType,
                    .value = expr,
                } };
                return castExpr;
            }
            expr.Ternary.type = toType;
            return expr;
        },
    }
}

fn typecheckExpr(self: *Typechecker, expr: *AST.Expression) TypeError!AST.Type {
    // Expr can error because of a type issue,
    // for now we can just return a generic type error
    // and handle it from the outer functions (no diagnostic)
    switch (expr.*) {
        .Cast => |cast| {
            return cast.type;
        },
        .Assignment => |assignment| {
            const lhsType = try typecheckExpr(self, assignment.lhs);
            const rhsType = try typecheckExpr(self, assignment.rhs);
            std.log.warn("Conversion from {any} to {any}\n", .{ rhsType, lhsType });
            expr.Assignment.rhs = try convert(self.allocator, assignment.rhs, lhsType);
            expr.Assignment.type = lhsType;
            std.log.warn("Returning lhsType: {any}\n", .{lhsType});
            return lhsType;
        },
        .Binary => {
            const lhsType = try typecheckExpr(self, expr.Binary.lhs);
            const rhsType = try typecheckExpr(self, expr.Binary.rhs);

            //INFO: Conversion logic
            expr.Binary.type = switch (expr.Binary.op) {
                .ADD, .SUBTRACT, .MULTIPLY, .DIVIDE, .REMAINDER => blk: {
                    if (lhsType == AST.Type.Long or rhsType == AST.Type.Long) {
                        expr.Binary.lhs = try convert(self.allocator, expr.Binary.lhs, AST.Type.Long);
                        expr.Binary.rhs = try convert(self.allocator, expr.Binary.rhs, AST.Type.Long);
                        break :blk AST.Type.Long;
                    } else {
                        break :blk AST.Type.Integer;
                    }
                },
                else => blk: {
                    if (lhsType == AST.Type.Long or rhsType == AST.Type.Long) {
                        expr.Binary.lhs = try convert(self.allocator, expr.Binary.lhs, AST.Type.Long);
                        expr.Binary.rhs = try convert(self.allocator, expr.Binary.rhs, AST.Type.Long);
                    }
                    break :blk AST.Type.Integer;
                },
            };

            std.log.warn("Returning from binary expr({any}): {any}\n", .{
                expr.Binary.op,
                expr.Binary.type.?,
            });
            return expr.Binary.type.?;
        },
        .FunctionCall => |fnCall| {
            const fnSymbol = if (self.symbolTable.get(fnCall.name)) |fnSym| fnSym else {
                std.log.warn("Unknown function: {s}\n", .{fnCall.name});
                return TypeError.UnknownFunction;
            };
            if (fnCall.args.items.len != fnSymbol.typeInfo.Function.args.items.len) {
                std.log.warn("Expected {d} arguments but found {d} arguments in {s}\n", .{
                    fnSymbol.typeInfo.Function.args.items.len,
                    fnCall.args.items.len,
                    fnCall.name,
                });
                return TypeError.TypeMismatch;
            }
            for (0..fnSymbol.typeInfo.Function.args.items.len) |i| {
                const argType = try typecheckExpr(self, fnCall.args.items[i]);
                if (fnSymbol.typeInfo.Function.args.items[i] != argType) {
                    std.log.warn("Casting function argument in fn: {s} from {any} to {any}\n", .{
                        fnCall.name,
                        fnCall.args.items[i].getType(),
                        fnSymbol.typeInfo.Function.args.items[i],
                    });
                    const converted = try convert(self.allocator, fnCall.args.items[i], fnSymbol.typeInfo.Function.args.items[i]);
                    fnCall.args.items[i] = converted;
                }
            }
            _ = try convert(self.allocator, expr, fnSymbol.typeInfo.Function.returnType);
            expr.FunctionCall.type = fnSymbol.typeInfo.Function.returnType;
            return fnSymbol.typeInfo.Function.returnType;
        },
        .Identifier => |identifier| {
            const symbol = if (self.symbolTable.get(identifier.name)) |sym| sym else {
                std.log.warn("Unknown identifier: {s}\n", .{identifier.name});
                return TypeError.UnknownIdentifier;
            };
            const astType = AST.Type.fromSemType(symbol.typeInfo);
            std.log.warn("Identifier({s}) type registered as: {any}", .{ identifier.name, symbol.typeInfo });
            expr.Identifier.type = astType;
            return astType;
        },
        .Constant => |constant| {
            const t = switch (constant.value) {
                .Integer => AST.Type.Integer,
                .Long => AST.Type.Long,
            };
            // TODO: Should we store this type?
            // expr.Constant.type = t;
            return t;
        },
        .Unary => |unary| {
            const exprType = try typecheckExpr(self, unary.exp);
            expr.Unary.type = exprType;
            return exprType;
        },
        .Ternary => |ternary| {
            _ = try typecheckExpr(self, ternary.condition);
            const lhsType = try typecheckExpr(self, ternary.lhs);
            std.debug.assert(lhsType == AST.Type.Integer);
            const rhsType = try typecheckExpr(self, ternary.rhs);
            std.debug.assert(rhsType == AST.Type.Integer);
            expr.Ternary.type = AST.Type.Integer;
            return AST.Type.Integer;
        },
    }
}

pub fn resolveLabels(allocator: std.mem.Allocator, program: *AST.Program) SemanticError!void {
    // Collect locals
    const localsSet = std.BufSet.init(allocator);
    for (program.function.blockItems.items) |blockItem| {
        if ((std.meta.activeTag(blockItem.*) == .Statement) and std.meta.activeTag(blockItem.Statement.*) == .Local) {
            try localsSet.insert(blockItem.Statement.Local);
        }
    }
    // Check gotos
    for (program.function.blockItems.items) |blockItem| {
        if ((std.meta.activeTag(blockItem.*) == .Statement) and std.meta.activeTag(blockItem.Statement.*) == .Goto) {
            if (!localsSet.contains(blockItem.Statement.Goto)) return SemanticError.UnknownLabel;
        }
    }
}
