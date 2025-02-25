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
        Integer: i32,
        Long: i64,
        UInteger: u32,
        ULong: u64,
        Float: f64,
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
    InvalidOperand,
} || AST.CodegenError || AST.DeclaratorError;

const TypeCheckerError = error{OutOfMemory} || TypeError || AST.DeclaratorError;

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
    ULong,
    UInteger,
    Float,
    Pointer,

    const Self = @This();
    pub fn from(self: AST.Type) Self {
        return switch (self) {
            .Integer => .Integer,
            .Void => .Void,
            .Long => .Long,
            .UInteger => .UInteger,
            .ULong => .ULong,
            .Float => .Float,
            .Pointer => .Pointer,
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
    ULong,
    UInteger,
    Float,
    Pointer: *TypeInfo,

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
        self.typecheckReturns(program) catch |err| {
            if (err == error.InvalidOperand) {
                const errorPayload = try std.fmt.allocPrint(self.allocator, "Type error at return statement\n", .{});
                return errorPayload;
            }
        };
        return null;
    }

    pub fn typecheckReturns(self: *Self, program: *AST.Program) TypeError!void {
        for (program.externalDecls.items) |externalDecl| {
            if (std.meta.activeTag(externalDecl.*) == .FunctionDecl) {
                for (externalDecl.FunctionDecl.blockItems.items) |blkItem| {
                    resolveBlockReturns(self, blkItem, externalDecl.FunctionDecl.returnType) catch |err| {
                        std.log.warn("Error at fn: {s}\n", .{(try externalDecl.FunctionDecl.declarator.unwrapFuncDeclarator()).declarator.Ident});
                        return err;
                    };
                }
            }
        }
    }
};

pub fn resolveBlockReturns(self: *Typechecker, blockItem: *AST.BlockItem, fnReturnType: AST.Type) TypeError!void {
    if (std.meta.activeTag(blockItem.*) == .Statement and std.meta.activeTag(blockItem.Statement.*) == .Return) {
        const isFnTypePtr = std.meta.activeTag(fnReturnType) == .Pointer;
        const isExprTypePtr = std.meta.activeTag(blockItem.Statement.Return.expression.getType()) == .Pointer;
        if (isFnTypePtr and (!isExprTypePtr and !blockItem.Statement.Return.expression.isNullPtr())) {
            std.log.warn("Invalid types: fn returns {any}, got {any} in return", .{ fnReturnType, blockItem.Statement.Return.expression.getType() });

            return TypeError.InvalidOperand;
        }
        blockItem.Statement.Return.expression = try convert(self.allocator, blockItem.Statement.Return.expression, fnReturnType);
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

inline fn handleNonPointerExpr(self: *Typechecker, varDecl: *AST.Declaration, expression: *AST.Expression, initializer: *InitValue) !void {
    const exprType = expression.getType();

    if (varDecl.type.deepEql(exprType)) {
        //TODO: handle pointers differently

        const castExpr = try self.allocator.create(AST.Expression);
        castExpr.* = AST.Expression{ .Cast = .{
            .type = varDecl.type,
            .value = expression,
        } };
        varDecl.expression = castExpr;
        initializer.* = .{ .Initial = switch (varDecl.type) {
            .Integer => .{ .type = .Integer, .value = .{
                .Integer = expression.Constant.to(i32),
            } },
            .Long => .{ .type = .Long, .value = .{
                .Long = expression.Constant.to(i64),
            } },
            .UInteger => .{ .type = .UInteger, .value = .{
                .UInteger = expression.Constant.to(u32),
            } },
            .ULong => .{ .type = .ULong, .value = .{
                .ULong = expression.Constant.to(u64),
            } },

            .Float => .{ .type = .Float, .value = .{
                .Float = expression.Constant.value.Float,
            } },
            .Pointer => .{
                .type = varDecl.type,
                .value = .{ .ULong = 0 },
            },
            else => unreachable,
        } };
    } else {
        initializer.* = .{ .Initial = switch (varDecl.type) {
            .Integer => .{ .type = .Integer, .value = .{ .Integer = expression.Constant.value.Integer } },
            .Long => .{ .type = .Long, .value = .{ .Long = expression.Constant.to(i64) } },
            .Float => .{ .type = .Float, .value = .{ .Float = expression.Constant.value.Float } },
            else => unreachable,
        } };
    }
}

fn globalDeclExprNotInteger(self: *Typechecker, expression: *AST.Expression, varName: []u8) TypeError!?*TypeErrorStruct {
    if (std.meta.activeTag(expression.*) != .Constant) {
        return TypeErrorStruct.typeError(
            self.allocator,
            TypeError.GlobalDeclarationNotInteger,
            try std.fmt.allocPrint(
                self.allocator,
                "Global declarations only support constants for declaration of {s}\n",
                .{varName},
            ),
        );
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
            const functionDeclarator = try functionDecl.declarator.unwrapFuncDeclarator();
            functionDecl.fixReturnType(self.allocator) catch unreachable;
            std.debug.assert(std.meta.activeTag(functionDeclarator.declarator.*) == .Ident);
            if (self.symbolTable.get(functionDeclarator.declarator.Ident)) |sym| {
                if (!sym.typeInfo.isOfKind(.Function)) {
                    return try TypeErrorStruct.typeError(
                        self.allocator,
                        TypeError.GlobVarRedeclaredAsFn,
                        (try std.fmt.allocPrint(self.allocator, "Global variable {s} redeclared as function\n", .{functionDecl.declarator.FunDeclarator.declarator.Ident})),
                    );
                }
                const nonStaticLinkageMismatch = !sym.attributes.FunctionAttr.global and (functionDecl.storageClass != AST.Qualifier.STATIC);
                const staticLinkageMismatch = sym.attributes.FunctionAttr.global and (functionDecl.storageClass == AST.Qualifier.STATIC);
                if (nonStaticLinkageMismatch or staticLinkageMismatch) {
                    return TypeErrorStruct.typeError(
                        self.allocator,
                        TypeError.LinkageMismatch,
                        try std.fmt.allocPrint(self.allocator, "Global variable {s} redeclared as non-static\n", .{functionDecl.declarator.FunDeclarator.declarator.Ident}),
                    );
                }

                if (functionDecl.isDefined() and sym.isDefined()) {
                    return TypeErrorStruct.typeError(
                        self.allocator,
                        TypeError.FnRedefined,
                        try std.fmt.allocPrint(
                            self.allocator,
                            "Function redefinition of {s}\n",
                            .{functionDecl.declarator.FunDeclarator.declarator.Ident},
                        ),
                    );
                }

                for (0..functionDeclarator.params.items.len) |i| {
                    if (std.meta.activeTag(functionDeclarator.params.items[i].NonVoidArg.type) != std.meta.activeTag(sym.typeInfo.Function.args.items[i])) {
                        return TypeErrorStruct.typeError(
                            self.allocator,
                            TypeError.FnPrevDeclArgMismatch,
                            try std.fmt.allocPrint(self.allocator, "Argument mismatch in function redeclaration: function name = {s}, function arg mismatch between {any} and {any}\n", .{
                                functionDecl.declarator.FunDeclarator.declarator.Ident,
                                functionDecl.declarator.FunDeclarator.params.items[i].NonVoidArg.type,
                                sym.typeInfo.Function.args.items[i],
                            }),
                        );
                    }
                }

                // WARN: This is incorrect, write a seperate function for type
                // equality
                if (std.meta.activeTag(functionDecl.returnType) != std.meta.activeTag(sym.typeInfo.Function.returnType)) {
                    return TypeErrorStruct.typeError(
                        self.allocator,
                        TypeError.FnPrevDeclArgMismatch,
                        try std.fmt.allocPrint(
                            self.allocator,
                            "Function {s} has mismatching declarations\n",
                            .{functionDecl.declarator.FunDeclarator.declarator.Ident},
                        ),
                    );
                }

                if (functionDeclarator.params.items.len != sym.typeInfo.Function.args.items.len) {
                    return TypeErrorStruct.typeError(
                        self.allocator,
                        TypeError.FnArgNumMismatch,
                        try std.fmt.allocPrint(
                            self.allocator,
                            "Function {s} has mismatching declarations\n",
                            .{functionDecl.declarator.FunDeclarator.declarator.Ident},
                        ),
                    );
                }
            }
            const fnSym = try self.allocator.create(Symbol);
            const fnName = (try functionDeclarator.declarator.unwrapIdentDecl()).Ident;
            var fnArgsList = std.ArrayList(AST.Type).init(self.allocator);
            std.log.warn("Params for {s}:\n", .{fnName});
            for (functionDeclarator.params.items) |arg| {
                arg.NonVoidArg.fixType(self.allocator) catch unreachable;
                std.log.warn("\targ: {any}\n", .{arg.NonVoidArg.type});
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
            try self.symbolTable.put(fnName, fnSym);

            for (functionDeclarator.params.items) |arg| {
                const argSym = try self.allocator.create(Symbol);
                argSym.* = .{
                    .typeInfo = try arg.NonVoidArg.type.toSemPointerTy(self.allocator),
                    .attributes = .LocalAttr,
                };
                const argName = (try arg.NonVoidArg.declarator.unwrapIdentDecl()).Ident;
                std.log.warn("Arg name: {s} and argSym: {any}\n", .{ argName, argSym });
                try self.symbolTable.put(argName, argSym);
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

            varDecl.fixReturnType(self.allocator) catch unreachable;
            var initializer: InitValue = .NoInit;
            var global = false;
            const varName = (try varDecl.declarator.unwrapIdentDecl()).Ident;

            // INFO: Has expression
            if (varDecl.expression) |expression| {
                // INFO: Should be constant initialized

                if (try globalDeclExprNotInteger(self, expression, varName)) |typeError| return typeError;

                // INFO: handle pointers here, no casting is really required, except
                // changing the type of the rhs explicitly(only 0 is allowed in
                // the RHS)

                if (std.meta.activeTag(varDecl.type) == .Pointer) {
                    if (!expression.isNullPtr())
                        return TypeErrorStruct.typeError(
                            self.allocator,
                            TypeError.GlobalDeclarationNotInteger,
                            try std.fmt.allocPrint(
                                self.allocator,
                                "Global pointer declarations only support 0 as rhs: assignment of {s}\n",
                                .{varDecl.declarator.Ident},
                            ),
                        );
                    varDecl.expression.?.Constant.type = varDecl.type;
                    varDecl.expression.?.Constant.value = .{ .ULong = 0 };
                } else {
                    try handleNonPointerExpr(self, varDecl, expression, &initializer);
                }

                // INFO: Extern decls no assignment check
                if (varDecl.storageClass == AST.Qualifier.EXTERN) {
                    return TypeErrorStruct.typeError(
                        self.allocator,
                        TypeError.ExternVarDeclared,
                        try std.fmt.allocPrint(
                            self.allocator,
                            "Extern declarations cant have an assignment: {s}\n",
                            .{varDecl.declarator.Ident},
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
            if (self.symbolTable.get(varName)) |varSym| {
                //INFO: Function redeclaration as variable handling
                if (varSym.typeInfo.isOfKind(.Function)) {
                    return TypeErrorStruct.typeError(
                        self.allocator,
                        TypeError.FnRedeclaredAsVar,
                        try std.fmt.allocPrint(
                            self.allocator,
                            "Function redeclared as var: {s}\n",
                            .{(try varDecl.declarator.unwrapIdentDecl()).Ident},
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
                                .{varDecl.declarator.Ident},
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
                                        .{varDecl.declarator.Ident},
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
                            .{varDecl.declarator.Ident},
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
                    .UInteger => .UInteger,
                    .ULong => .ULong,
                    .Float => .Float,
                    .Pointer => try varDecl.type.toSemPointerTy(self.allocator),
                    else => unreachable,
                },
                .attributes = .{
                    .StaticAttr = .{
                        .global = global,
                        .init = initializer,
                    },
                },
            };
            try self.symbolTable.put((try varDecl.declarator.unwrapIdentDecl()).Ident, sym);
        },
    }
    return null;
}

fn checkConversion(from: AST.Type, to: AST.Type) bool {
    // if both are numeric, its a valid conversion
    if (from.isNumeric() and to.isNumeric()) return true;
    if (from.isNumeric() or to.isNumeric()) return false;
    // Here both types are pointers, do a strict equality, if not then false
    return from.deepEql(to);
}

fn isNullPtrAssignment(expr: *AST.Expression, toType: AST.Type) bool {
    if (std.meta.activeTag(toType) != .Pointer) return false;
    return expr.isNullPtr();
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
            try decl.fixReturnType(self.allocator);
            if (decl.expression) |declExpression| {
                // Stages of typechecking an assigned expression
                // 1. Typecheck within the expression and retrieve the
                // expression type
                // 2. Check if a possible casting rule exists for declaration
                // and expression type
                // 3. If casting is possible, cast using convert function
                // 4. Check if the cast has overwritten the type of the
                // expression(Final assert)
                const exprType = typecheckExpr(self, declExpression) catch |err| {
                    const typeErrorStruct = try self.allocator.create(TypeErrorStruct);
                    std.log.warn("error: {any}\n", .{err});
                    typeErrorStruct.* = .{
                        .errorType = err,
                        .errorPayload = (try std.fmt.allocPrint(self.allocator, "Type error at local declaration expression\n", .{})),
                    };
                    return typeErrorStruct;
                };

                // Not all conversions are allowed, this has to be done before
                // calling convert, if checkConversion and null pointer
                // assignment fails then its a type error
                if (!checkConversion(decl.type, exprType) and !isNullPtrAssignment(declExpression, decl.type)) {
                    std.log.warn("error: check conversion\n", .{});
                    const typeErrorStruct = try self.allocator.create(TypeErrorStruct);
                    typeErrorStruct.* = .{
                        .errorType = TypeError.TypeMismatch,
                        .errorPayload = (try std.fmt.allocPrint(self.allocator, "Type error at local declaration\n", .{})),
                    };
                    return typeErrorStruct;
                }

                decl.expression = try convert(self.allocator, decl.expression.?, decl.type);
                std.debug.assert(std.meta.activeTag(decl.expression.?.getType()) == decl.type);
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
                                .{decl.declarator.Ident},
                            )) };
                            return typeErrorStruct;
                        }
                        if (self.symbolTable.get(decl.declarator.Ident)) |olderSym| {
                            std.log.warn("This: {any} and {any}\n", .{ olderSym.typeInfo, TypeKind.from(decl.type) });
                            if (!olderSym.typeInfo.isOfKind(TypeKind.from(decl.type))) {
                                const typeErrorStruct = try self.allocator.create(TypeErrorStruct);
                                typeErrorStruct.* = .{
                                    .errorType = TypeError.FnRedeclaredAsVar,
                                    .errorPayload = (try std.fmt.allocPrint(
                                        self.allocator,
                                        "Extern variable type error for symbol {s}\n",
                                        .{decl.declarator.Ident},
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
                                        .{decl.declarator.Ident},
                                    )),
                                };
                            }
                        } else {
                            const sym = try self.allocator.create(Symbol);
                            sym.* = .{ .typeInfo = .Integer, .attributes = .{ .StaticAttr = .{
                                .global = true,
                                .init = .NoInit,
                            } } };
                            try self.symbolTable.put(decl.declarator.Ident, sym);
                        }
                    },
                    .STATIC => {
                        if (decl.expression) |expr| {
                            if (!std.mem.eql(u8, @tagName(expr.*), "Constant")) {
                                const typeErrorStruct = try self.allocator.create(TypeErrorStruct);
                                typeErrorStruct.* = .{
                                    .errorType = TypeError.GlobalDeclarationNotInteger,
                                    .errorPayload = (try std.fmt.allocPrint(
                                        self.allocator,
                                        "Static variable {s} must have a constant initializer\n",
                                        .{decl.declarator.Ident},
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
                            try self.symbolTable.put(decl.declarator.Ident, sym);
                        } else {
                            const sym = try self.allocator.create(Symbol);
                            sym.* = .{
                                .typeInfo = .Integer,
                                .attributes = .{ .StaticAttr = .{
                                    .init = .{ .Initial = .{ .type = .Integer, .value = .{ .Integer = 0 } } },
                                    .global = false,
                                } },
                            };
                            try self.symbolTable.put(decl.declarator.Ident, sym);
                        }
                    },
                }
            } else {
                // No decl
                const sym = try self.allocator.create(Symbol);
                sym.* = .{
                    .typeInfo = try decl.type.toSemPointerTy(self.allocator),
                    .attributes = .LocalAttr,
                };
                try self.symbolTable.put((try decl.declarator.unwrapIdentDecl()).Ident, sym);
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
            try self.symbolTable.put(decl.declarator.Ident, sym);
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
        .AddrOf => {
            if (!expr.getType().deepEql(toType)) {
                const castExpr = try allocator.create(AST.Expression);
                castExpr.* = AST.Expression{ .Cast = .{
                    .type = toType,
                    .value = expr,
                } };
                return castExpr;
            }
            return expr;
        },
        .Deref => {
            if (std.meta.activeTag(expr.Deref.type.?) != std.meta.activeTag(toType)) {
                const castExpr = try allocator.create(AST.Expression);
                castExpr.* = AST.Expression{ .Cast = .{
                    .type = toType,
                    .value = expr,
                } };
                return castExpr;
            }
            return expr;
        },
        .Assignment => {
            if (std.meta.activeTag(expr.Assignment.type.?) != std.meta.activeTag(toType)) {
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
            if (std.meta.activeTag(expr.Binary.type.?) != std.meta.activeTag(toType)) {
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
            if (std.meta.activeTag(expr.FunctionCall.type.?) != std.meta.activeTag(toType)) {
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
            if (std.meta.activeTag(expr.Identifier.type.?) != toType) {
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
            if (std.meta.activeTag(expr.Constant.type) != toType) {
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
            if (std.meta.activeTag(expr.Unary.type.?) != toType) {
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
            if (std.meta.activeTag(expr.Ternary.type.?) != toType) {
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

fn getCommonPtrType(lhs: *AST.Expression, rhs: *AST.Expression) ?AST.Type {
    const type1 = lhs.getType();
    const type2 = rhs.getType();
    if (type1.deepEql(type2)) return type1;
    if (lhs.isNullPtr()) return type2;
    if (rhs.isNullPtr()) return type1;
    return null;
}

fn getCommonType(type1: AST.Type, type2: AST.Type) ?AST.Type {
    // INFO: same types => return the type
    if (std.meta.activeTag(type1) == std.meta.activeTag(type2)) return type1;
    if (std.meta.activeTag(type1) == .Float or std.meta.activeTag(type2) == .Float) {
        return .Float;
    }
    if (type1.size() == type2.size()) {
        // INFO: for same sized type
        // Otherwise, if both operands have signed integer types or both
        // have unsigned integer types, the operand with the type of the
        // lesser integer conversion rank is converted to the type of the
        // operand with greater rank.
        // Rank is proportional to size, rank(unsigned size) > rank(signed size)
        return if (type1.signed()) type2 else type1;
    }
    // Size is greater, more priority in case the sizes are unequal
    return if (type1.size() > type2.size()) type1 else type2;
}
pub inline fn xor(a: bool, b: bool) bool {
    return (a or b) and !(a and b);
}

fn typecheckExpr(self: *Typechecker, expr: *AST.Expression) TypeError!AST.Type {
    // Expr can error because of a type issue,
    // for now we can just return a generic type error
    // and handle it from the outer functions (no diagnostic)
    switch (expr.*) {
        .AddrOf => |addrOf| {
            const innerType = try self.allocator.create(AST.Type);
            innerType.* = try typecheckExpr(self, addrOf.exp);
            expr.AddrOf.type = .{ .Pointer = innerType };
            std.log.warn("Assigning type to AddrOf: pointer to {any} \n", .{innerType});
            return .{ .Pointer = innerType };
        },
        .Deref => |deref| {
            const innerType = try typecheckExpr(self, deref.exp);
            if (std.meta.activeTag(innerType) != .Pointer) {
                std.log.warn("Dereferenced type is not a pointer, got {any} instead\n", .{std.meta.activeTag(innerType)});
                return TypeError.InvalidOperand;
            }
            expr.Deref.type = innerType.Pointer.*;
            return innerType.Pointer.*;
        },
        .Cast => |cast| {
            return cast.type;
        },
        .Assignment => |assignment| {
            const lhsType = try typecheckExpr(self, assignment.lhs);
            _ = try typecheckExpr(self, assignment.rhs);
            expr.Assignment.rhs = try convert(self.allocator, assignment.rhs, lhsType);
            expr.Assignment.type = lhsType;
            return lhsType;
        },
        .Binary => {
            const lhsType = try typecheckExpr(self, expr.Binary.lhs);
            const rhsType = try typecheckExpr(self, expr.Binary.rhs);
            const isLhsPointer = std.meta.activeTag(lhsType) == .Pointer;
            const isRhsPointer = std.meta.activeTag(rhsType) == .Pointer;

            if (isLhsPointer and
                isRhsPointer and (expr.Binary.op == .MULTIPLY or
                expr.Binary.op == .DIVIDE))
            {
                std.log.warn("Cant {any} pointers\n", .{expr.Binary.op});
                return TypeError.InvalidOperand;
            }

            if (xor(isLhsPointer, isRhsPointer) and expr.Binary.op.isCompareOp()) {
                std.log.warn("Cant compare pointers and non-pointers\n", .{});
                return TypeError.InvalidOperand;
            }

            //INFO: Conversion logic
            const commonType = if (isLhsPointer or isRhsPointer) getCommonPtrType(expr.Binary.lhs, expr.Binary.rhs).? else getCommonType(lhsType, rhsType);
            if (commonType == null) {
                std.log.warn("Invalid pointer comparisions", .{});
                return TypeError.InvalidOperand;
            }
            if (commonType.? == .Float and expr.Binary.op == .REMAINDER) {
                std.log.warn("Cannot apply {any} to {any}", .{ expr.Binary.op, commonType });
                return TypeError.InvalidOperand;
            }
            expr.Binary.lhs = try convert(self.allocator, expr.Binary.lhs, commonType.?);
            expr.Binary.rhs = try convert(self.allocator, expr.Binary.rhs, commonType.?);
            expr.Binary.type = switch (expr.Binary.op) {
                .ADD, .SUBTRACT, .MULTIPLY, .DIVIDE, .REMAINDER => commonType.?,
                else => AST.Type.Integer,
            };

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
                if (std.meta.activeTag(fnSymbol.typeInfo.Function.args.items[i]) != std.meta.activeTag(argType)) {
                    std.log.warn("Casting function argument in fn: {s} from {any} to {any}\n", .{
                        fnCall.name,
                        fnCall.args.items[i].getType(),
                        fnSymbol.typeInfo.Function.args.items[i],
                    });
                    const converted = try convert(self.allocator, fnCall.args.items[i], fnSymbol.typeInfo.Function.args.items[i]);
                    fnCall.args.items[i] = converted;
                }
            }
            expr.FunctionCall.type = fnSymbol.typeInfo.Function.returnType;
            // FIXME: This convert call is stupid, delete it after some checks
            _ = try convert(self.allocator, expr, fnSymbol.typeInfo.Function.returnType);
            return fnSymbol.typeInfo.Function.returnType;
        },
        .Identifier => |identifier| {
            const symbol = if (self.symbolTable.get(identifier.name)) |sym| sym else {
                std.log.warn("Unknown identifier: {s}\n", .{identifier.name});
                return TypeError.UnknownIdentifier;
            };
            const astType = try AST.Type.fromSemType(&symbol.typeInfo, self.allocator);
            std.log.warn("Identifier({s}) type registered as: {any}", .{ identifier.name, symbol.typeInfo });
            expr.Identifier.type = astType;
            return astType;
        },
        .Constant => |constant| {
            const t: AST.Type = switch (constant.value) {
                .Integer => .Integer,
                .Long => .Long,
                .ULong => .ULong,
                .UInteger => .UInteger,
                .Float => .Float,
            };
            // INFO: Constant floats need to be in .rodata/.data
            if (t == .Float) {
                const tempId = try AST.tempGen.genTemp(self.allocator);
                std.log.warn("tempId: {any}\n", .{tempId});
                const sym = try self.allocator.create(Symbol);
                sym.* = .{
                    .typeInfo = .Float,
                    .attributes = .{
                        .StaticAttr = .{
                            .init = .{
                                .Initial = .{
                                    .type = .Float,
                                    .value = .{ .Float = constant.value.Float },
                                },
                            },
                            .global = false,
                        },
                    },
                };

                try self.symbolTable.put(
                    tempId,
                    sym,
                );
                expr.* = .{
                    .Identifier = .{
                        .type = .Float,
                        .name = tempId,
                    },
                };
            }
            // TODO: Should we store this type?
            // expr.Constant.type = t;
            return t;
        },
        .Unary => |unary| {
            const exprType = try typecheckExpr(self, unary.exp);
            if (exprType == .Float and unary.unaryOp == .COMPLEMENT) {
                std.log.warn("operation {any} cannot be applied to {any}", .{ unary.unaryOp, exprType });
                return TypeError.InvalidOperand;
            }
            expr.Unary.type = exprType;
            return exprType;
        },
        .Ternary => |ternary| {
            _ = try typecheckExpr(self, ternary.condition);

            const lhsType = try typecheckExpr(self, ternary.lhs);
            const rhsType = try typecheckExpr(self, ternary.rhs);
            const ternaryLhsPointer = std.meta.activeTag(lhsType) == .Pointer;
            const ternaryRhsPointer = std.meta.activeTag(rhsType) == .Pointer;

            // zig fmt: off
            expr.Ternary.type = if (ternaryLhsPointer or ternaryRhsPointer) getCommonPtrType(ternary.lhs, ternary.rhs) 
                                else getCommonType(lhsType, rhsType);
            //zig fmt:on
            std.debug.assert(expr.Ternary.type != null);
            if (!checkConversion(lhsType, expr.Ternary.type.?) and !isNullPtrAssignment(ternary.lhs, expr.Ternary.type.?)) {
                std.log.warn("Invalid types: lhs type {any}, rhs type {any}, ternary type {any}\n", .{ lhsType, ternary.type, ternary.type });
                return TypeError.TypeMismatch;
            }
            expr.Ternary.lhs = try convert(self.allocator, ternary.lhs, expr.Ternary.type.?);

            if (!checkConversion(rhsType, expr.Ternary.type.?) and !isNullPtrAssignment(ternary.rhs, expr.Ternary.type.?)) {
                std.log.warn("Invalid types: lhs type {any}, rhs type {any}, ternary type {any}\n", .{ lhsType, ternary.type, ternary.type });
                return TypeError.TypeMismatch;
            }
            expr.Ternary.rhs = try convert(self.allocator, ternary.rhs, expr.Ternary.type.?);
            return expr.Ternary.type.?;
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
