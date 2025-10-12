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

pub const TypeCheckerError = error{OutOfMemory} || TypeError || AST.DeclaratorError;

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
            .Array => unreachable,
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
        return std.meta.activeTag(self.*) == other;
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
    errors: std.ArrayList([]u8),
    const Self = @This();

    pub fn getErrString(self: *Self) ![]u8 {
        var combinedStr = std.ArrayList(u8).init(self.allocator);
        for (self.errors.items) |err| {
            try combinedStr.appendSlice(err);
            try combinedStr.append('\n');
        }
        return combinedStr.toOwnedSlice();
    }
    pub fn init(allocator: std.mem.Allocator) !*Self {
        const typechecker = try allocator.create(Typechecker);
        typechecker.* = .{
            .symbolTable = std.StringHashMap(*Symbol).init(allocator),
            .allocator = allocator,
            .errors = std.ArrayList([]u8).init(allocator),
        };
        return typechecker;
    }
    pub fn check(self: *Self, program: *AST.Program) !void {
        // This function doesnt use the typical try-catch error handling in zig
        // we just push the errors into an list, and we just return the string

        // INFO: Typechecks a program and constructs a table
        // Then it resolves the return labels
        try typecheckProgram(self, program);
        try typecheckReturns(self, program);
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
pub fn typecheckProgram(self: *Typechecker, program: *AST.Program) TypeCheckerError!void {
    for (program.externalDecls.items) |externalDecl| {
        try typecheckExternalDecl(self, externalDecl);
    }
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
        std.debug.assert(std.meta.activeTag(varDecl.varInitValue.?.*) == .Expression);
        varDecl.varInitValue.?.Expression = castExpr;
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
        // TODO: This looks like a place for a bug. Future me should add a test
        // case with the following:
        // 1. int with a long number assignment (global)
        // 2. float with a long number assignment (global)
        // INFO: Clang behaviour:
        // 1. Cast to integer
        // 2. Just a warning (a cast till)
        // Probable fix:
        // 1. Copy the cast behaviour into a zig function and cast at compile time
        // 2. Throw a warning (skip untill warnings are implemented)

        initializer.* = .{ .Initial = switch (varDecl.type) {
            .Integer => .{ .type = .Integer, .value = .{ .Integer = expression.Constant.to(i32) } },
            .Long => .{ .type = .Long, .value = .{ .Long = expression.Constant.to(i64) } },
            .Float => .{ .type = .Float, .value = .{ .Float = expression.Constant.value.Float } },
            else => unreachable,
        } };
    }
}

fn globalDeclExprNotInteger(self: *Typechecker, expression: *AST.Expression, varName: []u8) TypeError!void {
    if (std.meta.activeTag(expression.*) != .Constant) {
        try self.errors.append(try std.fmt.allocPrint(
            self.allocator,
            "Global declarations only support constants for declaration of {s}\n",
            .{varName},
        ));
        return TypeError.GlobalDeclarationNotInteger;
    }
}

pub fn typecheckExternalDecl(self: *Typechecker, externalDecl: *AST.ExternalDecl) TypeCheckerError!void {
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
                    try self.errors.append(try std.fmt.allocPrint(self.allocator, "Global variable {s} redeclared as function\n", .{functionDecl.declarator.FunDeclarator.declarator.Ident}));
                    return TypeError.GlobVarRedeclaredAsFn;
                }
                const nonStaticLinkageMismatch = !sym.attributes.FunctionAttr.global and (functionDecl.storageClass != AST.Qualifier.STATIC);
                const staticLinkageMismatch = sym.attributes.FunctionAttr.global and (functionDecl.storageClass == AST.Qualifier.STATIC);
                if (nonStaticLinkageMismatch or staticLinkageMismatch) {
                    try self.errors.append(try std.fmt.allocPrint(self.allocator, "Global variable {s} redeclared as non-static\n", .{functionDecl.declarator.FunDeclarator.declarator.Ident}));
                    return TypeError.LinkageMismatch;
                }

                if (functionDecl.isDefined() and sym.isDefined()) {
                    try self.errors.append(try std.fmt.allocPrint(
                        self.allocator,
                        "Function redefinition of {s}\n",
                        .{functionDecl.declarator.FunDeclarator.declarator.Ident},
                    ));
                    return TypeError.FnRedefined;
                }

                for (0..functionDeclarator.params.items.len) |i| {
                    if (std.meta.activeTag(functionDeclarator.params.items[i].NonVoidArg.type) != std.meta.activeTag(sym.typeInfo.Function.args.items[i])) {
                        try self.errors.append(try std.fmt.allocPrint(self.allocator, "Argument mismatch in function redeclaration: function name = {s}, function arg mismatch between {any} and {any}\n", .{
                            functionDecl.declarator.FunDeclarator.declarator.Ident,
                            functionDecl.declarator.FunDeclarator.params.items[i].NonVoidArg.type,
                            sym.typeInfo.Function.args.items[i],
                        }));
                        return TypeError.FnPrevDeclArgMismatch;
                    }
                }

                // WARN: This is incorrect, write a seperate function for type
                // equality

                if (std.meta.activeTag(functionDecl.returnType) != std.meta.activeTag(sym.typeInfo.Function.returnType)) {
                    try self.errors.append(try std.fmt.allocPrint(
                        self.allocator,
                        "Function {s} has mismatching declarations\n",
                        .{functionDecl.declarator.FunDeclarator.declarator.Ident},
                    ));
                    return TypeError.FnPrevDeclArgMismatch;
                }

                if (functionDeclarator.params.items.len != sym.typeInfo.Function.args.items.len) {
                    try self.errors.append(try std.fmt.allocPrint(
                        self.allocator,
                        "Function {s} has mismatching declarations\n",
                        .{functionDecl.declarator.FunDeclarator.declarator.Ident},
                    ));
                    return TypeError.FnArgNumMismatch;
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
                try typecheckBlkItem(self, blk);
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
            if (varDecl.varInitValue) |varInitValue| {
                // INFO: Should be constant initialized
                std.debug.assert(std.meta.activeTag(varInitValue.*) == .Expression);
                const expression = varInitValue.Expression;

                try globalDeclExprNotInteger(self, expression, varName);

                // INFO: handle pointers here, no casting is really required, except
                // changing the type of the rhs explicitly(only 0 is allowed in
                // the RHS)

                if (std.meta.activeTag(varDecl.type) == .Pointer) {
                    if (!expression.isNullPtr()) {
                        try self.errors.append(try std.fmt.allocPrint(
                            self.allocator,
                            "Global pointer declarations only support 0 as rhs: assignment of {s}\n",
                            .{varDecl.declarator.Ident},
                        ));
                        return TypeError.GlobalDeclarationNotInteger;
                    }
                    varDecl.varInitValue.?.Expression.Constant.type = varDecl.type;
                    varDecl.varInitValue.?.Expression.Constant.value = .{ .ULong = 0 };
                } else {
                    try handleNonPointerExpr(self, varDecl, expression, &initializer);
                }

                // INFO: Extern decls no assignment check
                if (varDecl.storageClass == AST.Qualifier.EXTERN) {
                    try self.errors.append(try std.fmt.allocPrint(
                        self.allocator,
                        "Extern declarations cant have an assignment: {s}\n",
                        .{varDecl.declarator.Ident},
                    ));
                    return TypeError.ExternVarDeclared;
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
                    try self.errors.append(try std.fmt.allocPrint(
                        self.allocator,
                        "Function redeclared as var: {s}\n",
                        .{(try varDecl.declarator.unwrapIdentDecl()).Ident},
                    ));
                    return TypeError.FnRedeclaredAsVar;
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
                        try self.errors.append(try std.fmt.allocPrint(
                            self.allocator,
                            "Type mismatch for declaration of {s}\n",
                            .{varDecl.declarator.Ident},
                        ));
                        return TypeError.TypeMismatch;
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
                                try self.errors.append(try std.fmt.allocPrint(
                                    self.allocator,
                                    "Static and non-static(conflicting) declarations for {s}\n",
                                    .{varDecl.declarator.Ident},
                                ));
                                return TypeError.ConflictingDeclarations;
                            }
                        },
                        else => {
                            unreachable;
                        },
                    }
                }

                if (!varSym.typeInfo.isOfKind(TypeKind.from(varDecl.type))) {
                    try self.errors.append(try std.fmt.allocPrint(
                        self.allocator,
                        "Type mismatch for declaration of {s}\n",
                        .{varDecl.declarator.Ident},
                    ));
                    return TypeError.TypeMismatch;
                }

                // zig fmt: off
                if((std.meta.activeTag(varSym.attributes.StaticAttr.init) == .Initial)
                    and (std.meta.activeTag(initializer) != .Initial))
                // zig fmt: on
                {
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
    std.log.warn("From isNullPtrAssignment, is expr null ptr: {}\n", .{expr});
    return expr.isNullPtr();
}

fn typecheckInitializer(self: *Typechecker, initializer: *AST.Initializer, declType: AST.Type) TypeCheckerError!*AST.Initializer {
    switch (initializer.*) {
        .Expression => |expr| {
            initializer.Expression = try typecheckExpr(self, expr);
            std.log.warn("declaration type: {} and expression type: {}\n", .{ declType, expr.getType() });
            if (!checkConversion(declType, expr.getType()) and !isNullPtrAssignment(expr, declType)) {
                std.log.warn("error: check conversion\n", .{});
                try self.errors.append(try std.fmt.allocPrint(self.allocator, "Type error at local declaration\n", .{}));
                return TypeError.TypeMismatch;
            }

            initializer.Expression = try convert(self.allocator, initializer.Expression, declType);
            std.debug.assert(std.meta.activeTag(initializer.Expression.getType()) == declType);
        },
        .ArrayExpr => |arrayExpr| {
            initializer.ArrayExpr.type = declType;
            for (0..arrayExpr.initializers.items.len) |i| {
                initializer.ArrayExpr.initializers.items[i] = try typecheckInitializer(
                    self,
                    arrayExpr.initializers.items[i],
                    declType.decrementDepth() catch {
                        try self.errors.append(try std.fmt.allocPrint(
                            self.allocator,
                            "Type error at compound initializer, declaration of type {} expected and got array\n",
                            .{declType},
                        ));
                        return TypeCheckerError.TypeMismatch;
                    },
                );
            }
        },
    }
    return initializer;
}

fn typecheckBlkItem(self: *Typechecker, blkItem: *AST.BlockItem) TypeCheckerError!void {
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
            if (decl.varInitValue) |varInitValue| {
                // Stages of typechecking an assigned expression
                // 1. Typecheck within the expression and retrieve the
                // expression type
                // 2. Check if a possible casting rule exists for declaration
                // and expression type
                // 3. If casting is possible, cast using convert function
                // 4. Check if the cast has overwritten the type of the
                // expression(Final assert)
                decl.varInitValue = try typecheckInitializer(self, varInitValue, decl.type);

                // Not all conversions are allowed, this has to be done before
                // calling convert, if checkConversion and null pointer
                // assignment fails then its a type error
            }

            // handle qualifiers
            if (decl.storageClass) |storageClass| {
                switch (storageClass) {
                    .EXTERN => {
                        if (decl.varInitValue) |_| {
                            try self.errors.append(try std.fmt.allocPrint(
                                self.allocator,
                                "Extern variable {s} defined\n",
                                .{decl.declarator.Ident},
                            ));
                            return TypeError.ExternVarDeclared;
                        }
                        if (self.symbolTable.get(decl.declarator.Ident)) |olderSym| {
                            std.log.warn("This: {any} and {any}\n", .{ olderSym.typeInfo, TypeKind.from(decl.type) });
                            if (!olderSym.typeInfo.isOfKind(TypeKind.from(decl.type))) {
                                try self.errors.append(try std.fmt.allocPrint(
                                    self.allocator,
                                    "Extern variable type error for symbol {s}\n",
                                    .{decl.declarator.Ident},
                                ));
                                return TypeError.FnRedeclaredAsVar;
                            }

                            if (std.meta.activeTag(olderSym.typeInfo) == .Function) {
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
                        if (decl.varInitValue) |varInitValue| {
                            std.debug.assert(std.meta.activeTag(varInitValue.*) == .Expression);
                            const expr = varInitValue.Expression;

                            if (std.meta.activeTag(expr.*) != .Constant) {
                                try self.errors.append(try std.fmt.allocPrint(
                                    self.allocator,
                                    "Static variable {s} must have a constant initializer\n",
                                    .{decl.declarator.Ident},
                                ));
                                return TypeError.GlobalDeclarationNotInteger;
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
            try typecheckStmt(self, stmt);
        },
    }
}

fn typecheckStmt(self: *Typechecker, stmt: *AST.Statement) TypeCheckerError!void {
    switch (stmt.*) {
        .Return => |ret| {
            _ = try typecheckExpr(self, ret.expression);
        },
        .Expression => |expr| {
            _ = try typecheckExpr(self, expr);
        },
        .If => |ifStmt| {
            _ = try typecheckExpr(self, ifStmt.condition);
            try typecheckStmt(self, ifStmt.thenStmt);
            if (ifStmt.elseStmt != null) {
                try typecheckStmt(self, ifStmt.elseStmt.?);
            }
        },
        .Compound => |compound| {
            for (compound.items) |blkItem| {
                try typecheckBlkItem(self, blkItem);
            }
        },
        .For => |forStmt| {
            try typecheckForInit(self, forStmt.init);
            if (forStmt.condition) |condition| {
                _ = try typecheckExpr(self, condition);
            }
            if (forStmt.post) |post| {
                _ = try typecheckExpr(self, post);
            }
            try typecheckStmt(self, forStmt.body);
        },
        .DoWhile => |doWhileStmt| {
            _ = try typecheckExpr(self, doWhileStmt.condition);
            try typecheckStmt(self, doWhileStmt.body);
        },
        .While => |whileStmt| {
            _ = try typecheckExpr(self, whileStmt.condition);
            try typecheckStmt(self, whileStmt.body);
        },
        .Break => {},
        .Continue => {},
        .Label => {},
        .Goto => {},
        .Null => {},
    }
}

fn typecheckForInit(self: *Typechecker, forInit: *AST.ForInit) TypeCheckerError!void {
    switch (forInit.*) {
        .Declaration => |decl| {
            const sym = try self.allocator.create(Symbol);
            sym.* = .{
                .typeInfo = .Integer,
                .attributes = .LocalAttr,
            };
            try self.symbolTable.put(decl.declarator.Ident, sym);
            if (decl.varInitValue) |varInitValue| {
                std.debug.assert(std.meta.activeTag(varInitValue.*) == .Expression);
                const expression = varInitValue.Expression;
                _ = try typecheckExpr(self, expression);
            }
        },
        .Expression => |expr| {
            _ = try typecheckExpr(self, expr);
        },
    }
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
                const rhsCopy = try allocator.create(AST.Expression);
                rhsCopy.* = expr.*;
                castExpr.* = AST.Expression{ .Cast = .{
                    .type = toType,
                    .value = rhsCopy,
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
        .ArrSubscript => {
            if (std.meta.activeTag(expr.ArrSubscript.type.?) != toType) {
                const castExpr = try allocator.create(AST.Expression);
                castExpr.* = AST.Expression{ .Cast = .{
                    .type = toType,
                    .value = expr,
                } };
                return castExpr;
            }
            expr.ArrSubscript.type = toType;
            return expr;
        },
    }
}

fn getCommonPtrTypeForArith() !AST.Type {
    return .Long;
}

// TODO: This is funky, make a different function for pointer arithmetic
fn getCommonPtrType(self: *Typechecker, lhs: *AST.Expression, rhs: *AST.Expression) !?AST.Type {
    var type1 = lhs.getType();
    var type2 = rhs.getType();
    if (type1.deepEql(type2)) return type1;
    if (type1 == .Integer) {
        lhs.* = (try convert(self.allocator, lhs, .Long)).*;
        type1 = .Long;
    }
    if (type2 == .Integer) {
        std.log.warn("rhs is {}\n", .{rhs});
        rhs.* = (try convert(self.allocator, rhs, .Long)).*;
        std.log.warn("rhs is {}\n", .{rhs});
        type2 = .Long;
    }
    if (lhs.isNullPtr() or type1 == .Long) return type2;
    if (rhs.isNullPtr() or type2 == .Long) return type1;
    std.log.warn("no common pointer found between: {} and {}", .{ type1, type2 });
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

fn typecheckExpr(self: *Typechecker, expr: *AST.Expression) TypeError!*AST.Expression {
    // Expr can error because of a type issue,
    // for now we can just return a generic type error
    // and handle it from the outer functions (no diagnostic)
    switch (expr.*) {
        .AddrOf => |addrOf| {
            const innerType = try self.allocator.create(AST.Type);
            innerType.* = (try typecheckExpr(self, addrOf.exp)).getType();
            expr.AddrOf.type = .{ .Pointer = innerType };
            std.log.warn("Assigning type to AddrOf: pointer to {any} \n", .{innerType});
            return expr;
        },
        .Deref => |deref| {
            const innerType = (try typecheckExpr(self, deref.exp)).getType();
            if (std.meta.activeTag(innerType) != .Pointer) {
                std.log.warn("Dereferenced type is not a pointer, got {any} instead\n", .{std.meta.activeTag(innerType)});
                return TypeError.InvalidOperand;
            }
            expr.Deref.type = innerType.Pointer.*;
            return expr;
        },
        .Cast => {
            return expr;
        },
        .Assignment => |assignment| {
            const lhsType = (try typecheckExpr(self, assignment.lhs)).getType();
            _ = try typecheckExpr(self, assignment.rhs);
            expr.Assignment.rhs = try convert(self.allocator, assignment.rhs, lhsType);
            expr.Assignment.type = lhsType;
            return expr;
        },
        .Binary => {
            const lhsType = (try typecheckExpr(self, expr.Binary.lhs)).getType();
            const rhsType = (try typecheckExpr(self, expr.Binary.rhs)).getType();
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
                try self.errors.append(
                    try std.fmt.allocPrint(self.allocator, "Cant compare pointers and non pointers\n", .{}),
                );
                return TypeError.InvalidOperand;
            }

            //INFO: Conversion logic
            const commonType = if (isLhsPointer or isRhsPointer) (try getCommonPtrType(
                self,
                expr.Binary.lhs,
                expr.Binary.rhs,
            )).? else getCommonType(lhsType, rhsType);

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

            return expr;
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
                const argType = (try typecheckExpr(self, fnCall.args.items[i])).getType();
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
            return try convert(self.allocator, expr, fnSymbol.typeInfo.Function.returnType);
        },
        .Identifier => |identifier| {
            const symbol = if (self.symbolTable.get(identifier.name)) |sym| sym else {
                std.log.warn("Unknown identifier: {s}\n", .{identifier.name});
                return TypeError.UnknownIdentifier;
            };
            const astType = try AST.Type.fromSemType(&symbol.typeInfo, self.allocator);
            std.log.warn("Identifier({s}) type registered as: {any}", .{ identifier.name, symbol.typeInfo });
            expr.Identifier.type = astType;
            return expr;
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
            return expr;
        },
        .Unary => |unary| {
            const exprType = (try typecheckExpr(self, unary.exp)).getType();
            if (exprType == .Float and unary.unaryOp == .COMPLEMENT) {
                std.log.warn("operation {any} cannot be applied to {any}", .{ unary.unaryOp, exprType });
                return TypeError.InvalidOperand;
            }
            expr.Unary.type = exprType;
            return expr;
        },
        .Ternary => |ternary| {
            _ = try typecheckExpr(self, ternary.condition);

            const lhsType = (try typecheckExpr(self, ternary.lhs)).getType();
            const rhsType = (try typecheckExpr(self, ternary.rhs)).getType();
            std.log.warn("rhs is {}\n", .{ternary.rhs});
            std.log.warn("Ternary lhs type: {} and ternary rhs type: {}\n", .{ lhsType, rhsType });
            const ternaryLhsPointer = std.meta.activeTag(lhsType) == .Pointer;
            const ternaryRhsPointer = std.meta.activeTag(rhsType) == .Pointer;

            // zig fmt: off
            expr.Ternary.type = if (ternaryLhsPointer or ternaryRhsPointer) try getCommonPtrType(self, ternary.lhs, ternary.rhs) 
                                else getCommonType(lhsType, rhsType);
            //zig fmt:on
            std.debug.assert(expr.Ternary.type != null);
            if (!checkConversion(lhsType, expr.Ternary.type.?) and !isNullPtrAssignment(ternary.lhs, expr.Ternary.type.?)) {
                std.log.warn("Invalid types: lhs type {any}, rhs type {any}, ternary type {any}\n", .{ lhsType, rhsType, expr.Ternary.type });
                return TypeError.TypeMismatch;
            }
            expr.Ternary.lhs = try convert(self.allocator, ternary.lhs, expr.Ternary.type.?);

            if (!checkConversion(rhsType, expr.Ternary.type.?) and !isNullPtrAssignment(ternary.rhs, expr.Ternary.type.?)) {
                std.log.warn("Invalid types: lhs type {any}, rhs type {any}, ternary type {any}\n", .{ lhsType, rhsType, expr.Ternary.type });
                return TypeError.TypeMismatch;
            }
            expr.Ternary.rhs = try convert(self.allocator, ternary.rhs, expr.Ternary.type.?);
            return expr;
        },
        .ArrSubscript => |arrSubscript| {
            expr.ArrSubscript.arr = try typecheckExpr(self, arrSubscript.arr);
            expr.ArrSubscript.index = try typecheckExpr(self, arrSubscript.index);
            if(expr.ArrSubscript.index.getType() != .ULong){
                expr.ArrSubscript.index = try convert(self.allocator, expr.ArrSubscript.index, .ULong);
            }

            // Error cases
            if(arrSubscript.arr.getType() != .Pointer and arrSubscript.arr.getType() != .Integer){
                return TypeError.TypeMismatch;
            }
            if(arrSubscript.index.getType() != .Integer and arrSubscript.index.getType() != .Pointer){
                return TypeError.TypeMismatch;
            }

            // if neither arr nor index is a pointer, we need to throw an error
            expr.ArrSubscript.type = (if(std.meta.activeTag(arrSubscript.arr.getType()) == .Pointer) arrSubscript.arr else arrSubscript.index).getType().decrementDepth() catch {
                try self.errors.append(try std.fmt.allocPrint(self.allocator, "Array subscript error: cant index non arrays\n", .{}));
                return TypeError.TypeMismatch;
            };
            return expr;
        }
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
