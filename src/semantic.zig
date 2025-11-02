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
    Initial: []const Constant,
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
    Array,

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
            .Function => unreachable,
        };
    }
};

pub const Symbol = struct {
    typeInfo: AST.Type,
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

    fn addFileScopeSymbol(self: *Typechecker, name: []u8, t: AST.Type, global: bool, initValue: InitValue) !void {
        const sym = try self.allocator.create(Symbol);
        sym.* = .{
            .typeInfo = t,
            .attributes = .{
                .StaticAttr = .{
                    .global = global,
                    .init = initValue,
                },
            },
        };
        try self.symbolTable.put(name, sym);
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

inline fn pointerDeclWithScalarNonNullRhs(self: *Typechecker, varDecl: *AST.Declaration, varInitValue: *AST.Initializer, varName: []u8) !void {
    const varDeclType = varDecl.type;
    if (std.meta.activeTag(varDeclType) == .Pointer and std.meta.activeTag(varInitValue.*) == .Expression and !varInitValue.Expression.isNullPtr()) {
        try self.errors.append(try std.fmt.allocPrint(
            self.allocator,
            "Global pointer declarations only support 0 as rhs: assignment of {s}\n",
            .{varName},
        ));
        return TypeError.GlobalDeclarationNotInteger;
    }
}

fn castExprToDeclType(self: *Typechecker, varDeclType: AST.Type, varInitValue: *AST.Initializer) !*AST.Initializer {
    switch (varInitValue.*) {
        .Expression => |expr| {
            if (std.meta.activeTag(varDeclType) == .Pointer) {
                std.debug.assert(expr.isNullPtr()); // call pointerDeclWithScalarNonNullRhs before this
                const initializer = try self.allocator.create(AST.Initializer);
                const expression = try self.allocator.create(AST.Expression);
                expression.* = .{ .Constant = .{ .type = varDeclType, .value = .{ .ULong = 0 } } };
                initializer.* = .{ .Expression = expression };
                return initializer;
            } else {
                const exprType = expr.getType();
                const initializer = try self.allocator.create(AST.Initializer);
                initializer.* = if (!varDeclType.deepEql(exprType)) .{ .Expression = try convert(self.allocator, expr, varDeclType) } else .{ .Expression = expr };
                return initializer;
            }
        },
        .ArrayExpr => |arrayExpr| {
            const arrMemberType = try varDeclType.decrementDepth();
            const initializer = try self.allocator.create(AST.Initializer);
            var initializers = try std.ArrayList(*AST.Initializer).initCapacity(self.allocator, arrayExpr.initializers.items.len);
            const arrayInitializer = try self.allocator.create(AST.ArrayInitializer);
            for (arrayExpr.initializers.items) |initValue| {
                initializers.appendAssumeCapacity(try castExprToDeclType(self, arrMemberType, initValue));
            }
            arrayInitializer.* = .{
                .type = varDeclType,
                .initializers = initializers,
            };
            initializer.* = .{
                .ArrayExpr = arrayInitializer,
            };
            return initializer;
        },
    }
}

fn getInitializerValue(self: *Typechecker, varDeclType: AST.Type, initializer: *AST.Initializer) !*InitValue {
    const initVal = try self.allocator.create(InitValue);
    switch (initializer.*) {
        .ArrayExpr => |arrayExpr| {
            var initValues = std.ArrayList(Constant).init(self.allocator);
            const arrMemberType = varDeclType.decrementDepth() catch unreachable;
            for (arrayExpr.initializers.items) |arrayItemInitializer| {
                const arrMemberInitValue = try getInitializerValue(self, arrMemberType, arrayItemInitializer);
                std.debug.assert(std.meta.activeTag(arrMemberInitValue.*) == .Initial);
                try initValues.appendSlice(arrMemberInitValue.Initial);
            }
            initVal.* = .{ .Initial = try initValues.toOwnedSlice() };
        },
        .Expression => |expression| {
            var constants = try std.ArrayList(Constant).initCapacity(self.allocator, 1);
            const constant: Constant = switch (varDeclType) {
                .Void => unreachable,
                .Array => unreachable,
                .Pointer => blk: {
                    std.debug.assert(expression.isNullPtr());
                    break :blk .{
                        .type = .ULong,
                        .value = .{ .ULong = 0 },
                    };
                },
                .Float => .{
                    .type = .Float,
                    .value = .{ .Float = expression.Constant.value.Float },
                },
                .Integer => .{
                    .type = .Integer,
                    .value = .{ .Integer = expression.Constant.to(i32) },
                },
                .Long => .{
                    .type = .Long,
                    .value = .{ .Long = expression.Constant.to(i64) },
                },
                .UInteger => .{
                    .type = .UInteger,
                    .value = .{ .UInteger = expression.Constant.to(u32) },
                },
                .ULong => .{
                    .type = .ULong,
                    .value = .{ .ULong = expression.Constant.to(u64) },
                },
                .Function => unreachable,
            };
            constants.appendAssumeCapacity(constant);
            initVal.* = .{
                .Initial = try constants.toOwnedSlice(),
            };
        },
    }
    std.log.warn("Initval = {any}, initVal.Initial = {any}\n", .{ initVal, initVal.Initial });
    return initVal;
}

/// This function is used to inherit the attributes of an extern
/// declaration that was earlier defined as a varSym
/// int k = 4;
/// int main(){
///     extern int k;
///     printf("k = %d\n",k);
/// }
/// prints k = 4
inline fn externInheritEarlyDecl(varDecl: *AST.Declaration, varSym: *Symbol, global: *bool) void {
    if (varDecl.storageClass == .EXTERN and std.meta.activeTag(varSym.attributes) == .StaticAttr) {
        global.* = varSym.attributes.StaticAttr.global;
    }
}

inline fn chooseSubsequentDecl(varSym: *Symbol, initializer: *InitValue) !void {
    // zig fmt: off
                if((std.meta.activeTag(varSym.attributes.StaticAttr.init) == .Initial)
                    and (std.meta.activeTag(initializer.*) != .Initial))
                // zig fmt: on
    {
        // INFO: If initialized again choose that value, by exiting out of
        // this block
        std.log.warn("Choosing earlier initializer value => old = {any} and new = {any}\n", .{ initializer, varSym.attributes.StaticAttr.init });
        initializer.* = varSym.attributes.StaticAttr.init;
    }
}

inline fn checkStaticNonStaticConflict(self: *Typechecker, varDecl: *AST.Declaration, varSym: *Symbol, global: bool) !void {
    std.log.warn("DEBUG: {any} , {any} , {any}, {any}\n", .{ varDecl.storageClass, varSym.attributes, varSym.attributes.StaticAttr.global, global });
    if (varDecl.storageClass != .EXTERN and std.meta.activeTag(varSym.attributes) == .StaticAttr and varSym.attributes.StaticAttr.global != global) {
        try self.errors.append(try std.fmt.allocPrint(
            self.allocator,
            "Static and non-static(conflicting) declarations for {s}\n",
            .{varDecl.name},
        ));
        return TypeError.ConflictingDeclarations;
    }
}

/// Throw an error if:
/// 1. Previous Declaration
/// 2. Extern declaration and the earlier var symbol is not of the same type
/// 3. Static - non static  conflict
inline fn validateVarDeclWithPrevDecl(self: *Typechecker, varDecl: *AST.Declaration, varSym: *Symbol, varName: []u8, global: bool) !void {
    if (std.meta.activeTag(varSym.typeInfo) == .Function) {
        try self.errors.append(try std.fmt.allocPrint(
            self.allocator,
            "Function redeclared as var: {s}\n",
            .{varName},
        ));
        return TypeError.FnRedeclaredAsVar;
    }

    //INFO: extern inherit scope (global/static)
    if (varDecl.storageClass == .EXTERN and std.meta.activeTag(varSym.typeInfo) != varDecl.type) {
        // INFO: repeated cause extern requires this (should be
        // a function at some point)
        try self.errors.append(try std.fmt.allocPrint(
            self.allocator,
            "Type mismatch for declaration of {s}\n",
            .{varName},
        ));
        return TypeError.TypeMismatch;
    }

    try checkStaticNonStaticConflict(self, varDecl, varSym, global);
}

fn globalDeclExprNotConstant(self: *Typechecker, varInitValue: *AST.Initializer, varName: []u8) TypeError!void {
    switch (varInitValue.*) {
        .Expression => |expression| {
            std.log.warn("globalDeclExprNotConstant => Expression: {any}\n", .{expression});
            if (std.meta.activeTag(expression.*) != .Constant) {
                try self.errors.append(try std.fmt.allocPrint(
                    self.allocator,
                    "Global declarations only support constants for declaration of {s}\n",
                    .{varName},
                ));
                return TypeError.GlobalDeclarationNotInteger;
            }
        },
        .ArrayExpr => |arrayExpr| {
            for (arrayExpr.initializers.items) |arrayItem| {
                try globalDeclExprNotConstant(self, arrayItem, varName);
            }
        },
    }
}

inline fn checkRedeclaration(self: *Typechecker, varSym: *Symbol, varDeclType: AST.Type, varName: []u8) !void {
    if (std.meta.activeTag(varSym.typeInfo) != varDeclType) {
        try self.errors.append(try std.fmt.allocPrint(
            self.allocator,
            "Type mismatch for declaration of {s}\n",
            .{varName},
        ));
        return TypeError.TypeMismatch;
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
                if (std.meta.activeTag(sym.typeInfo) != .Function) {
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
                    if (std.meta.activeTag(functionDeclarator.params.items[i].NonVoidArg.type) != std.meta.activeTag(sym.typeInfo.Function.args.items[i].*)) {
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

                if (std.meta.activeTag(functionDecl.returnType) != std.meta.activeTag(sym.typeInfo.Function.returnType.*)) {
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
            var fnArgsList = std.ArrayList(*AST.Type).init(self.allocator);
            std.log.warn("Params for {s}:\n", .{fnName});
            for (functionDeclarator.params.items) |arg| {
                arg.NonVoidArg.fixType(self.allocator) catch unreachable;
                std.log.warn("\targ: {any}\n", .{arg.NonVoidArg.type});
                try fnArgsList.append(&arg.NonVoidArg.type);
            }
            fnSym.* = .{
                .typeInfo = .{
                    .Function = .{
                        .args = fnArgsList,
                        .returnType = &functionDecl.returnType,
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
                    .typeInfo = arg.NonVoidArg.type,
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

            var initializer: InitValue = .NoInit;
            var global = varDecl.storageClass != AST.Qualifier.STATIC;
            const varName = varDecl.name;

            // INFO: Has expression
            if (varDecl.varInitValue) |varInitValue| {
                // INFO: Should be constant initialized

                try globalDeclExprNotConstant(self, varInitValue, varName);

                // INFO: handle pointers here, no casting is really required, except
                // changing the type of the rhs explicitly(only 0 is allowed in
                // the RHS)
                try pointerDeclWithScalarNonNullRhs(self, varDecl, varInitValue, varName);

                varDecl.varInitValue = try castExprToDeclType(self, varDecl.type, varInitValue);
                initializer = (try getInitializerValue(self, varDecl.type, varInitValue)).*;
                std.log.warn("Got initializer from getInitializerValue: {any}\n", .{initializer});

                // INFO: Extern decls no assignment check
                if (varDecl.storageClass == AST.Qualifier.EXTERN) {
                    try self.errors.append(try std.fmt.allocPrint(
                        self.allocator,
                        "Extern declarations cant have an assignment: {s}\n",
                        .{varDecl.name},
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

            if (self.symbolTable.get(varName)) |varSym| {
                std.log.warn("DEBUG: Validating symbol {s}\n", .{varName});
                try validateVarDeclWithPrevDecl(self, varDecl, varSym, varName, global);

                externInheritEarlyDecl(varDecl, varSym, &global);

                try chooseSubsequentDecl(varSym, &initializer);

                try checkRedeclaration(self, varSym, varDecl.type, varName);
            }

            try self.addFileScopeSymbol(varName, varDecl.type, global, initializer);
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
    const newInitializer = try self.allocator.create(AST.Initializer);
    newInitializer.* = initializer.*;
    switch (initializer.*) {
        .Expression => |expr| {
            newInitializer.Expression = try typecheckExpr(self, expr);
            // Normalize declaration type: decay arrays within nested structure to pointers
            const declPtrType = declType;
            std.log.warn("declaration type: {} and expression type: {}\n", .{ declPtrType, expr.getType() });
            if (!checkConversion(declPtrType, expr.getType()) and !isNullPtrAssignment(expr, declType)) {
                std.log.warn("error: check conversion: {any} to {any}\n", .{ declType, expr.getType() });
                try self.errors.append(try std.fmt.allocPrint(self.allocator, "Type error at local declaration\n", .{}));
                return TypeError.TypeMismatch;
            }

            newInitializer.Expression = try convert(self.allocator, newInitializer.Expression, declPtrType);
            std.debug.assert(std.meta.activeTag(newInitializer.Expression.getType()) == declPtrType);
        },
        .ArrayExpr => |arrayExpr| {
            for (0..arrayExpr.initializers.items.len) |i| {
                newInitializer.ArrayExpr.initializers.items[i] = try typecheckInitializer(
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
            newInitializer.ArrayExpr.type = declType;
        },
    }
    return newInitializer;
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
            std.log.warn("decl is {any}\n", .{decl});
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
                                .{decl.name},
                            ));
                            return TypeError.ExternVarDeclared;
                        }
                        if (self.symbolTable.get(decl.name)) |olderSym| {
                            std.log.warn("This: {any} and {any}\n", .{ olderSym.typeInfo, TypeKind.from(decl.type) });
                            if (std.meta.activeTag(olderSym.typeInfo) != decl.type) {
                                try self.errors.append(try std.fmt.allocPrint(
                                    self.allocator,
                                    "Extern variable type error for symbol {s}\n",
                                    .{decl.name},
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
                        if (decl.varInitValue) |varInitValue| {
                            std.debug.assert(std.meta.activeTag(varInitValue.*) == .Expression);
                            const expr = varInitValue.Expression;

                            if (std.meta.activeTag(expr.*) != .Constant) {
                                try self.errors.append(try std.fmt.allocPrint(
                                    self.allocator,
                                    "Static variable {s} must have a constant initializer\n",
                                    .{decl.name},
                                ));
                                return TypeError.GlobalDeclarationNotInteger;
                            }
                            const sym = try self.allocator.create(Symbol);
                            const attributes = try self.allocator.alloc(Constant, 1);
                            attributes[0] = switch (decl.type) {
                                .Integer => Constant{ .type = .Integer, .value = .{ .Integer = expr.Constant.value.Integer } },
                                .Long => Constant{ .type = .Long, .value = .{ .Long = expr.Constant.value.Long } },
                                .ULong => Constant{ .type = .ULong, .value = .{ .ULong = expr.Constant.value.ULong } },
                                .UInteger => Constant{ .type = .UInteger, .value = .{ .UInteger = expr.Constant.value.UInteger } },
                                .Float => Constant{ .type = .Float, .value = .{ .Float = expr.Constant.value.Float } },
                                else => unreachable,
                            };
                            // TODO: accomodate longs
                            sym.* = .{ .typeInfo = .Integer, .attributes = .{
                                .StaticAttr = .{
                                    .init = .{ .Initial = attributes },
                                    .global = true,
                                },
                            } };
                            try self.symbolTable.put(decl.name, sym);
                        } else {
                            const sym = try self.allocator.create(Symbol);
                            sym.* = .{
                                .typeInfo = .Integer,
                                .attributes = .{ .StaticAttr = .{
                                    .init = blk: {
                                        break :blk .{ .Initial = &[_]Constant{
                                            Constant{
                                                .type = .Integer,
                                                .value = .{ .Integer = 0 },
                                            },
                                        } };
                                    },
                                    .global = false,
                                } },
                            };
                            try self.symbolTable.put(decl.name, sym);
                        }
                    },
                }
            } else {
                // No decl
                std.log.warn("Adding local declaration for {s}\n", .{decl.name});
                const sym = try self.allocator.create(Symbol);
                sym.* = .{
                    .typeInfo = decl.type,
                    .attributes = .LocalAttr,
                };
                try self.symbolTable.put(decl.name, sym);
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
            try self.symbolTable.put(decl.name, sym);
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
    // Centralize array-to-pointer decay: whenever a conversion target is a Pointer
    // and the source expression currently has an Array type, rewrite the
    // expression as an AddrOf to model C's array-to-pointer decay semantics.
    // This ensures consistent behavior across assignment, binary arithmetic,
    // function arguments, ternary expressions, and returns, without sprinkling
    // decay logic in each caller.
    const expr_ty = expr.getType();
    if (std.meta.activeTag(expr_ty) == .Array and std.meta.activeTag(toType) == .Pointer) {
        const inner = try allocator.create(AST.Expression);
        inner.* = expr.*;
        expr.* = .{ .AddrOf = .{ .exp = inner, .type = toType } };
        return expr;
    }
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

inline fn arrOrPtr(t: AST.Type) ?AST.Type {
    return switch (t) {
        .Pointer, .Array => t,
        else => null,
    };
}

pub fn getArrSubscriptType(self: *Typechecker, arrSubscript: *AST.ArrSubscript) TypeError!AST.Type {
    const arr_ty = arrSubscript.arr.getType();
    const idx_ty = arrSubscript.index.getType();

    const arr_choice = arrOrPtr(arr_ty);
    const idx_choice = arrOrPtr(idx_ty);

    // At least one side must be array/pointer; otherwise this is invalid.
    if (arr_choice == null and idx_choice == null) {
        try self.errors.append(try std.fmt.allocPrint(
            self.allocator,
            "Invalid array subscript: expected pointer or array, got a {any}, {any} pair",
            .{ arr_ty, idx_ty },
        ));
        return TypeError.InvalidOperand;
    }

    const chosen = (arr_choice orelse idx_choice orelse unreachable);

    // Cast the actual index expression to ULong (size_t) if needed.
    // If the array/pointer is on the left, the right is the index; otherwise left is the index.
    if (arr_choice != null) {
        arrSubscript.index = try convert(self.allocator, arrSubscript.index, .ULong);
        std.log.warn("arrSubscript.index = {any}\n", .{arrSubscript.index});
    } else {
        arrSubscript.arr = try convert(self.allocator, arrSubscript.arr, .ULong);
        std.log.warn("arrSubscript.arr = {any}\n", .{arrSubscript.arr});
    }

    // Return the element type after one level of indexing.
    return switch (chosen) {
        .Pointer => chosen.decrementDepth() catch unreachable,
        .Array => |arrayTy| (try arrayTy.toPointer(self.allocator)).decrementDepth() catch unreachable,
        else => unreachable,
    };
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
            const checkedRhs = try typecheckExpr(self, assignment.rhs);
            expr.Assignment.rhs = try convert(self.allocator, checkedRhs, lhsType);
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
            std.log.warn("Converting lhs and rhs to common type = {any}\n", .{commonType});
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
            std.log.warn("Before function call arguments are modified\n", .{});
            for (0..fnSymbol.typeInfo.Function.args.items.len) |i| {
                std.log.warn("argument {d}: {any}\n", .{ i, fnCall.args.items[i] });
            }
            for (0..fnSymbol.typeInfo.Function.args.items.len) |i| {
                const arg = try typecheckExpr(self, fnCall.args.items[i]);
                const argType = arg.getType();

                expr.FunctionCall.args.items[i] = if (std.meta.activeTag(fnSymbol.typeInfo.Function.args.items[i].*) != std.meta.activeTag(argType)) blk: {
                    std.log.warn("Casting function argument in fn: {s} from {any} to {any}\n", .{
                        fnCall.name,
                        fnCall.args.items[i].getType(),
                        fnSymbol.typeInfo.Function.args.items[i],
                    });
                    break :blk try convert(self.allocator, arg, fnSymbol.typeInfo.Function.args.items[i].*);
                } else arg;
            }
            std.log.warn("Post function call arguments are modified\n", .{});
            for (0..fnSymbol.typeInfo.Function.args.items.len) |i| {
                std.log.warn("argument {d}: {any}\n", .{ i, fnCall.args.items[i] });
            }
            expr.FunctionCall.type = fnSymbol.typeInfo.Function.returnType.*;
            // FIXME: This convert call is stupid, delete it after some checks
            return try convert(self.allocator, expr, fnSymbol.typeInfo.Function.returnType.*);
        },
        .Identifier => |identifier| {
            const symbol = if (self.symbolTable.get(identifier.name)) |sym| sym else {
                std.log.warn("Unknown identifier: {s}\n", .{identifier.name});
                return TypeError.UnknownIdentifier;
            };
            if (std.meta.activeTag(symbol.typeInfo) == .Array) {
                const innerExpr = try self.allocator.create(AST.Expression);
                innerExpr.* = expr.*;
                expr.* = .{
                    .AddrOf = .{
                        .exp = innerExpr,
                        .type = symbol.typeInfo.changeArrtoPointer(),
                    },
                };
                return expr;
            }
            const astType = symbol.typeInfo;
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
                            .init = .{ .Initial = blk: {
                                const constants = try self.allocator.alloc(Constant, 1);
                                constants[0] = Constant{
                                    .type = .Float,
                                    .value = .{ .Float = constant.value.Float },
                                };
                                break :blk constants;
                            } },
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
            if (!checkConversion(lhsType, expr.Ternary.type.?) and !isNullPtrAssignment(ternary.lhs,expr.Ternary.type.?)) {
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

            // Error cases
            // if neither arr nor index is a pointer, we need to throw an error
            expr.ArrSubscript.type = try getArrSubscriptType(self, @constCast(&expr.ArrSubscript));
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
