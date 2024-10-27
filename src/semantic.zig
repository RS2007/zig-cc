// Contains the semantic passes

// Do stack resolution first
// All variables have unique names
// How to do error recovery?

const std = @import("std");
const AST = @import("./AST.zig");

const SemanticError = error{
    VarNotDeclared,
    OutOfMemory,
    UnknownLabel,
    NoSpaceLeft,
};

const TypeError = error{ TypeMismatch, OutOfMemory, UnknownFunction, UnknownIdentifier };
const TypeCheckerError = error{OutOfMemory} || TypeError;

pub const TypeErrorStruct = struct {
    errorType: ?TypeError,
    errorPayload: []u8,
};

pub const SymbolKind = enum {
    Function,
    Void,
    Integer,
};
pub const FnSymbol = struct {
    argsLen: u32,
    returnType: AST.Type,
};

pub const Symbol = union(SymbolKind) {
    Function: FnSymbol,
    //INFO: For now we just keep the count,
    // later we will keep track of the types of the args
    Void,
    Integer,
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
        const typeErrorStruct = try typecheckProgram(self, program);
        if (typeErrorStruct != null and typeErrorStruct.?.errorType != null) {
            return typeErrorStruct.?.errorPayload;
        }
        return null;
    }
};

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
        .FunctionDecl => |functionDecl| {
            const fnSym = try self.allocator.create(Symbol);
            fnSym.* = .{
                .Function = .{
                    .argsLen = @intCast(functionDecl.args.items.len),
                    .returnType = functionDecl.returnType,
                },
            };
            try self.symbolTable.put(
                functionDecl.name,
                fnSym,
            );
            for (functionDecl.args.items) |arg| {
                switch (arg.*) {
                    .Void => {
                        // do nothing
                    },
                    .NonVoidArg => |nonVoid| {
                        std.debug.assert(nonVoid.type == AST.Type.Integer);
                        const sym = try self.allocator.create(Symbol);
                        sym.* = .Integer;
                        try self.symbolTable.put(
                            nonVoid.identifier,
                            sym,
                        );
                    },
                }
                //try self.symbolTable.put();
            }
            for (functionDecl.blockItems.items) |blkItem| {
                const blockTypeCheckResult = try typecheckBlkItem(self, blkItem);
                if (blockTypeCheckResult) |blockTypeCheck| {
                    return blockTypeCheck;
                }
            }
        },
        .VarDeclaration => |varDecl| {
            // There should be a global scope
            std.debug.assert(varDecl.type == AST.Type.Integer);
            const sym = try self.allocator.create(Symbol);
            if (varDecl.expression != null) {
                _ = typecheckExpr(self, varDecl.expression.?) catch |err| {
                    const typeErrorStruct = try self.allocator.create(TypeErrorStruct);
                    typeErrorStruct.* = .{
                        .errorType = err,
                        .errorPayload = (try std.fmt.allocPrint(self.allocator, "Type error at global declaration of {s}\n", .{varDecl.name})),
                    };
                    return typeErrorStruct;
                };
            }
            sym.* = .Integer;
            try self.symbolTable.put(
                varDecl.name,
                sym,
            );
        },
    }
    return null;
}
fn typecheckBlkItem(self: *Typechecker, blkItem: *AST.BlockItem) TypeCheckerError!?*TypeErrorStruct {
    switch (blkItem.*) {
        .Declaration => |decl| {
            if (decl.expression != null) {
                const exprType = typecheckExpr(self, decl.expression.?) catch |err| {
                    const typeErrorStruct = try self.allocator.create(TypeErrorStruct);
                    typeErrorStruct.* = .{
                        .errorType = err,
                        .errorPayload = (try std.fmt.allocPrint(self.allocator, "Type error at local declaration of {s}\n", .{decl.name})),
                    };
                    return typeErrorStruct;
                };
                if (decl.type != exprType) {
                    const typeErrorStruct = try self.allocator.create(TypeErrorStruct);
                    typeErrorStruct.* = .{
                        .errorType = TypeError.TypeMismatch,
                        .errorPayload = (try std.fmt.allocPrint(self.allocator, "Type mismatch: local declaration of {s} is {any} but expression is {any}\n", .{ decl.name, decl.type, exprType })),
                    };
                    return typeErrorStruct;
                }
            }

            const sym = try self.allocator.create(Symbol);
            std.debug.assert(decl.type == AST.Type.Integer);
            sym.* = .Integer;
            try self.symbolTable.put(decl.name, sym);
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
            if (decl.expression) |declExpression| {
                _ = typecheckExpr(self, declExpression) catch |err| {
                    const typeErrorStruct = try self.allocator.create(TypeErrorStruct);
                    typeErrorStruct.* = .{
                        .errorType = err,
                        .errorPayload = (try std.fmt.allocPrint(self.allocator, "Type error at for init\n", .{})),
                    };
                    return typeErrorStruct;
                };
                std.debug.assert(decl.type == AST.Type.Integer);
                const sym = try self.allocator.create(Symbol);
                sym.* = .Integer;
                try self.symbolTable.put(decl.name, sym);
            }
        },
        .Expression => |expr| {
            _ = typecheckExpr(self, expr) catch |err| {
                const typeErrorStruct = try self.allocator.create(TypeErrorStruct);
                typeErrorStruct.* = .{
                    .errorType = err,
                    .errorPayload = (try std.fmt.allocPrint(self.allocator, "Type error at for init\n", .{})),
                };
                return typeErrorStruct;
            };
        },
    }
    return null;
}

fn typecheckExpr(self: *Typechecker, expr: *AST.Expression) TypeError!AST.Type {
    // Expr can error because of a type issue,
    // for now we can just return a generic type error
    // and handle it from the outer functions (no diagnostic)
    switch (expr.*) {
        .Assignment => |assignment| {
            const lhsType = try typecheckExpr(self, assignment.lhs);
            std.log.warn("lhs type: {any}\n", .{lhsType});
            const rhsType = try typecheckExpr(self, assignment.rhs);
            std.log.warn("rhs type: {any}\n", .{rhsType});
            if (lhsType != rhsType) {
                std.log.warn("lhs type: {any} and rhs type: {any} not matching at assignment\n", .{ lhsType, rhsType });
                return TypeError.TypeMismatch;
            }
            return AST.Type.Integer;
        },
        .Binary => {
            const lhsType = try typecheckExpr(self, expr.Binary.lhs);
            const rhsType = try typecheckExpr(self, expr.Binary.rhs);
            if (lhsType != rhsType) {
                std.log.warn("lhs type: {any} and rhs type: {any} not matching at binary op\n", .{ lhsType, rhsType });
                return TypeError.TypeMismatch;
            }
            return AST.Type.Integer;
        },
        .FunctionCall => |fnCall| {
            const fnSymbol = if (self.symbolTable.get(fnCall.name)) |fnSym| fnSym else {
                std.log.warn("Unknown function: {s}\n", .{fnCall.name});
                return TypeError.UnknownFunction;
            };
            if (fnCall.args.items.len != fnSymbol.Function.argsLen) {
                std.log.warn("Expected {d} arguments but found {d} arguments in {s}\n", .{ fnSymbol.Function.argsLen, fnCall.args.items.len, fnCall.name });
                return TypeError.TypeMismatch;
            }
            return fnSymbol.Function.returnType;
        },
        .Identifier => |identifier| {
            const symbol = if (self.symbolTable.get(identifier)) |sym| sym else {
                std.log.warn("Unknown identifier: {s}\n", .{identifier});
                return TypeError.UnknownIdentifier;
            };
            std.debug.assert(std.mem.eql(u8, @tagName(symbol.*), "Integer"));
            return AST.Type.Integer;
        },
        .Integer => {
            return AST.Type.Integer;
        },
        .Unary => |unary| {
            _ = try typecheckExpr(self, unary.exp);
            return AST.Type.Integer;
        },
        .Ternary => |ternary| {
            _ = try typecheckExpr(self, ternary.condition);
            const lhsType = try typecheckExpr(self, ternary.lhs);
            std.debug.assert(lhsType == AST.Type.Integer);
            const rhsType = try typecheckExpr(self, ternary.rhs);
            std.debug.assert(rhsType == AST.Type.Integer);
            return AST.Type.Integer;
        },
    }
}

//pub fn typechecker(program: *AST.Program, allocator: std.mem.Allocator) !?*TypeErrorStruct {
//    // INFO: Only typechecking requirement right now is function call return
//    // INFO: Maintain a function metadata hashmap and query that to check if the return type match
//    // Buildling a metadata map for functions.
//    var fnMetaDataMap = std.StringHashMap(FnMetaData).init(allocator);
//    for (program.externalDecls.items) |externalDecl| {
//        switch (externalDecl.*) {
//            .FunctionDecl => |fnDecl| {
//                const localVarTypeMap = std.StringHashMap(AST.Type).init(allocator);
//                try fnMetaDataMap.put(
//                    fnDecl.name,
//                    .{
//                        .returnType = fnDecl.returnType,
//                        .localVarTypeMap = @constCast(&localVarTypeMap),
//                    },
//                );
//            },
//            .VarDeclaration => {},
//        }
//    }
//    for (program.externalDecls.items) |externalDecl| {
//        switch (externalDecl.*) {
//            .FunctionDecl => |fnDecl| {
//                for (fnDecl.blockItems.items) |blkItem| {
//                    switch (blkItem.*) {
//                        .Declaration => |decl| {
//                            try fnMetaDataMap.get(fnDecl.name).?.localVarTypeMap.put(
//                                decl.name,
//                                decl.type,
//                            );
//                        },
//                        .Statement => {},
//                    }
//                }
//            },
//            .VarDeclaration => {},
//        }
//    }
//    var iter = fnMetaDataMap.iterator();
//    while (iter.next()) |entry| {
//        std.log.warn("{s} : \n", .{entry.key_ptr.*});
//        var varIter = entry.value_ptr.localVarTypeMap.iterator();
//        while (varIter.next()) |varEntry| {
//            std.log.warn("{s}: {any}\n", .{ varEntry.key_ptr.*, varEntry.value_ptr.* });
//        }
//    }
//    for (program.externalDecls.items) |externalDecl| {
//        switch (externalDecl.*) {
//            .FunctionDecl => |fnDecl| {
//                for (fnDecl.blockItems.items) |blkItem| {
//                    switch (blkItem.*) {
//                        .Statement => |stmt| {
//                            // Check assignment statements
//                            if (std.mem.eql(u8, @tagName(stmt.*), "Expression")) {
//                                if (std.mem.eql(u8, @tagName(stmt.Expression.*), "Assignment")) {
//                                    if (std.mem.eql(u8, @tagName(stmt.Expression.Assignment.rhs.*), "FunctionCall")) {
//                                        std.debug.assert(std.mem.eql(u8, @tagName(stmt.Expression.Assignment.lhs.*), "Identifier"));
//                                        const lhsType = fnMetaDataMap.get(fnDecl.name).?.localVarTypeMap.get(stmt.Expression.Assignment.lhs.Identifier).?;
//                                        const rhsType = fnMetaDataMap.get(stmt.Expression.Assignment.rhs.FunctionCall.name).?.returnType;
//                                        if (lhsType != rhsType) {
//                                            const typeErrorStruct = try allocator.create(TypeErrorStruct);
//                                            typeErrorStruct.* = .{
//                                                .errorType = TypeError.TypeMismatch,
//                                                .errorPayload = (try std.fmt.allocPrint(allocator, "Type mismatch: lhs type: {any} and rhs type: {any}\n", .{ lhsType, rhsType })),
//                                            };
//                                            return typeErrorStruct;
//                                        }
//                                    }
//                                }
//                            }
//                            // Check return statements in the function and check the type
//                            if (std.mem.eql(u8, @tagName(stmt.*), "Return")) {
//                                if (std.mem.eql(u8, @tagName(stmt.Return.expression.*), "Identifier")) {
//                                    const identifierType = fnMetaDataMap.get(fnDecl.name).?.localVarTypeMap.get(stmt.Return.expression.Identifier).?;
//                                    if (identifierType != fnDecl.returnType) {
//                                        const typeErrorStruct = try allocator.create(TypeErrorStruct);
//                                        typeErrorStruct.* = .{
//                                            .errorType = TypeError.TypeMismatch,
//                                            .errorPayload = (try std.fmt.allocPrint(allocator, "Type mismatch: function supposed to return: {any} and found type in return: {any}\n", .{ fnDecl.returnType, identifierType })),
//                                        };
//                                        return typeErrorStruct;
//                                    }
//                                }
//                            }
//                        },
//                        .Declaration => |decl| {
//                            // Check declaration assignments
//                            if (decl.expression) |expr| {
//                                if (std.mem.eql(u8, @tagName(expr.*), "FunctionCall")) {
//                                    if (decl.type != fnMetaDataMap.get(expr.FunctionCall.name).?.returnType) {
//                                        const typeErrorStruct = try allocator.create(TypeErrorStruct);
//                                        typeErrorStruct.* = .{
//                                            .errorType = TypeError.TypeMismatch,
//                                            .errorPayload = (try std.fmt.allocPrint(allocator, "Type mismatch: lhs type: {any} and rhs type: {any}\n", .{ decl.type, fnMetaDataMap.get(expr.FunctionCall.name).?.returnType })),
//                                        };
//                                        return typeErrorStruct;
//                                    }
//                                }
//                            }
//                        },
//                    }
//                }
//            },
//            .VarDeclaration => {},
//        }
//    }
//    return null;
//}

pub fn resolveDeclaration(declaration: *AST.Declaration, varMap: *std.StringHashMap([]u8)) SemanticError!void {
    if (!varMap.contains(declaration.name)) {
        const temp = try AST.tempGen.genTemp(varMap.allocator);
        try varMap.put(declaration.name, temp);
        declaration.name = temp;
        return;
    }
    if (varMap.get(declaration.name)) |resolvedVar| {
        declaration.name = resolvedVar;
    }
    return SemanticError.VarNotDeclared;
}

pub fn resolveExpression(expression: *AST.Expression, varMap: *std.StringHashMap([]u8)) SemanticError!void {
    switch (expression.*) {
        .Unary => |unary| {
            try resolveExpression(unary.exp, varMap);
        },
        .Binary => |binary| {
            try resolveExpression(binary.lhs, varMap);
            try resolveExpression(binary.rhs, varMap);
        },
        .Integer => {},
        .Identifier => {
            if (varMap.get(expression.Identifier)) |resolvedVar| {
                expression.Identifier = resolvedVar;
            } else {
                return SemanticError.VarNotDeclared;
            }
        },
        .Assignment => |assignment| {
            try resolveExpression(assignment.lhs, varMap);
            try resolveExpression(assignment.rhs, varMap);
        },
        .Ternary => |ternary| {
            try resolveExpression(ternary.condition, varMap);
            try resolveExpression(ternary.lhs, varMap);
            try resolveExpression(ternary.rhs, varMap);
        },
        .FunctionCall => |fnCall| {
            for (fnCall.args.items) |arg| {
                try resolveExpression(arg, varMap);
            }
        },
    }
}

pub fn resolveStatement(statement: *AST.Statement, varMap: *std.StringHashMap([]u8), allocator: std.mem.Allocator) SemanticError!void {
    switch (statement.*) {
        .Return => |ret| {
            try resolveExpression(ret.expression, varMap);
        },
        .Expression => |expression| {
            try resolveExpression(expression, varMap);
        },
        .Null => {},
        .If => |ifNode| {
            try resolveExpression(ifNode.condition, varMap);
            try resolveStatement(ifNode.thenStmt, varMap, allocator);
            if (ifNode.elseStmt) |elseStmt| {
                try resolveStatement(elseStmt, varMap, allocator);
            }
        },
        .Label => {},
        .Goto => {},
        .Compound => |compound| {
            var compoundMap = std.StringHashMap([]u8).init(allocator);
            var varMapIter = varMap.iterator();
            while (varMapIter.next()) |iterator| {
                try compoundMap.put(iterator.key_ptr.*, iterator.value_ptr.*);
            }
            for (compound.items) |blockItemInCompound| {
                try resolveBlockItem(blockItemInCompound, &compoundMap, allocator);
            }
        },
        .For => |forStmt| {
            if (std.mem.eql(u8, @tagName(forStmt.init.*), "Expression")) {
                try resolveExpression(forStmt.init.Expression, varMap);
            }
            if (forStmt.condition) |condition|
                try resolveExpression(condition, varMap);
            if (forStmt.post) |post|
                try resolveExpression(post, varMap);
            try resolveStatement(forStmt.body, varMap, allocator);
        },
        .DoWhile => |doWhile| {
            try resolveExpression(doWhile.condition, varMap);
            try resolveStatement(doWhile.body, varMap, allocator);
        },
        .While => |whileStmt| {
            try resolveExpression(whileStmt.condition, varMap);
            try resolveStatement(whileStmt.body, varMap, allocator);
        },
        .Break => {},
        .Continue => {},
    }
}

pub fn resolveBlockItem(blockItem: *AST.BlockItem, varMap: *std.StringHashMap([]u8), allocator: std.mem.Allocator) SemanticError!void {
    switch (blockItem.*) {
        .Statement => |statement| {
            try resolveStatement(statement, varMap, allocator);
        },
        .Declaration => |declaration| {
            try resolveDeclaration(
                declaration,
                varMap,
            );
        },
    }
}

pub fn varResolutionPass(allocator: std.mem.Allocator, node: *AST.Program) SemanticError!void {
    // TODO: Support for global variables
    for (node.externalDecls.items) |externalDecl| {
        switch (externalDecl.*) {
            .FunctionDecl => |functionDecl| {
                const varMap = std.StringHashMap([]u8).init(allocator);
                for (functionDecl.blockItems.items) |blockItem| {
                    try resolveBlockItem(blockItem, @constCast(&varMap), allocator);
                }
            },
            .VarDeclaration => {
                unreachable();
            },
        }
    }
}

pub fn resolveLocals(allocator: std.mem.Allocator, program: *AST.Program) SemanticError!void {
    // Collect locals
    const localsSet = std.BufSet.init(allocator);
    for (program.function.blockItems.items) |blockItem| {
        switch (blockItem.*) {
            .Statement => |statement| {
                switch (statement.*) {
                    .Label => |label| {
                        try localsSet.insert(label);
                    },
                }
            },
        }
    }
    // Check gotos
    for (program.function.blockItems.items) |blockItem| {
        switch (blockItem.*) {
            .Statement => |statement| {
                switch (statement.*) {
                    .Goto => |goto| {
                        if (!localsSet.contains(goto)) return SemanticError.UnknownLabel;
                    },
                }
            },
        }
    }

    localsSet.insert();
}

pub fn gotoResolutionPass(allocator: std.mem.Allocator, node: *AST.Program) SemanticError!void {
    _ = allocator;
    _ = node;
    //for(node.function.blockItems.items) |blockItem| {
    //    switch(blockItem.*){
    //      .Statement  => |statement| {
    //          switch(statement.*){
    //          }
    //      },
    //    }
    //  }
}

// Lots of typechecker tests here

test "typechecker-error-fnCall-1" {
    const lexer = @import("./lexer.zig");
    const parser = @import("./parser.zig");
    const ast = @import("./AST.zig");
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr =
        \\ void add() { int a; return a;}
        \\ int main(){
        \\     int c = add();
        \\     return c;
        \\ }
    ;
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const program = try p.parseProgram();
    try ast.scopeVariableResolutionPass(program, allocator);
    const typechecker = try Typechecker.init(allocator);
    const hasTypeError = try typechecker.check(program);
    if (hasTypeError) |typeError| {
        std.log.warn("\x1b[31mError\x1b[0m: {s}\n", .{typeError});
    }
    try ast.loopLabelPass(program, allocator);
}

test "typechecker-error-fn-not found" {
    const lexer = @import("./lexer.zig");
    const parser = @import("./parser.zig");
    const ast = @import("./AST.zig");
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr =
        \\ void add() { int a; return a;}
        \\ int main(){
        \\     int c = addOne();
        \\     return c;
        \\ }
    ;
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const program = try p.parseProgram();
    try ast.scopeVariableResolutionPass(program, allocator);
    const typechecker = try Typechecker.init(allocator);
    const hasTypeError = try typechecker.check(program);
    if (hasTypeError) |typeError| {
        std.log.warn("\x1b[31mError\x1b[0m: {s}\n", .{typeError});
    }
    try ast.loopLabelPass(program, allocator);
}
