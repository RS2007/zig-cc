// Contains the semantic passes
const std = @import("std");
const AST = @import("./AST.zig");

const SemanticError = error{
    VarNotDeclared,
    OutOfMemory,
    UnknownLabel,
    NoSpaceLeft,
};

const TypeError = error{ TypeMismatch, OutOfMemory };

pub const TypeErrorStruct = struct {
    errorType: ?TypeError,
    errorPayload: []u8,
};

pub const FnMetaData = struct {
    returnType: AST.Type,
    localVarTypeMap: *std.StringHashMap(AST.Type),
};

pub fn typechecker(program: *AST.Program, allocator: std.mem.Allocator) !?*TypeErrorStruct {
    // INFO: Only typechecking requirement right now is function call return
    // INFO: Maintain a function metadata hashmap and query that to check if the return type match
    // Buildling a metadata map for functions.
    var fnMetaDataMap = std.StringHashMap(FnMetaData).init(allocator);
    for (program.externalDecls.items) |externalDecl| {
        switch (externalDecl.*) {
            .FunctionDecl => |fnDecl| {
                const localVarTypeMap = std.StringHashMap(AST.Type).init(allocator);
                try fnMetaDataMap.put(
                    fnDecl.name,
                    .{
                        .returnType = fnDecl.returnType,
                        .localVarTypeMap = @constCast(&localVarTypeMap),
                    },
                );
            },
            .VarDeclaration => {},
        }
    }
    for (program.externalDecls.items) |externalDecl| {
        switch (externalDecl.*) {
            .FunctionDecl => |fnDecl| {
                for (fnDecl.blockItems.items) |blkItem| {
                    switch (blkItem.*) {
                        .Declaration => |decl| {
                            try fnMetaDataMap.get(fnDecl.name).?.localVarTypeMap.put(
                                decl.name,
                                decl.type,
                            );
                        },
                        .Statement => {},
                    }
                }
            },
            .VarDeclaration => {},
        }
    }
    var iter = fnMetaDataMap.iterator();
    while (iter.next()) |entry| {
        std.log.warn("{s} : \n", .{entry.key_ptr.*});
        var varIter = entry.value_ptr.localVarTypeMap.iterator();
        while (varIter.next()) |varEntry| {
            std.log.warn("{s}: {any}\n", .{ varEntry.key_ptr.*, varEntry.value_ptr.* });
        }
    }
    for (program.externalDecls.items) |externalDecl| {
        switch (externalDecl.*) {
            .FunctionDecl => |fnDecl| {
                for (fnDecl.blockItems.items) |blkItem| {
                    switch (blkItem.*) {
                        .Statement => |stmt| {
                            // Check assignment statements
                            if (std.mem.eql(u8, @tagName(stmt.*), "Expression")) {
                                if (std.mem.eql(u8, @tagName(stmt.Expression.*), "Assignment")) {
                                    if (std.mem.eql(u8, @tagName(stmt.Expression.Assignment.rhs.*), "FunctionCall")) {
                                        std.debug.assert(std.mem.eql(u8, @tagName(stmt.Expression.Assignment.lhs.*), "Identifier"));
                                        const lhsType = fnMetaDataMap.get(fnDecl.name).?.localVarTypeMap.get(stmt.Expression.Assignment.lhs.Identifier).?;
                                        const rhsType = fnMetaDataMap.get(stmt.Expression.Assignment.rhs.FunctionCall.name).?.returnType;
                                        if (lhsType != rhsType) {
                                            const typeErrorStruct = try allocator.create(TypeErrorStruct);
                                            typeErrorStruct.* = .{
                                                .errorType = TypeError.TypeMismatch,
                                                .errorPayload = (try std.fmt.allocPrint(allocator, "Type mismatch: lhs type: {any} and rhs type: {any}\n", .{ lhsType, rhsType })),
                                            };
                                            return typeErrorStruct;
                                        }
                                    }
                                }
                            }
                            // Check return statements in the function and check the type
                            if (std.mem.eql(u8, @tagName(stmt.*), "Return")) {
                                if (std.mem.eql(u8, @tagName(stmt.Return.expression.*), "Identifier")) {
                                    const identifierType = fnMetaDataMap.get(fnDecl.name).?.localVarTypeMap.get(stmt.Return.expression.Identifier).?;
                                    if (identifierType != fnDecl.returnType) {
                                        const typeErrorStruct = try allocator.create(TypeErrorStruct);
                                        typeErrorStruct.* = .{
                                            .errorType = TypeError.TypeMismatch,
                                            .errorPayload = (try std.fmt.allocPrint(allocator, "Type mismatch: function supposed to return: {any} and found type in return: {any}\n", .{ fnDecl.returnType, identifierType })),
                                        };
                                        return typeErrorStruct;
                                    }
                                }
                            }
                        },
                        .Declaration => |decl| {
                            // Check declaration assignments
                            if (decl.expression) |expr| {
                                if (std.mem.eql(u8, @tagName(expr.*), "FunctionCall")) {
                                    if (decl.type != fnMetaDataMap.get(expr.FunctionCall.name).?.returnType) {
                                        const typeErrorStruct = try allocator.create(TypeErrorStruct);
                                        typeErrorStruct.* = .{
                                            .errorType = TypeError.TypeMismatch,
                                            .errorPayload = (try std.fmt.allocPrint(allocator, "Type mismatch: lhs type: {any} and rhs type: {any}\n", .{ decl.type, fnMetaDataMap.get(expr.FunctionCall.name).?.returnType })),
                                        };
                                        return typeErrorStruct;
                                    }
                                }
                            }
                        },
                    }
                }
            },
            .VarDeclaration => {},
        }
    }
    return null;
}

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

test "typechecker" {
    const lexer = @import("./lexer.zig");
    const parser = @import("./parser.zig");
    const ast = @import("./AST.zig");
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr =
        \\ void add() { int a; return a;}
        \\ int main(){
        \\     int c = 3;
        \\     return c;
        \\ }
    ;
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const program = try p.parseProgram();
    try ast.scopeVariableResolutionPass(program, allocator);
    try ast.loopLabelPass(program, allocator);
    const hasTypeError = try typechecker(program, allocator);
    if (hasTypeError) |typeError| {
        std.log.warn("{s}\n", .{typeError.errorPayload});
    } else {
        std.debug.assert(false);
    }
}
