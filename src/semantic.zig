// Contains the semantic passes
const std = @import("std");
const AST = @import("./AST.zig");

const SemanticError = error{
    VarNotDeclared,
    OutOfMemory,
    UnknownLabel,
};

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
    }
}

pub fn resolveStatement(statement: *AST.Statement, varMap: *std.StringHashMap([]u8)) SemanticError!void {
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
            try resolveStatement(ifNode.thenStmt, varMap);
            if (ifNode.elseStmt) |elseStmt| {
                try resolveStatement(elseStmt, varMap);
            }
        },
        .Label => {},
        .Goto => {},
        .Compound => |compound| {
            for (compound.items) |blockItemInCompound| {
                try resolveBlockItem(blockItemInCompound, varMap);
            }
        },
    }
}

pub fn resolveBlockItem(blockItem: *AST.BlockItem, varMap: *std.StringHashMap([]u8)) SemanticError!void {
    switch (blockItem.*) {
        .Statement => |statement| {
            try resolveStatement(statement, varMap);
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
    // Takes in a program node traverses the AST and replaces the variables there with temps
    const varMap = std.StringHashMap([]u8).init(allocator);
    for (node.function.blockItems.items) |blockItem| {
        try resolveBlockItem(blockItem, @constCast(&varMap));
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
