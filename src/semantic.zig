// Contains the semantic passes
const std = @import("std");
const AST = @import("./AST.zig");

const SemanticError = error{
    VarNotDeclared,
    OutOfMemory,
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
    }
}

pub fn varResolutionPass(allocator: std.mem.Allocator, node: *AST.Program) SemanticError!void {
    // Takes in a program node traverses the AST and replaces the variables there with temps
    const varMap = std.StringHashMap([]u8).init(allocator);
    for (node.function.blockItems.items) |blockItem| {
        switch (blockItem.*) {
            .Statement => |statement| {
                switch (statement.*) {
                    .Return => |ret| {
                        try resolveExpression(ret.expression, @constCast(&varMap));
                    },
                    .Expression => |expression| {
                        try resolveExpression(expression, @constCast(&varMap));
                    },
                    .Null => {},
                }
            },
            .Declaration => |declaration| {
                try resolveDeclaration(
                    declaration,
                    @constCast(&varMap),
                );
            },
        }
    }
}
