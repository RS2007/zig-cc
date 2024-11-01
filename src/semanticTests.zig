const std = @import("std");
const semantic = @import("./semantic.zig");

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
    const varResolver = try ast.VarResolver.init(allocator);
    try varResolver.resolve(program);
    const typechecker = try semantic.Typechecker.init(allocator);
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
    const varResolver = try ast.VarResolver.init(allocator);
    try varResolver.resolve(program);
    const typechecker = try semantic.Typechecker.init(allocator);
    const hasTypeError = try typechecker.check(program);
    if (hasTypeError) |typeError| {
        std.log.warn("\x1b[31mError\x1b[0m: {s}\n", .{typeError});
    }
    try ast.loopLabelPass(program, allocator);
}

// INFO: Function declaration semantic errors:
test "function if declared as static should not be redeclared as non static" {
    const lexer = @import("./lexer.zig");
    const parser = @import("./parser.zig");
    const ast = @import("./AST.zig");
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr =
        \\ extern int add(int a,int b);
        \\ static int add(int a, int b) {return a+b;}
        \\ int main(){
        \\     int c = add(2,3);
        \\     return c;
        \\ }
    ;
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const program = try p.parseProgram();
    const varResolver = try ast.VarResolver.init(allocator);
    try varResolver.resolve(program);
    const typechecker = try semantic.Typechecker.init(allocator);
    const hasTypeError = try typechecker.check(program);
    var hasErr = false;
    if (hasTypeError) |typeError| {
        std.log.warn("\x1b[31mError\x1b[0m: {s}\n", .{typeError});
        hasErr = true;
    }
    std.debug.assert(hasErr);
}

test "function redefinition" {
    const lexer = @import("./lexer.zig");
    const parser = @import("./parser.zig");
    const ast = @import("./AST.zig");
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr =
        \\ int add(int a,int b){return a+b;}
        \\ int add(int a, int b) {return a+b+3;}
        \\ int main(){
        \\     int c = add(2,3);
        \\     return c;
        \\ }
    ;
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const program = try p.parseProgram();
    const varResolver = try ast.VarResolver.init(allocator);
    try varResolver.resolve(program);
    const typechecker = try semantic.Typechecker.init(allocator);
    const hasTypeError = try typechecker.check(program);
    var hasErr = false;
    if (hasTypeError) |typeError| {
        std.log.warn("\x1b[31mError\x1b[0m: {s}\n", .{typeError});
        hasErr = true;
    }
    std.debug.assert(hasErr);
}

test "global var having same name as func" {
    const lexer = @import("./lexer.zig");
    const parser = @import("./parser.zig");
    const ast = @import("./AST.zig");
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr =
        \\ int add = 5;
        \\ int add(int a, int b) {return a+b+3;}
        \\ int main(){
        \\     int c = add(2,3);
        \\     return c;
        \\ }
    ;
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const program = try p.parseProgram();
    const varResolver = try ast.VarResolver.init(allocator);
    try varResolver.resolve(program);
    const typechecker = try semantic.Typechecker.init(allocator);
    const hasTypeError = try typechecker.check(program);
    var hasErr = false;
    if (hasTypeError) |typeError| {
        std.log.warn("\x1b[31mError\x1b[0m: {s}\n", .{typeError});
        hasErr = true;
    }
    std.debug.assert(hasErr);
}

test "global var declaration and definition having different argument numbers" {
    const lexer = @import("./lexer.zig");
    const parser = @import("./parser.zig");
    const ast = @import("./AST.zig");
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr =
        \\ int add(int a, int b, int c);
        \\ int add(int a, int b);
        \\ int main(){
        \\     int c = add(2,3);
        \\     return c;
        \\ }
    ;
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const program = try p.parseProgram();
    const varResolver = try ast.VarResolver.init(allocator);
    try varResolver.resolve(program);
    const typechecker = try semantic.Typechecker.init(allocator);
    const hasTypeError = try typechecker.check(program);
    var hasErr = false;
    if (hasTypeError) |typeError| {
        std.log.warn("\x1b[31mError\x1b[0m: {s}\n", .{typeError});
        hasErr = true;
    }
    std.debug.assert(hasErr);
}

// INFO: Global declaration semantic errors:
test "global var declaration having non integer expression" {
    const lexer = @import("./lexer.zig");
    const parser = @import("./parser.zig");
    const ast = @import("./AST.zig");
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr =
        \\ int add(int a, int b, int c){return a+b+c;}
        \\ int k = add(2,3,4);
        \\ int main(){
        \\     return k;
        \\ }
    ;
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const program = try p.parseProgram();
    const varResolver = try ast.VarResolver.init(allocator);
    try varResolver.resolve(program);
    const typechecker = try semantic.Typechecker.init(allocator);
    const hasTypeError = try typechecker.check(program);
    var hasErr = false;
    if (hasTypeError) |typeError| {
        std.log.warn("\x1b[31mError\x1b[0m: {s}\n", .{typeError});
        hasErr = true;
    }
    std.debug.assert(hasErr);
}

test "extern shouldnt have an init value" {
    const lexer = @import("./lexer.zig");
    const parser = @import("./parser.zig");
    const ast = @import("./AST.zig");
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr =
        \\ extern int k = 5;
        \\ int main(){
        \\     return k;
        \\ }
    ;
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const program = try p.parseProgram();
    const varResolver = try ast.VarResolver.init(allocator);
    try varResolver.resolve(program);
    const typechecker = try semantic.Typechecker.init(allocator);
    const hasTypeError = try typechecker.check(program);
    var hasErr = false;
    if (hasTypeError) |typeError| {
        std.log.warn("\x1b[31mError\x1b[0m: {s}\n", .{typeError});
        hasErr = true;
    }
    std.debug.assert(hasErr);
}

test "extern declarations after static" {
    const lexer = @import("./lexer.zig");
    const parser = @import("./parser.zig");
    const ast = @import("./AST.zig");
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr =
        \\ static int k = 5;
        \\ extern int k;
        \\ int main(){
        \\     return k;
        \\ }
    ;
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const program = try p.parseProgram();
    const varResolver = try ast.VarResolver.init(allocator);
    try varResolver.resolve(program);
    const typechecker = try semantic.Typechecker.init(allocator);
    const hasTypeError = try typechecker.check(program);
    var hasErr = false;
    if (hasTypeError) |typeError| {
        std.log.warn("\x1b[31mError\x1b[0m: {s}\n", .{typeError});
        hasErr = true;
    }
    std.debug.assert(!hasErr);
}
test "static declarations after extern" {
    const lexer = @import("./lexer.zig");
    const parser = @import("./parser.zig");
    const ast = @import("./AST.zig");
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr =
        \\ extern int k;
        \\ static int k = 5;
        \\ int main(){
        \\     return k;
        \\ }
    ;
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const program = try p.parseProgram();
    const varResolver = try ast.VarResolver.init(allocator);
    try varResolver.resolve(program);
    const typechecker = try semantic.Typechecker.init(allocator);
    const hasTypeError = try typechecker.check(program);
    var hasErr = false;
    if (hasTypeError) |typeError| {
        std.log.warn("\x1b[31mError\x1b[0m: {s}\n", .{typeError});
        hasErr = true;
    }
    std.debug.assert(hasErr);
}
test "just extern" {
    const lexer = @import("./lexer.zig");
    const parser = @import("./parser.zig");
    const ast = @import("./AST.zig");
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr =
        \\ extern int k;
        \\ int main(){
        \\     return k;
        \\ }
    ;
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const program = try p.parseProgram();
    const varResolver = try ast.VarResolver.init(allocator);
    try varResolver.resolve(program);
    const typechecker = try semantic.Typechecker.init(allocator);
    const hasTypeError = try typechecker.check(program);
    const externVarAttrs = typechecker.symbolTable.get("k").?.attributes;
    if (hasTypeError) |typeError| {
        std.log.warn("\x1b[31mError\x1b[0m: {s}\n", .{typeError});
        std.debug.assert(false);
    }
    std.debug.assert(externVarAttrs.StaticAttr.global);
    std.debug.assert(std.mem.eql(u8, @tagName(externVarAttrs.StaticAttr.init), "NoInit"));
}
test "static and global declarations" {
    const lexer = @import("./lexer.zig");
    const parser = @import("./parser.zig");
    const ast = @import("./AST.zig");
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr =
        \\ static int k = 5;
        \\ int k = 7;
        \\ int main(){
        \\     return k;
        \\ }
    ;
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const program = try p.parseProgram();
    const varResolver = try ast.VarResolver.init(allocator);
    try varResolver.resolve(program);
    const typechecker = try semantic.Typechecker.init(allocator);
    const hasTypeError = try typechecker.check(program);
    var hasErr = false;
    if (hasTypeError) |typeError| {
        std.log.warn("\x1b[31mError\x1b[0m: {s}\n", .{typeError});
        hasErr = true;
    }
    std.debug.assert(hasErr);
}

test "extern inheriting init value from older linkage" {
    const lexer = @import("./lexer.zig");
    const parser = @import("./parser.zig");
    const ast = @import("./AST.zig");
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr =
        \\ int k = 4;
        \\ extern int k;
        \\ int main(){
        \\     return k;
        \\ }
    ;
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const program = try p.parseProgram();
    const varResolver = try ast.VarResolver.init(allocator);
    try varResolver.resolve(program);
    const typechecker = try semantic.Typechecker.init(allocator);
    const hasTypeError = try typechecker.check(program);
    const externVarAttrs = typechecker.symbolTable.get("k").?.attributes;
    if (hasTypeError) |typeError| {
        std.log.warn("\x1b[31mError\x1b[0m: {s}\n", .{typeError});
        std.debug.assert(false);
    }
    std.debug.assert(externVarAttrs.StaticAttr.global);
    std.debug.assert(externVarAttrs.StaticAttr.init.Initial == 4);
}

test "multiple global inheriting init value from older linkage" {
    const lexer = @import("./lexer.zig");
    const parser = @import("./parser.zig");
    const ast = @import("./AST.zig");
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr =
        \\ int k = 4;
        \\ int k;
        \\ int main(){
        \\     return k;
        \\ }
    ;
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const program = try p.parseProgram();
    const varResolver = try ast.VarResolver.init(allocator);
    try varResolver.resolve(program);
    const typechecker = try semantic.Typechecker.init(allocator);
    const hasTypeError = try typechecker.check(program);
    const externVarAttrs = typechecker.symbolTable.get("k").?.attributes;
    if (hasTypeError) |typeError| {
        std.log.warn("\x1b[31mError\x1b[0m: {s}\n", .{typeError});
        std.debug.assert(false);
    }
    std.debug.assert(externVarAttrs.StaticAttr.global);
    std.debug.assert(externVarAttrs.StaticAttr.init.Initial == 4);
}

test "tentative init values" {
    const lexer = @import("./lexer.zig");
    const parser = @import("./parser.zig");
    const ast = @import("./AST.zig");
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr =
        \\ int k;
        \\ int main(){
        \\     return k;
        \\ }
    ;
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const program = try p.parseProgram();
    const varResolver = try ast.VarResolver.init(allocator);
    try varResolver.resolve(program);
    const typechecker = try semantic.Typechecker.init(allocator);
    const hasTypeError = try typechecker.check(program);
    const externVarAttrs = typechecker.symbolTable.get("k").?.attributes;
    if (hasTypeError) |typeError| {
        std.log.warn("\x1b[31mError\x1b[0m: {s}\n", .{typeError});
        std.debug.assert(false);
    }
    std.debug.assert(externVarAttrs.StaticAttr.global);
    std.debug.assert(std.mem.eql(u8, @tagName(externVarAttrs.StaticAttr.init), "Tentative"));
}