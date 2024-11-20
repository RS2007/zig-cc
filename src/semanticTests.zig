const lexer = @import("./lexer.zig");
const parser = @import("./parser.zig");
const ast = @import("./AST.zig");
const std = @import("std");
const semantic = @import("./semantic.zig");

test "testing variable rename pass" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr = "{ int k = 4; return k+5;}";
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const stmt = try p.parseStatement();
    const varResolver = try ast.VarResolver.init(allocator);
    try ast.statementScopeVariableResolve(varResolver, stmt, 0);
}

test "testing variable rename pass error" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr =
        \\ int k =4; 
        \\ int main(){
        \\     int k;
        \\     return k;
        \\ }
    ;
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const program = try p.parseProgram();
    const varResolver = try ast.VarResolver.init(allocator);
    varResolver.resolve(program) catch {
        std.debug.assert(false);
    };
}

test "testing with nested scopes and with an error" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr =
        \\ int k = 5;
        \\ int main(){{ extern int k; int k;
        \\     }
        \\     return k;
        \\ }
    ;
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const program = try p.parseProgram();
    const varResolver = try ast.VarResolver.init(allocator);
    var hasErr = false;
    varResolver.resolve(program) catch |err| {
        hasErr = true;
        switch (err) {
            error.ConflicingVarDeclaration => {
                std.log.warn("\x1b[31mConflicting var declaration\x1b[0m\n", .{});
            },
            else => {
                std.log.warn("\x1b[31mUnknown error\x1b[0m\n", .{});
            },
        }
    };
    std.debug.assert(hasErr);
}

test "Declarations" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr = "int main(){ int x = 2; int y = -x; return ~y; }";
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const program = try p.parseProgram();
    //const stdout = std.io.getStdOut().writer();
    //try prettyPrintAST(Node{ .Program = program }, stdout, 0);
    std.log.warn("{any}", .{program.externalDecls.items[0].FunctionDecl.blockItems.items[2].Statement.Return.expression.Unary.exp});
}

test "typechecker-error-fnCall-1" {
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
    std.debug.assert(externVarAttrs.StaticAttr.init.Initial.value.Integer == 4);
}

test "multiple global inheriting init value from older linkage" {
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
    std.debug.assert(externVarAttrs.StaticAttr.init.Initial.value.Integer == 4);
}

test "tentative init values" {
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

test "typed ast check" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr =
        \\ int main(){
        \\     int a;
        \\     a = 3 + 2;
        \\     return 0;
        \\ }
    ;
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const program = try p.parseProgram();
    const varResolver = try ast.VarResolver.init(allocator);
    try varResolver.resolve(program);
    const typechecker = try semantic.Typechecker.init(allocator);
    const hasTypeErr = try typechecker.check(program);
    var hasErr = false;
    if (hasTypeErr) |typeErr| {
        std.log.warn("Type error: {s}\n", .{typeErr});
        hasErr = true;
    }
    _ = try std.testing.expect(!hasErr);
    std.log.warn("type of expr: {any}", .{program.externalDecls.items[0].FunctionDecl.blockItems.items[1].Statement.Expression.Assignment.type});
}

test "conversion logic" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr =
        \\ int main(){
        \\     int a = 1L;
        \\     return 0;
        \\ }
    ;
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const program = try p.parseProgram();
    const varResolver = try ast.VarResolver.init(allocator);
    try varResolver.resolve(program);
    const typechecker = try semantic.Typechecker.init(allocator);
    const hasTypeErr = try typechecker.check(program);
    var hasErr = false;
    if (hasTypeErr) |typeErr| {
        std.log.warn("Type error: {s}\n", .{typeErr});
        hasErr = true;
    }
    _ = try std.testing.expect(!hasErr);
    // Checking if casting has happened to the rhs of declaration
    _ = try std.testing.expectEqual(program.externalDecls.items[0].FunctionDecl.blockItems.items[0].Declaration.expression.?.getType(), ast.Type.Integer);
}

test "global variable with conflicting types" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr =
        \\ int k = 4;
        \\ long k;
        \\ int main(){
        \\     int a = 1L;
        \\     return 0;
        \\ }
    ;
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const program = try p.parseProgram();
    const varResolver = try ast.VarResolver.init(allocator);
    try varResolver.resolve(program);
    const typechecker = try semantic.Typechecker.init(allocator);
    const hasTypeErr = try typechecker.check(program);
    var hasErr = false;
    if (hasTypeErr) |typeErr| {
        std.log.warn("Type error: {s}\n", .{typeErr});
        hasErr = true;
    }
    _ = try std.testing.expect(hasErr);
}

test "extern conflict int and longs" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr =
        \\ long k = 4;
        \\ int main(){
        \\     extern int k;
        \\     return 0;
        \\ }
    ;
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const program = try p.parseProgram();
    const varResolver = try ast.VarResolver.init(allocator);
    try varResolver.resolve(program);
    const typechecker = try semantic.Typechecker.init(allocator);
    const hasTypeErr = try typechecker.check(program);
    var hasErr = false;
    if (hasTypeErr) |typeErr| {
        std.log.warn("Type error: {s}\n", .{typeErr});
        hasErr = true;
    }
    _ = try std.testing.expect(hasErr);
}

test "function definition with different types" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr =
        \\ long add(int a, int b);
        \\ long add(long a, long b);
        \\ int main(){
        \\     extern int k;
        \\     return 0;
        \\ }
    ;
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const program = try p.parseProgram();
    const varResolver = try ast.VarResolver.init(allocator);
    try varResolver.resolve(program);
    const typechecker = try semantic.Typechecker.init(allocator);
    const hasTypeErr = try typechecker.check(program);
    var hasErr = false;
    if (hasTypeErr) |typeErr| {
        std.log.warn("Type error: {s}\n", .{typeErr});
        hasErr = true;
    }
    _ = try std.testing.expect(hasErr);
}

test "unsigned type conflicts" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr =
        \\ unsigned int add(unsigned int a, unsigned int b);
        \\ int add(int a, int b);
        \\ int main(){
        \\     return 0;
        \\ }
    ;
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const program = try p.parseProgram();
    const varResolver = try ast.VarResolver.init(allocator);
    try varResolver.resolve(program);
    const typechecker = try semantic.Typechecker.init(allocator);
    const hasTypeErr = try typechecker.check(program);
    var hasErr = false;
    if (hasTypeErr) |typeErr| {
        std.log.warn("Type error: {s}\n", .{typeErr});
        hasErr = true;
    }
    _ = try std.testing.expect(hasErr);
}

test "function definition with different types(float)" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr =
        \\ double add(double a, double b);
        \\ long add(double a, double b);
        \\ int main(){
        \\     extern int k;
        \\     return 0;
        \\ }
    ;
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const program = try p.parseProgram();
    const varResolver = try ast.VarResolver.init(allocator);
    try varResolver.resolve(program);
    const typechecker = try semantic.Typechecker.init(allocator);
    const hasTypeErr = try typechecker.check(program);
    var hasErr = false;
    if (hasTypeErr) |typeErr| {
        std.log.warn("Type error: {s}\n", .{typeErr});
        hasErr = true;
    }
    _ = try std.testing.expect(hasErr);
}

test "extern global type conflict" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr =
        \\ double k;
        \\ int main(){
        \\     extern int k;
        \\     return 0;
        \\ }
    ;
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const program = try p.parseProgram();
    const varResolver = try ast.VarResolver.init(allocator);
    try varResolver.resolve(program);
    const typechecker = try semantic.Typechecker.init(allocator);
    const hasTypeErr = try typechecker.check(program);
    var hasErr = false;
    if (hasTypeErr) |typeErr| {
        std.log.warn("Type error: {s}\n", .{typeErr});
        hasErr = true;
    }
    _ = try std.testing.expect(hasErr);
}

test "invalid operation on float(complement)" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr =
        \\ double k;
        \\ int main(){
        \\     k = 3.0;
        \\     return ~k;
        \\ }
    ;
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const program = try p.parseProgram();
    const varResolver = try ast.VarResolver.init(allocator);
    try varResolver.resolve(program);
    const typechecker = try semantic.Typechecker.init(allocator);
    const hasTypeErr = try typechecker.check(program);
    var hasErr = false;
    if (hasTypeErr) |typeErr| {
        std.log.warn("Type error: {s}\n", .{typeErr});
        hasErr = true;
    }
    _ = try std.testing.expect(hasErr);
}

test "invalid operation on float(remainder)" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr =
        \\ int main(){
        \\     double k = 3.0;
        \\     double l = 2.0;
        \\     return k % l;
        \\ }
    ;
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const program = try p.parseProgram();
    const varResolver = try ast.VarResolver.init(allocator);
    try varResolver.resolve(program);
    const typechecker = try semantic.Typechecker.init(allocator);
    const hasTypeErr = try typechecker.check(program);
    var hasErr = false;
    if (hasTypeErr) |typeErr| {
        std.log.warn("Type error: {s}\n", .{typeErr});
        hasErr = true;
    }
    _ = try std.testing.expect(hasErr);
}
