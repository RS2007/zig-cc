const std = @import("std");
const lexer = @import("./lexer.zig");
const parser = @import("./parser.zig");
const ast = @import("./AST.zig");
const semantic = @import("./semantic.zig");

test "test multiple functions" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr =
        \\ int add(int x, int y) { return x+y;}
        \\ int main(){
        \\     return add(2,3);
        \\ }
    ;
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const program = try p.parseProgram();
    const varResolver = try ast.VarResolver.init(allocator);
    try varResolver.resolve(program);
    const typechecker = try semantic.Typechecker.init(allocator);
    const hasTypeErr = try typechecker.check(program);
    if (hasTypeErr) |typeErr| {
        std.log.warn("Type error: {s}\n", .{typeErr});
        std.debug.assert(false);
    }
    const tacRenderer = try ast.TACRenderer.init(allocator, typechecker.symbolTable);
    _ = try tacRenderer.render(program);
}
test "test while and do while" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr =
        \\ int main(){
        \\     int x = 0;
        \\     while(x < 5) x = x + 1;
        \\     do x = x + 1; while(x < 10);
        \\     return x;
        \\ }
    ;
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const program = try p.parseProgram();
    const varResolver = try ast.VarResolver.init(allocator);
    try varResolver.resolve(program);
    const typechecker = try semantic.Typechecker.init(allocator);
    const hasTypeErr = try typechecker.check(program);
    if (hasTypeErr) |typeErr| {
        std.log.warn("Type error: {s}\n", .{typeErr});
        std.debug.assert(false);
    }
    const tacRenderer = try ast.TACRenderer.init(allocator, typechecker.symbolTable);
    _ = try tacRenderer.render(program);
    //for (instructions.items, 0..) |inst, i| {
    //    std.log.warn("Inst at {}: {any}\n", .{ i, inst });
    //}
}

test "test tac generation for ternary" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr = "int main(){int y = 4; int x = 3;return x == 3?x:y}";
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const program = try p.parseProgram();
    const varResolver = try ast.VarResolver.init(allocator);
    try varResolver.resolve(program);
    const typechecker = try semantic.Typechecker.init(allocator);
    const hasTypeErr = try typechecker.check(program);
    if (hasTypeErr) |typeErr| {
        std.log.warn("Type error: {any}\n", .{typeErr});
        std.debug.assert(false);
    }
    const tacRenderer = try ast.TACRenderer.init(allocator, typechecker.symbolTable);
    _ = try tacRenderer.render(program);
}

// TODO: shaky tests, rewrite later

//test "codegen TAC with declarations" {
//    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
//    const allocator = arena.allocator();
//    defer arena.deinit();
//    const programStr = "int main(){ int x = 2; int y = 3 || 4; return x && y; }";
//    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
//    var p = try parser.Parser.init(allocator, l);
//    var program = try p.parseProgram();
//    const varResolver = try ast.VarResolver.init(allocator);
//    try varResolver.resolve(program);
//    const typechecker = try semantic.Typechecker.init(allocator);
//    const hasTypeErr = try typechecker.check(program);
//    if (hasTypeErr) |typeErr| {
//        std.log.warn("Type error: {any}\n", .{typeErr});
//        std.debug.assert(false);
//    }
//    const maybeInstructions = for ((try program.genTAC(allocator)).function.items) |function| {
//        if (std.mem.eql(u8, function.name, "main")) break function.instructions;
//    } else null;
//    const instructions = maybeInstructions.?;
//    std.log.warn("\n \x1b[34m{any}\x1b[0m", .{instructions.items[0]});
//    std.log.warn("\n \x1b[34m{any}\x1b[0m", .{instructions.items[1]});
//}
//
//test "codegen TAC with logical and relational ops" {
//    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
//    const allocator = arena.allocator();
//    defer arena.deinit();
//    const programStr = "int main(){ return 2 && ( 3 || 4 ) ; }";
//    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
//    var p = try parser.Parser.init(allocator, l);
//    var program = try p.parseProgram();
//    const varResolver = try ast.VarResolver.init(allocator);
//    try varResolver.resolve(program);
//    const typechecker = try semantic.Typechecker.init(allocator);
//    const hasTypeErr = try typechecker.check(program);
//    if (hasTypeErr) |typeErr| {
//        std.log.warn("Type error: {any}\n", .{typeErr});
//        std.debug.assert(false);
//    }
//    const maybeInstructions = for ((try program.genTAC(allocator)).function.items) |function| {
//        if (std.mem.eql(u8, function.name, "main")) break function.instructions;
//    } else null;
//    const instructions = maybeInstructions.?;
//    std.log.warn("\n \x1b[34m{any}\x1b[0m", .{instructions.items[0]});
//    std.log.warn("\n \x1b[34m{any}\x1b[0m", .{instructions.items[1]});
//}
//
//test "codegen TAC" {
//    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
//    const allocator = arena.allocator();
//    defer arena.deinit();
//    const programStr = "int main(){ return ~(-2); }";
//    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
//    var p = try parser.Parser.init(allocator, l);
//    var program = try p.parseProgram();
//    // INFO: repeat this in the other places
//    const varResolver = try ast.VarResolver.init(allocator);
//    try varResolver.resolve(program);
//    const typechecker = try semantic.Typechecker.init(allocator);
//    const hasTypeErr = try typechecker.check(program);
//    if (hasTypeErr) |typeErr| {
//        std.log.warn("Type error: {any}\n", .{typeErr});
//        std.debug.assert(false);
//    }
//    const maybeInstructions = for ((try program.genTAC(typechecker.symbolTable, allocator)).function.items) |function| {
//        if (std.mem.eql(u8, function.name, "main")) break function.instructions;
//    } else null;
//    const instructions = maybeInstructions.?;
//    std.log.warn("\n \x1b[34m{any}\x1b[0m", .{instructions.items[0]});
//    std.log.warn("\n \x1b[34m{any}\x1b[0m", .{instructions.items[1]});
//    std.log.warn("\n \x1b[34m{s}\x1b[0m", .{instructions.items[0].Unary.dest.Variable});
//    std.log.warn("\n \x1b[34m{s}\x1b[0m", .{instructions.items[1].Unary.dest.Variable});
//    std.log.warn("\n \x1b[34m{any}\x1b[0m", .{instructions.items[2]});
//}
