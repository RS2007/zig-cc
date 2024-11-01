const std = @import("std");
const ast = @import("./AST.zig");
const lexer = @import("./lexer.zig");
const parser = @import("./parser.zig");
const semantic = @import("./semantic.zig");

// TODO: This test fails check it later
test "static storage codegenaration" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr =
        \\ int recurse(int n){
        \\         static int accum = 0;
        \\         static int k = 0;
        \\         if(k < n){
        \\                 k = k +1;
        \\                 accum = accum + k;
        \\                 recurse(n);
        \\         }
        \\         return accum;
        \\ }
        \\ 
        \\ int main(){
        \\     return recurse(10);
        \\ }
    ;
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    var program = try p.parseProgram();
    const varResolver = try ast.VarResolver.init(allocator);
    try varResolver.resolve(program);
    const typechecker = try semantic.Typechecker.init(allocator);
    const hasTypeErr = try typechecker.check(program);
    if (hasTypeErr) |typeError| {
        std.log.warn("\x1b[33mError\x1b[0m: {s}\n", .{typeError});
        std.debug.assert(false);
    }
    try ast.loopLabelPass(program, allocator);
    const asmProgram = try (try program.genTAC(typechecker.symbolTable, allocator)).codegen(allocator);
    try asmProgram.stringify(std.io.getStdErr().writer(), allocator);
}
