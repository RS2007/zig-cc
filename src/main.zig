const std = @import("std");
const lexer = @import("./lexer.zig");
const ast = @import("./AST.zig");
const parser = @import("./parser.zig");
const assembly = @import("./Assembly.zig");

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();

    // Access argv[1]
    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len <= 1) {
        std.debug.print("Usage: {s} <filename>\n", .{args[0]});
        return;
    }

    const filename = args[1];

    const file = try std.fs.cwd().openFile(filename, std.fs.File.OpenFlags{ .mode = .read_only });
    defer file.close();

    // Read the entire file into a buffer
    const file_size = try file.getEndPos();
    const buffer = try allocator.alloc(u8, file_size);
    defer allocator.free(buffer);

    _ = try file.readAll(buffer);
    const l = try lexer.Lexer.init(allocator, buffer);
    var p = try parser.Parser.init(allocator, l);
    var program = try p.parseProgram();
    try ast.scopeVariableResolutionPass(program, allocator);
    try ast.loopLabelPass(program, allocator);
    const asmProgram = try (try program.genTAC(allocator)).codegen(allocator);
    try asmProgram.stringify(std.io.getStdOut().writer(), allocator);
}
