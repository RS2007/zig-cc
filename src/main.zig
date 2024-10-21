const std = @import("std");
const lexer = @import("./lexer.zig");
const ast = @import("./AST.zig");
const parser = @import("./parser.zig");
const assembly = @import("./Assembly.zig");
const semantic = @import("./semantic.zig");

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
    const shouldDumpTac = if (args.len >= 3) args[2] else null;

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

    try ast.scopeVariableResolutionPass(program, allocator); // Resolve scope by a variable renaming pass
    const hasTypeError = try semantic.typechecker(program, allocator); // Check if there are type issues
    if (hasTypeError) |typeError| {
        std.log.warn("\x1b[33mError\x1b[0m: {s}\n", .{typeError.errorPayload});
        std.os.linux.exit(-1);
    }
    try ast.loopLabelPass(program, allocator); // Labelling loops for break and continue
    const tacProgram = try program.genTAC(allocator);

    if (shouldDumpTac != null and std.mem.eql(u8, shouldDumpTac.?, "tacDump")) {
        _ = try std.fs.cwd().createFile("tacDump", .{});
        const tacDump = try std.fs.cwd().openFile("tacDump", std.fs.File.OpenFlags{ .mode = .read_write });
        defer tacDump.close();
        const tacDumpWriter = tacDump.writer();
        for (tacProgram.function.items) |tacFn| {
            try tacDumpWriter.writeAll(tacFn.name);
            try tacDumpWriter.writeAll("\n\n");
            for (tacFn.instructions.items) |tacFnInst| {
                try tacDumpWriter.writeAll(try std.fmt.allocPrint(allocator, "{any}\n", .{tacFnInst}));
            }
        }
    }

    const asmProgram = try tacProgram.codegen(allocator);
    try asmProgram.stringify(std.io.getStdOut().writer(), allocator);
}
