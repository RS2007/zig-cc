const std = @import("std");
const lexer = @import("./lexer.zig");
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

    // Open the file for reading
    const file = try std.fs.cwd().openFile(filename, .{});
    defer file.close();

    // Read the entire file into a buffer
    const file_size = try file.getEndPos();
    const buffer = try allocator.alloc(u8, file_size);
    defer allocator.free(buffer);

    _ = try file.readAll(buffer);
    const l = try lexer.Lexer.init(allocator, buffer);
    var p = try parser.Parser.init(allocator, l);
    var program = try p.parseProgram();
    const maybeInstructions = for ((try program.genTAC(allocator)).function.items) |function| {
        if (std.mem.eql(u8, function.name, "main")) break function.instructions;
    } else null;
    const instructions = maybeInstructions.?;
    var asmInstructions = std.ArrayList(*assembly.Instruction).init(allocator);
    for (instructions.items) |inst| {
        try inst.codegen(&asmInstructions, allocator);
    }
    for (asmInstructions.items) |asmInst| {
        std.log.warn("\n \x1b[34m{any}\x1b[0m", .{asmInst});
    }
    try assembly.replacePseudoRegs(&asmInstructions, allocator);
    const fixedAsmInstructions = assembly.fixupInstructions(&asmInstructions, allocator);
    std.log.warn("POST PSEUDO REPLACEMENT AND STACK TO STACK MOVES", .{});
    // for (asmInstructions.items) |asmInst| {
    //     std.log.warn("\n \x1b[34m{any}\x1b[0m", .{asmInst});
    // }
    var mem: [2048]u8 = std.mem.zeroes([2048]u8);
    var buf = @as([]u8, &mem);
    const header = try std.fmt.bufPrint(buf, ".globl main\nmain:\npush %rbp", .{});
    buf = buf[header.len..];
    for (fixedAsmInstructions.items) |asmInst| {
        const printedSlice = try std.fmt.bufPrint(buf, "\n{s}", .{try asmInst.stringify(allocator)});
        buf = buf[printedSlice.len..];
    }

    const temp_filename = "temp.s";
    var writeFile = try std.fs.cwd().createFile(temp_filename, .{});
    defer writeFile.close();

    try writeFile.writeAll(@as([]u8, &mem));

    // Step 2: Run `gcc -o test temp.s`
    //var gcc_args = [_][]const u8{ "gcc", "-o", "test", temp_filename };
    //var gcc_exec = std.process.Exec.make(allocator, gcc_args) catch |err| {
    //    std.debug.print("Failed to start gcc: {}\n", .{err});
    //    return err;
    //};

    std.log.warn("\n\x1b[33m{s}\x1b[0m", .{buf});
}
