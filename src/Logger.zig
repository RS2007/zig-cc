const std = @import("std");

pub const Logger = struct {
    file: std.fs.File,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) *Logger {
        const file = std.fs.cwd().createFile(
            "lastRun.log",
            .{ .read = true, .truncate = true },
        ) catch {
            std.process.exit(1);
        };
        const logger = allocator.create(Logger) catch {
            std.process.exit(1);
        };
        logger.* = Logger{ .file = file, .allocator = std.heap.page_allocator };
        return logger;
    }

    pub fn deinit(self: *Logger) void {
        self.file.close();
    }

    fn log(self: *Logger, comptime level: []const u8, comptime fmt: []const u8, args: anytype) !void {
        //const stderr = std.io.getStdErr().writer();
        const writer = self.file.writer();

        // Format the log message
        const message = try std.fmt.allocPrint(self.allocator, "[{s}]: " ++ fmt ++ "\n", .{level} ++ args);
        defer self.allocator.free(message);

        // Write to file
        try writer.writeAll(message);

        // Also print to stderr
        //try stderr.writeAll(message);
    }

    pub inline fn info(self: *Logger, comptime fmt: []const u8, args: anytype) !void {
        try self.log("INFO", fmt, args);
    }

    pub inline fn warn(self: *Logger, comptime fmt: []const u8, args: anytype) !void {
        try self.log("WARN", fmt, args);
    }

    pub inline fn err(self: *Logger, comptime fmt: []const u8, args: anytype) !void {
        try self.log("ERROR", fmt, args);
    }
};

test "basic logging" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    var MyLogger = Logger.init(allocator);
    try MyLogger.info("Sup bois: {d}\n", .{69});
}
