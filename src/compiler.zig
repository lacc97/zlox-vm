const std = @import("std");

const print = std.debug.print;

const Chunk = @import("chunk.zig").Chunk;

const Scanner = @import("scanner.zig").Scanner;

pub const CompileError = error{};

pub fn compile(source: []const u8) CompileError!Chunk {
    var scanner = Scanner.init(source);

    var line: usize = 0;
    while (scanner.scanToken()) |token| {
        if (token.line != line) {
            print("{d:>4} ", .{token.line});
            line = token.line;
        } else {
            print("   | ", .{});
        }
        print("{s:>16} '{s}'\n", .{ @tagName(token.tt), token.lexeme });
    }

    unreachable;
}
