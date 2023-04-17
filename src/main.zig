const std = @import("std");

const OpCode = @import("chunk.zig").OpCode;
const Chunk = @import("chunk.zig").Chunk;

const debug = @import("debug.zig");

const VM = @import("vm.zig").VM;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    const allocator = gpa.allocator();

    var vm = try VM.init(allocator);
    defer vm.deinit();

    var chunk = Chunk.init(allocator);
    defer chunk.deinit();

    _ = try chunk.writeConstant(1.2, 123);
    _ = try chunk.writeConstant(3.4, 123);

    try chunk.writeOp(.OP_ADD, 123);

    _ = try chunk.writeConstant(5.6, 123);

    try chunk.writeOp(.OP_DIVIDE, 123);
    try chunk.writeOp(.OP_NEGATE, 123);

    try chunk.writeOp(.OP_RETURN, 123);

    try vm.interpret(&chunk);
}
