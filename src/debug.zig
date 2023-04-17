const std = @import("std");

const OpCode = @import("chunk.zig").OpCode;
const Chunk = @import("chunk.zig").Chunk;

const Value = @import("value.zig").Value;

pub const trace_execution = @import("build_options").debug_trace_execution;

pub fn disassemble(chunk: *Chunk, name: []const u8) void {
    std.debug.print("== {s} == \n", .{name});

    var off: usize = 0;
    while (off < chunk.len()) {
        off = disassembleInstruction(chunk, off);
    }
}

pub fn disassembleInstruction(chunk: *const Chunk, off: usize) usize {
    std.debug.print("{x:0>4} ", .{off});

    const constants = chunk.constants.items;
    const bytes = chunk.code.items(.byte);
    const lines = chunk.code.items(.line);

    if (off > 0 and lines[off] == lines[off - 1]) {
        std.debug.print("   | ", .{});
    } else {
        std.debug.print("{d:4} ", .{lines[off]});
    }

    const instruction = chunk.code.items(.byte)[off];
    switch (@intToEnum(OpCode, instruction)) {
        .OP_CONSTANT, .OP_CONSTANT_LONG => |constant_op| return constantInstruction(constants, bytes, constant_op, off),
        .OP_ADD, .OP_SUBTRACT, .OP_MULTIPLY, .OP_DIVIDE, .OP_NEGATE, .OP_RETURN => |simple_op| return simpleInstruction(constants, bytes, simple_op, off),
        _ => {
            std.debug.print("unknown opcode {d}\n", .{instruction});
            return off + 1;
        },
    }
}

pub fn printValue(value: Value) void {
    std.debug.print("{d}", .{value});
}

fn printOpName(op: OpCode) void {
    std.debug.print("{s:->20}", .{@tagName(op)});
}

fn constantInstruction(constants: []const Value, bytes: []const u8, op: OpCode, off: usize) usize {
    var next_off = off + 1;

    const const_idx = idx_calc: {
        switch (op) {
            .OP_CONSTANT => {
                next_off += 1;
                break :idx_calc @as(usize, bytes[next_off - 1]);
            },
            .OP_CONSTANT_LONG => {
                const idx_bytes = bytes[next_off..];
                next_off += 4;
                break :idx_calc @as(usize, std.mem.bytesToValue(u32, idx_bytes[0..4]));
            },
            else => unreachable,
        }
    };
    printOpName(op);
    std.debug.print(" {d:8} '", .{const_idx});
    printValue(constants[const_idx]);
    std.debug.print("'\n", .{});
    return next_off;
}

fn simpleInstruction(_: []const Value, _: []const u8, op: OpCode, off: usize) usize {
    printOpName(op);
    std.debug.print("\n", .{});
    return off + 1;
}