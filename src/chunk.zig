const std = @import("std");

const Value = @import("value.zig").Value;
const ValueArray = @import("value.zig").ValueArray;

pub const OpCode = enum(u8) {
    CONSTANT,
    CONSTANT_LONG,
    ADD,
    SUBTRACT,
    MULTIPLY,
    DIVIDE,
    NEGATE,
    RETURN,
    _,
};

pub const Instruction = struct {
    byte: u8,
    line: usize,
};

pub const Chunk = struct {
    code: std.MultiArrayList(Instruction),
    constants: ValueArray,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) Chunk {
        return Chunk{ .code = std.MultiArrayList(Instruction){}, .constants = ValueArray{}, .allocator = allocator };
    }
    pub fn deinit(self: Chunk) void {
        var this = self;
        this.code.deinit(self.allocator);
        this.constants.deinit(self.allocator);
    }

    pub fn len(self: *const Chunk) usize {
        return self.code.len;
    }

    pub fn write(self: *Chunk, bytes: []const u8, line: usize) !void {
        const old_len = self.code.len;
        try self.code.resize(self.allocator, old_len + bytes.len);

        const c_bytes = self.code.items(.byte)[old_len..];
        const c_lines = self.code.items(.line)[old_len..];

        var cur: usize = 0;
        while (cur < bytes.len) : (cur += 1) {
            c_bytes[cur] = bytes[cur];
            c_lines[cur] = line;
        }
    }

    pub fn writeOp(self: *Chunk, op: OpCode, line: usize) !void {
        return self.write(&[_]u8{@enumToInt(op)}, line);
    }
    pub fn writeOpU8(self: *Chunk, op: OpCode, b: u8, line: usize) !void {
        return self.write(&[_]u8{ @enumToInt(op), b }, line);
    }

    pub fn writeConstant(self: *Chunk, value: Value, line: usize) !usize {
        try self.constants.append(self.allocator, value);
        errdefer _ = self.constants.pop(); // clean up constants

        const const_idx = self.constants.items.len - 1;
        if (const_idx > 255) {
            const bytes = [_]u8{@enumToInt(OpCode.CONSTANT_LONG)} ++ std.mem.toBytes(@intCast(u32, const_idx));
            try self.write(&bytes, line);
        } else {
            const bytes = [_]u8{@enumToInt(OpCode.CONSTANT)} ++ [_]u8{@intCast(u8, const_idx)};
            try self.write(&bytes, line);
        }
        return const_idx;
    }
};
