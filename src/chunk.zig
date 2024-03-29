const std = @import("std");

const Value = @import("value.zig").Value;
const ValueArray = @import("value.zig").ValueArray;

pub const OpCode = enum(u8) {
    CONST,
    CONST_LONG,
    NIL,
    TRUE,
    FALSE,
    POP,
    GET_GLOBAL,
    GET_GLOBAL_LONG,
    DEF_GLOBAL,
    DEF_GLOBAL_LONG,
    EQ,
    GT,
    LT,
    ADD,
    SUB,
    MUL,
    DIV,
    NOT,
    NEG,
    PRINT,
    RET,
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

    pub fn addConstant(self: *Chunk, value: Value) !u32 {
        try self.constants.append(self.allocator, value);
        return @intCast(u32, self.constants.items.len - 1);
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
        const bytes = [_]u8{@enumToInt(op)} ++ [_]u8{b};
        return self.write(&bytes, line);
    }
    pub fn writeOpU32(self: *Chunk, op: OpCode, u: u32, line: usize) !void {
        const bytes = [_]u8{@enumToInt(op)} ++ std.mem.toBytes(u);
        return self.write(&bytes, line);
    }

    pub fn writeConstant(self: *Chunk, value: Value, line: usize) !u32 {
        const const_idx = try self.addConstant(value);
        errdefer _ = self.constants.pop(); // clean up constants

        if (const_idx > 255) {
            try self.writeOpU32(.CONST_LONG, const_idx, line);
        } else {
            try self.writeOpU8(.CONST, @intCast(u8, const_idx), line);
        }
        return const_idx;
    }
};
