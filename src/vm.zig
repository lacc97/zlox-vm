const std = @import("std");

const debug = @import("debug.zig");

const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;

const Value = @import("value.zig").Value;

pub const InterpretError = error{ compile, runtime };

pub const VM = struct {
    ip: [*]const u8,
    stack: Stack,
    chunk: *const Chunk,

    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) !VM {
        return VM{ .ip = undefined, .stack = try Stack.init(allocator, 9), .chunk = undefined, .allocator = allocator };
    }

    pub fn deinit(self: VM) void {
        self.stack.deinit(self.allocator);
    }

    pub fn interpret(self: *VM, source: []const u8) !void {
        defer {
            self.stack.reset();
            self.chunk = undefined;
        }

        var chunk = try @import("compiler.zig").compile(source);
        self.chunk = &chunk;
        self.ip = self.chunk.code.items(.byte).ptr;

        try self.run();
    }

    fn run(self: *VM) !void {
        while (true) {
            if (comptime debug.trace_execution) {
                self.stack.dump();
                _ = debug.disassembleInstruction(self.chunk, @ptrToInt(self.ip) - @ptrToInt(self.chunk.code.items(.byte).ptr));
            }

            const instruction = @intToEnum(OpCode, self.readU8());
            switch (instruction) {
                .OP_CONSTANT => {
                    const constant = self.readConstant();
                    self.stack.push(constant);
                },
                .OP_CONSTANT_LONG => {
                    const constant = self.readConstantLong();
                    self.stack.push(constant);
                },
                .OP_ADD => {
                    const op = struct {
                        fn op(a: Value, b: Value) Value {
                            return a + b;
                        }
                    }.op;

                    self.binaryOp(op);
                },
                .OP_SUBTRACT => {
                    const op = struct {
                        fn op(a: Value, b: Value) Value {
                            return a - b;
                        }
                    }.op;

                    self.binaryOp(op);
                },
                .OP_MULTIPLY => {
                    const op = struct {
                        fn op(a: Value, b: Value) Value {
                            return a * b;
                        }
                    }.op;

                    self.binaryOp(op);
                },
                .OP_DIVIDE => {
                    const op = struct {
                        fn op(a: Value, b: Value) Value {
                            return a / b;
                        }
                    }.op;

                    self.binaryOp(op);
                },
                .OP_NEGATE => {
                    self.stack.push(-self.stack.pop());
                },
                .OP_RETURN => {
                    debug.printValue(self.stack.pop());
                    std.debug.print("\n", .{});
                    return;
                },

                _ => unreachable,
            }
        }
    }

    inline fn readU8(self: *VM) u8 {
        const byte = self.ip[0];
        self.ip += 1;
        return byte;
    }

    inline fn readU32(self: *VM) u32 {
        const word = std.mem.bytesToValue(u32, self.ip[0..4]);
        self.ip += 4;
        return word;
    }

    inline fn readConstant(self: *VM) Value {
        return self.chunk.constants.items[self.readU8()];
    }
    inline fn readConstantLong(self: *VM) Value {
        return self.chunk.constants.items[self.readU32()];
    }

    inline fn binaryOp(self: *VM, comptime op: fn (a: Value, b: Value) Value) void {
        const b = self.stack.pop();
        const a = self.stack.pop();
        self.stack.push(op(a, b));
    }
};

const Stack = struct {
    top: [*]Value,
    storage: []Value,

    pub fn init(allocator: std.mem.Allocator, size_bits: u6) !Stack {
        const storage = try allocator.alloc(Value, @as(usize, 1) << size_bits);
        return Stack{ .top = bottomPtr(storage), .storage = storage };
    }

    pub fn deinit(self: Stack, allocator: std.mem.Allocator) void {
        allocator.free(self.storage);
    }

    fn bottomPtr(stack: []Value) [*]Value {
        return stack.ptr + stack.len - 1;
    }
    pub fn reset(self: *Stack) void {
        self.top = bottomPtr(self.storage);
    }

    pub fn push(self: *Stack, value: Value) void {
        self.top[0] = value;
        self.top -= 1;
    }
    pub fn pop(self: *Stack) Value {
        self.top += 1;
        return self.top[0];
    }

    fn dump(self: *const Stack) void {
        const print = std.debug.print;

        const top_idx = (@ptrToInt(self.top) - @ptrToInt(self.storage.ptr)) / @sizeOf(Value);

        print("          ", .{});

        for (self.storage[(top_idx + 1)..]) |value| {
            print("[ ", .{});
            debug.printValue(value);
            print(" ]", .{});
        }

        print("\n", .{});
    }
};
