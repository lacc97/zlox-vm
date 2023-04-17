const std = @import("std");

const stderr = std.io.getStdErr().writer();

const debug = @import("debug.zig");

const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;

const Value = @import("value.zig").Value;

pub const InterpretError = error{ out_of_memory, compile, runtime };

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

    pub fn interpret(self: *VM, source: []const u8) InterpretError!void {
        const compile = @import("compiler.zig").compile;

        defer {
            self.stack.reset();
            self.chunk = undefined;
        }

        var chunk = Chunk.init(self.allocator);
        defer chunk.deinit();

        if (!(compile(source, &chunk) catch return InterpretError.out_of_memory)) {
            return InterpretError.compile;
        }

        self.chunk = &chunk;
        self.ip = self.chunk.code.items(.byte).ptr;

        try self.run();
    }

    fn run(self: *VM) InterpretError!void {
        while (true) {
            if (comptime debug.trace_execution) {
                self.stack.dump();
                _ = debug.disassembleInstruction(self.chunk, @ptrToInt(self.ip) - @ptrToInt(self.chunk.code.items(.byte).ptr));
            }

            const instruction = @intToEnum(OpCode, self.readU8());
            switch (instruction) {
                .CONSTANT => {
                    const constant = self.readConstant();
                    self.stack.push(constant);
                },
                .CONSTANT_LONG => {
                    const constant = self.readConstantLong();
                    self.stack.push(constant);
                },
                .ADD => {
                    const op = struct {
                        fn op(a: f64, b: f64) f64 {
                            return a + b;
                        }
                    }.op;

                    try self.binaryOp(op);
                },
                .SUBTRACT => {
                    const op = struct {
                        fn op(a: f64, b: f64) f64 {
                            return a - b;
                        }
                    }.op;

                    try self.binaryOp(op);
                },
                .MULTIPLY => {
                    const op = struct {
                        fn op(a: f64, b: f64) f64 {
                            return a * b;
                        }
                    }.op;

                    try self.binaryOp(op);
                },
                .DIVIDE => {
                    const op = struct {
                        fn op(a: f64, b: f64) f64 {
                            return a / b;
                        }
                    }.op;

                    try self.binaryOp(op);
                },
                .NEGATE => {
                    if (!self.stack.peek(0).isNumber()) {
                        return self.runtimeError("operand must be a number", .{});
                    }
                    self.stack.push(Value.numberVal(-self.stack.pop().asNumber()));
                },
                .RETURN => {
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

    inline fn binaryOp(self: *VM, comptime op: fn (a: f64, b: f64) f64) InterpretError!void {
        if (!self.stack.peek(0).isNumber() or !self.stack.peek(1).isNumber()) {
            return self.runtimeError("operands must be numbers", .{});
        }

        const b = self.stack.pop().asNumber();
        const a = self.stack.pop().asNumber();
        self.stack.push(Value.numberVal(op(a, b)));
    }

    fn runtimeError(self: *VM, comptime fmt: []const u8, args: anytype) InterpretError {
        stderr.print(fmt, args) catch {};
        stderr.print("\n", .{}) catch {};

        const instruction = @ptrToInt(self.ip) - @ptrToInt(self.chunk.code.items(.byte).ptr) - 1;
        const line = self.chunk.code.items(.line)[instruction];
        stderr.print("[line {d}] in script\n", .{line}) catch {};

        self.stack.reset();

        return InterpretError.runtime;
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

    pub fn peek(self: *Stack, distance: usize) Value {
        return self.top[1 + distance];
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

        const tidx = (@ptrToInt(self.top) - @ptrToInt(self.storage.ptr)) / @sizeOf(Value);

        print("          ", .{});

        for (self.storage[(tidx + 1)..]) |value| {
            print("[ ", .{});
            debug.printValue(value);
            print(" ]", .{});
        }

        print("\n", .{});
    }
};
