const std = @import("std");

const stdout = std.io.getStdOut().writer();
const stderr = std.io.getStdErr().writer();

const debug = @import("debug.zig");

const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;

const Value = @import("value.zig").Value;
const ObjType = @import("object.zig").ObjType;
const ObjAllocator = @import("object.zig").ObjAllocator;
const ObjString = @import("object.zig").ObjString;

pub const InterpretError = error{ out_of_memory, compile, runtime };

pub const VM = struct {
    pub const Mem = struct {
        allocator: std.mem.Allocator,
        obj_allocator: ObjAllocator,
        string_allocator: StringIntern,

        pub fn init(allocator: std.mem.Allocator) Mem {
            return Mem{ .allocator = allocator, .obj_allocator = ObjAllocator.init(allocator), .string_allocator = StringIntern.init(allocator) };
        }
        pub fn deinit(self: *Mem) void {
            self.obj_allocator.deinit();
            self.string_allocator.deinit();
        }

        pub fn createObj(self: *Mem, comptime ot: ObjType) !*ObjAllocator.Subtype(ot) {
            return self.obj_allocator.create(ot);
        }

        pub fn allocString(self: *Mem, len: usize) ![]u8 {
            return self.string_allocator.alloc(len);
        }
        pub fn concatStrings(self: *Mem, strs: []const []const u8) ![]u8 {
            return std.mem.concat(self.string_allocator.allocator, u8, strs);
        }
        pub fn dupeString(self: *Mem, str: []const u8) ![]u8 {
            return self.string_allocator.allocator.dupe(u8, str);
        }
        pub fn internString(self: *Mem, str: []const u8) ![]const u8 {
            return self.string_allocator.intern(str);
        }
    };

    ip: [*]const u8,
    stack: Stack,
    chunk: *const Chunk,

    mem: Mem,

    pub fn init(allocator: std.mem.Allocator) !VM {
        return VM{ .ip = undefined, .stack = try Stack.init(allocator, 9), .chunk = undefined, .mem = Mem.init(allocator) };
    }

    pub fn deinit(self: *VM) void {
        self.mem.deinit();
        self.stack.deinit(self.mem.allocator);
    }

    pub fn interpret(self: *VM, source: []const u8) InterpretError!void {
        const compile = @import("compiler.zig").compile;

        defer {
            self.stack.reset();
            self.chunk = undefined;
        }

        var chunk = Chunk.init(self.mem.allocator);
        defer chunk.deinit();

        if (!(compile(&self.mem, source, &chunk) catch return InterpretError.out_of_memory)) {
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
                .CONST => {
                    const constant = self.readConstant();
                    self.stack.push(constant);
                },
                .CONST_LONG => {
                    const constant = self.readConstantLong();
                    self.stack.push(constant);
                },
                .NIL => self.stack.push(Value.val(.{})),
                .TRUE => self.stack.push(Value.val(true)),
                .FALSE => self.stack.push(Value.val(false)),
                .POP => _ = self.stack.pop(),
                .EQ => {
                    const b = self.stack.pop();
                    const a = self.stack.pop();
                    self.stack.push(Value.val(a.equals(b)));
                },
                .GT => {
                    const op = struct {
                        fn op(a: f64, b: f64) bool {
                            return a > b;
                        }
                    }.op;

                    try self.binaryOp(bool, op);
                },
                .LT => {
                    const op = struct {
                        fn op(a: f64, b: f64) bool {
                            return a < b;
                        }
                    }.op;

                    try self.binaryOp(bool, op);
                },
                .ADD => {
                    if (self.stack.peek(0).isNumber() and self.stack.peek(1).isNumber()) {
                        const b = self.stack.pop().asNumber();
                        const a = self.stack.pop().asNumber();
                        self.stack.push(Value.val(a + b));
                    } else if (self.stack.peek(0).isString() and self.stack.peek(1).isString()) {
                        const b = self.stack.pop().asString();
                        const a = self.stack.pop().asString();
                        const c = self.mem.concatStrings(&.{ a, b }) catch return InterpretError.out_of_memory;
                        const c_obj = self.mem.createObj(.string) catch return InterpretError.out_of_memory;
                        c_obj.string = self.mem.internString(c) catch return InterpretError.out_of_memory;
                        self.stack.push(Value.val(c_obj));
                    } else {
                        return self.runtimeError("operands must be two numbers or two strings", .{});
                    }
                },
                .SUB => {
                    const op = struct {
                        fn op(a: f64, b: f64) f64 {
                            return a - b;
                        }
                    }.op;

                    try self.binaryOp(f64, op);
                },
                .MUL => {
                    const op = struct {
                        fn op(a: f64, b: f64) f64 {
                            return a * b;
                        }
                    }.op;

                    try self.binaryOp(f64, op);
                },
                .DIV => {
                    const op = struct {
                        fn op(a: f64, b: f64) f64 {
                            return a / b;
                        }
                    }.op;

                    try self.binaryOp(f64, op);
                },
                .NOT => {
                    self.stack.push(Value.val(self.stack.pop().isFalsey()));
                },
                .NEG => {
                    if (!self.stack.peek(0).isNumber()) {
                        return self.runtimeError("operand must be a number", .{});
                    }
                    self.stack.push(Value.val(-self.stack.pop().asNumber()));
                },
                .PRINT => {
                    @import("value.zig").printValue(stdout, self.stack.pop());
                    stdout.print("\n", .{}) catch {};
                },
                .RET => {
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

    inline fn binaryOp(self: *VM, comptime Return: type, comptime op: fn (a: f64, b: f64) Return) InterpretError!void {
        if (!self.stack.peek(0).isNumber() or !self.stack.peek(1).isNumber()) {
            return self.runtimeError("operands must be numbers", .{});
        }

        const b = self.stack.pop().asNumber();
        const a = self.stack.pop().asNumber();
        self.stack.push(Value.val(op(a, b)));
    }

    fn runtimeError(self: *VM, comptime fmt: []const u8, args: anytype) InterpretError {
        @setCold(true);

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

    pub inline fn peek(self: *Stack, distance: usize) Value {
        return self.top[1 + distance];
    }
    pub inline fn push(self: *Stack, value: Value) void {
        self.top[0] = value;
        self.top -= 1;
    }
    pub inline fn pop(self: *Stack) Value {
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

pub const StringIntern = struct {
    table: std.StringHashMapUnmanaged(void),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) StringIntern {
        return StringIntern{ .table = std.StringHashMapUnmanaged(void){}, .allocator = allocator };
    }
    pub fn deinit(self: *StringIntern) void {
        var it = self.table.keyIterator();
        while (it.next()) |k| {
            self.allocator.free(k.*);
        }
        self.table.deinit(self.allocator);
    }

    pub fn alloc(self: *StringIntern, len: usize) ![]u8 {
        return self.allocator.alloc(u8, len);
    }
    pub fn intern(self: *StringIntern, s: []const u8) ![]const u8 {
        const res = try self.table.getOrPut(self.allocator, s);

        if (res.found_existing) {
            self.allocator.free(s);
        }

        return res.key_ptr.*;
    }
};
