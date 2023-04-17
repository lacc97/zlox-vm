const std = @import("std");

pub const ValueType = enum {
    nil,
    boolean,
    number,
};

pub const Value = union(ValueType) {
    nil: void,
    boolean: bool,
    number: f64,

    pub inline fn nilVal() Value {
        return Value{ .nil = .{} };
    }
    pub inline fn boolVal(b: bool) Value {
        return Value{ .boolean = b };
    }
    pub inline fn numberVal(n: f64) Value {
        return Value{ .number = n };
    }

    pub inline fn isNil(self: Value) bool {
        return self == .nil;
    }
    pub inline fn isBool(self: Value) bool {
        return self == .boolean;
    }
    pub inline fn isNumber(self: Value) bool {
        return self == .number;
    }

    pub inline fn asBool(self: Value) bool {
        return self.boolean;
    }
    pub inline fn asNumber(self: Value) f64 {
        return self.number;
    }
};

pub const ValueArray = std.ArrayListUnmanaged(Value);
