const std = @import("std");

pub const ValueType = enum {
    nil,
    boolean,
    number,
};

pub const Value = union(ValueType) {
    nil: @TypeOf(.{}),
    boolean: bool,
    number: f64,

    pub inline fn val(v: anytype) Value {
        const V = @TypeOf(v);
        if (V == @TypeOf(.{})) {
            return Value.nilVal();
        } else if (V == bool) {
            return Value.boolVal(v);
        } else if (V == f64) {
            return Value.numberVal(v);
        } else {
            @compileError("wrong value type");
        }
    }
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

    pub inline fn isFalsey(self: Value) bool {
        return self.isNil() or (self.isBool() and !self.asBool());
    }
    pub inline fn equals(self: Value, other: Value) bool {
        if (@as(ValueType, self) != @as(ValueType, other)) return false;

        return switch (self) {
            .nil => true,
            .boolean => |a| (a == other.asBool()),
            .number => |a| (a == other.asNumber()),
        };
    }

    pub inline fn asBool(self: Value) bool {
        return self.boolean;
    }
    pub inline fn asNumber(self: Value) f64 {
        return self.number;
    }
};

pub const ValueArray = std.ArrayListUnmanaged(Value);
