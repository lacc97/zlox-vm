const std = @import("std");

const print = std.debug.print;

const ObjType = @import("object.zig").ObjType;
const Obj = @import("object.zig").Obj;
const ObjString = @import("object.zig").ObjString;

pub const ValueType = enum {
    nil,
    boolean,
    number,
    object,
};

pub const Value = union(ValueType) {
    nil: @TypeOf(.{}),
    boolean: bool,
    number: f64,
    object: *Obj,

    pub inline fn val(v: anytype) Value {
        const V = @TypeOf(v);
        if (V == @TypeOf(.{})) {
            return Value.nilVal();
        } else if (V == bool) {
            return Value.boolVal(v);
        } else if (V == f64) {
            return Value.numberVal(v);
        } else {
            const v_info = @typeInfo(V);
            if (v_info == .Pointer) {
                return Value.objVal(v);
            }

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
    pub inline fn objVal(optr: anytype) Value {
        const optr_info = @typeInfo(@TypeOf(optr));
        if (optr_info != .Pointer) {
            @compileError("wrong value type (expected pointer)");
        }

        const O = optr_info.Pointer.child;
        if (O == Obj) {
            return Value{ .object = optr };
        }

        const o_info = @typeInfo(O);
        if (o_info != .Struct) {
            @compileError("wrong value type (expected pointer to struct)");
        }
        if (o_info.Struct.fields.len < 1 or o_info.Struct.fields[0].type != Obj) {
            @compileError("wrong struct type (missing first field with type Obj)");
        }

        return Value{ .object = &@field(optr, o_info.Struct.fields[0].name) };
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
    pub inline fn isObject(self: Value) bool {
        return self == .object;
    }
    pub inline fn isObjectOfType(self: Value, ot: ObjType) bool {
        return self.isObject() and self.asObject().type == ot;
    }
    pub inline fn isString(self: Value) bool {
        return self.isObjectOfType(.string);
    }

    pub inline fn isFalsey(self: Value) bool {
        return self.isNil() or (self.isBool() and !self.asBool());
    }
    pub inline fn equals(self: Value, other: Value) bool {
        if (@as(ValueType, self) != @as(ValueType, other)) return false;

        switch (self) {
            .nil => return true,
            .boolean => |a| return (a == other.asBool()),
            .number => |a| return (a == other.asNumber()),
            .object => |a| {
                if (self.object.type != other.object.type) return false;
                switch (a.type) {
                    .string => return (self.asString().ptr == other.asString().ptr),
                }
            },
        }
    }

    pub inline fn asBool(self: Value) bool {
        return self.boolean;
    }
    pub inline fn asNumber(self: Value) f64 {
        return self.number;
    }
    pub inline fn asObject(self: Value) *Obj {
        return self.object;
    }
    pub inline fn asStringObject(self: Value) *ObjString {
        return @fieldParentPtr(ObjString, "base", self.asObject());
    }
    pub inline fn asString(self: Value) []const u8 {
        return self.asStringObject().string;
    }
};

pub const ValueArray = std.ArrayListUnmanaged(Value);

pub fn printValue(out: anytype, value: Value) void {
    switch (value) {
        .nil => out.print("nil", .{}) catch {},
        .boolean => |b| out.print("{}", .{b}) catch {},
        .number => |n| out.print("{d}", .{n}) catch {},
        .object => printObject(out, value),
    }
}

fn printObject(out: anytype, value: Value) void {
    switch (value.asObject().type) {
        .string => out.print("{s}", .{value.asString()}) catch {},
    }
}
