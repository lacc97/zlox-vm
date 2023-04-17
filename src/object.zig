const std = @import("std");

pub const ObjType = enum {
    string,
};

pub const Obj = struct {
    type: ObjType,
};

pub const ObjAllocator = struct {
    arena: std.heap.ArenaAllocator,

    pub fn Subtype(comptime ot: ObjType) type {
        return switch (ot) {
            .string => ObjString,
        };
    }

    pub fn init(allocator: std.mem.Allocator) ObjAllocator {
        return ObjAllocator{ .arena = std.heap.ArenaAllocator.init(allocator) };
    }

    pub fn deinit(self: *ObjAllocator) void {
        self.arena.deinit();
    }

    pub fn create(self: *ObjAllocator, comptime ot: ObjType) !*Subtype(ot) {
        const obj = try self.arena.allocator().create(Subtype(ot));
        obj.base = Obj{ .type = ot };
        return obj;
    }
};

pub const ObjString = struct {
    base: Obj,
    string: []u8,
};
