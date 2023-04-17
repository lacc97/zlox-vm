const std = @import("std");

const OpCode = @import("chunk.zig").OpCode;
const Chunk = @import("chunk.zig").Chunk;

const debug = @import("debug.zig");

const VM = @import("vm.zig").VM;

const stdout = std.io.getStdOut().writer();

pub fn main() !void {
    const args_count = count: {
        var args = std.process.args();
        var c: usize = 0;
        while (args.next()) |_| : (c += 1) {}
        break :count c;
    };

    if (args_count > 2) {
        try stdout.print("Usage: zlox-vm [script]\n", .{});
        std.process.exit(64);
    }

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    const allocator = gpa.allocator();

    var vm = try VM.init(allocator);
    defer vm.deinit();

    if (args_count == 1) {
        repl(&vm);
    } else {
        const filepath = filearg: {
            var args = std.process.args();
            _ = args.skip();
            break :filearg args.next().?;
        };

        runFile(&vm, filepath[0..]);
    }
}

fn repl(vm: *VM) void {
    const stdin = std.io.getStdIn().reader();

    var buf = [_]u8{0} ** 1024;
    while (true) {
        stdout.print("> ", .{}) catch {};

        const line_opt = stdin.readUntilDelimiterOrEof(&buf, '\n') catch |err| {
            stdout.print("\nerror: {}", .{err}) catch {};
            break;
        };
        if (line_opt) |line| {
            vm.interpret(line) catch {
                stdout.print("error\n", .{}) catch {};
            };
        } else {
            stdout.print("\n", .{}) catch {};
            break;
        }
    }
}

fn runFile(vm: *VM, filepath: []const u8) void {
    _ = filepath;
    _ = vm;

    unreachable;
}
