const std = @import("std");

const stderr = std.io.getStdErr().writer();

const print = std.debug.print;

const debug = @import("debug.zig");

const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;

const Scanner = @import("scanner.zig").Scanner;
const Token = @import("scanner.zig").Token;
const TokenType = @import("scanner.zig").TokenType;

const Value = @import("value.zig").Value;

const Compiler = struct {
    current: Token,
    previous: Token,

    chunk: *Chunk,

    scanner: Scanner,

    had_error: bool,
    in_panic: bool,

    const Precedence = enum(u8) {
        NONE,
        ASSIGN,
        OR,
        AND,
        EQUALITY,
        COMPARISON,
        TERM,
        FACTOR,
        UNARY,
        CALL,
        PRIMARY,

        pub fn next(self: Precedence) Precedence {
            return switch (self) {
                .PRIMARY => .PRIMARY,
                else => @intToEnum(Precedence, @enumToInt(self) + 1),
            };
        }
    };

    const ExpressionRule = struct {
        prefix: ?*const fn (*Compiler) std.mem.Allocator.Error!void,
        infix: ?*const fn (*Compiler) std.mem.Allocator.Error!void,
        precedence: Precedence,
    };
    const expression_rules = gen_rules: {
        var rules = std.enums.EnumArray(TokenType, ExpressionRule).initFill(ExpressionRule{
            .prefix = null,
            .infix = null,
            .precedence = .NONE,
        });

        rules.set(.LEFT_PAREN, ExpressionRule{ .prefix = grouping, .infix = null, .precedence = .NONE });
        rules.set(.MINUS, ExpressionRule{ .prefix = unary, .infix = binary, .precedence = .TERM });
        rules.set(.PLUS, ExpressionRule{ .prefix = null, .infix = binary, .precedence = .TERM });
        rules.set(.SLASH, ExpressionRule{ .prefix = null, .infix = binary, .precedence = .FACTOR });
        rules.set(.STAR, ExpressionRule{ .prefix = null, .infix = binary, .precedence = .FACTOR });
        rules.set(.NUMBER, ExpressionRule{ .prefix = number, .infix = null, .precedence = .NONE });

        break :gen_rules rules;
    };

    pub fn init(source: []const u8, chunk: *Chunk) Compiler {
        return Compiler{ .current = Token{ .tt = .ERROR, .line = 0, .lexeme = "" }, .previous = undefined, .chunk = chunk, .scanner = Scanner.init(source), .had_error = false, .in_panic = false };
    }
    pub fn deinit(self: *Compiler) bool {
        self.emitOp(.RETURN) catch {
            self.had_error = true;
        };

        if (comptime debug.print_code) {
            if (!self.had_error) {
                debug.disassemble(self.currentChunk(), "code");
            }
        }

        self.scanner.deinit();

        return !self.had_error;
    }

    pub fn advance(self: *Compiler) void {
        self.previous = self.current;

        while (true) {
            self.current = self.scanner.scanToken();
            if (self.current.tt != .ERROR) break;
            self.onError(self.current.lexeme);
        }
    }
    pub fn consume(self: *Compiler, tt: TokenType, msg: []const u8) void {
        if (self.current.tt == tt) {
            self.advance();
        } else {
            self.onErrorAtCurrent(msg);
        }
    }

    fn currentChunk(self: *Compiler) *Chunk {
        return self.chunk;
    }
    fn getRule(tt: TokenType) *const ExpressionRule {
        return expression_rules.getPtrConst(tt);
    }
    fn emitConstant(self: *Compiler, value: Value) !void {
        _ = self.currentChunk().writeConstant(value, self.previous.line) catch |err| {
            stderr.print("failed to emit", .{}) catch {};
            return err;
        };
    }
    fn emitOp(self: *Compiler, op: OpCode) !void {
        self.currentChunk().writeOp(op, self.previous.line) catch |err| {
            stderr.print("failed to emit", .{}) catch {};
            return err;
        };
    }
    fn emitOpU8(self: *Compiler, op: OpCode, b: u8) !void {
        self.currentChunk().writeOpU8(op, b, self.previous.line) catch |err| {
            stderr.print("failed to emit", .{}) catch {};
            return err;
        };
    }

    fn precedence(self: *Compiler, p: Precedence) !void {
        self.advance();

        if (getRule(self.previous.tt).prefix) |prefixRule| {
            try prefixRule(self);
        } else {
            self.onError("expected expression");
        }

        while (@enumToInt(p) <= @enumToInt(getRule(self.current.tt).precedence)) {
            self.advance();
            if (getRule(self.previous.tt).infix) |infixRule| {
                try infixRule(self);
            } else {
                self.onError("expected expression");
            }
        }
    }

    fn expression(self: *Compiler) !void {
        try self.precedence(.ASSIGN);
    }
    fn number(self: *Compiler) !void {
        const value = std.fmt.parseFloat(f64, self.previous.lexeme) catch unreachable;
        try self.emitConstant(value);
    }
    fn grouping(self: *Compiler) !void {
        try self.expression();
        self.consume(.RIGHT_PAREN, "expected ')' after expression");
    }
    fn unary(self: *Compiler) !void {
        const op = self.previous.tt;

        try self.precedence(.UNARY);

        switch (op) {
            .MINUS => try self.emitOp(.NEGATE),
            else => unreachable,
        }
    }
    fn binary(self: *Compiler) !void {
        const op = self.previous.tt;
        const rule = getRule(op);

        // Parse right hand side (left hand side has already been parsed).
        try self.precedence(rule.precedence.next());

        switch (op) {
            .PLUS => try self.emitOp(.ADD),
            .MINUS => try self.emitOp(.SUBTRACT),
            .STAR => try self.emitOp(.MULTIPLY),
            .SLASH => try self.emitOp(.DIVIDE),
            else => unreachable,
        }
    }

    fn onError(self: *Compiler, msg: []const u8) void {
        self.onErrorAt(&self.previous, msg);
    }
    fn onErrorAtCurrent(self: *Compiler, msg: []const u8) void {
        self.onErrorAt(&self.current, msg);
    }

    fn onErrorAt(self: *Compiler, token: *const Token, msg: []const u8) void {
        if (self.in_panic) return;
        self.in_panic = true;

        stderr.print("[line {d}] Error", .{token.line}) catch {};
        if (token.tt == .EOF) {
            stderr.print(" at end", .{}) catch {};
        } else if (token.tt != .ERROR) {
            stderr.print(" at '{s}'", .{token.lexeme}) catch {};
        }

        stderr.print(": {s}\n", .{msg}) catch {};
        self.had_error = true;
    }
};

pub fn compile(source: []const u8, chunk: *Chunk) !bool {
    var compiler = Compiler.init(source, chunk);

    compiler.advance();
    try compiler.expression();

    return compiler.deinit();
}
