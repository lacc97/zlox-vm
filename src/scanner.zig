const std = @import("std");

const stdout = std.io.getStdOut().writer();

pub const TokenType = enum(u8) {
    // Single-character tokens.
    LEFT_PAREN,
    RIGHT_PAREN,
    LEFT_BRACE,
    RIGHT_BRACE,
    COMMA,
    DOT,
    MINUS,
    PLUS,
    SEMICOLON,
    SLASH,
    STAR,

    // One or two character tokens.
    BANG,
    BANG_EQUAL,
    EQUAL,
    EQUAL_EQUAL,
    GREATER,
    GREATER_EQUAL,
    LESS,
    LESS_EQUAL,

    // Literals.
    IDENTIFIER,
    STRING,
    NUMBER,

    // Keywords.
    AND,
    CLASS,
    ELSE,
    FALSE,
    FOR,
    FUN,
    IF,
    NIL,
    OR,
    PRINT,
    RETURN,
    SUPER,
    THIS,
    TRUE,
    VAR,
    WHILE,

    // Control.
    ERROR,
};

pub const Token = struct {
    tt: TokenType,
    line: u32,
    lexeme: []const u8,
};

// pub const ScanError = error{
//     unexpected_character,
// };

pub const Scanner = struct {
    cur: [*]const u8,
    beg: [*]const u8,
    end: [*]const u8,
    line: u32,

    pub fn init(source: []const u8) Scanner {
        return Scanner{
            .cur = source.ptr,
            .beg = source.ptr,
            .end = source.ptr + source.len,
            .line = 1,
        };
    }

    pub fn scanToken(self: *Scanner) ?Token {
        self.skipWhitespace();

        self.beg = self.cur;
        if (self.advance()) |ch| {
            if (isAlpha(ch)) return self.identifier();
            if (isDigit(ch)) return self.number();
            return switch (ch) {
                '(' => self.makeToken(.LEFT_PAREN),
                ')' => self.makeToken(.RIGHT_PAREN),
                '{' => self.makeToken(.LEFT_BRACE),
                '}' => self.makeToken(.RIGHT_BRACE),
                ';' => self.makeToken(.SEMICOLON),
                ',' => self.makeToken(.COMMA),
                '.' => self.makeToken(.DOT),
                '-' => self.makeToken(.MINUS),
                '+' => self.makeToken(.PLUS),
                '/' => self.makeToken(.SLASH),
                '*' => self.makeToken(.STAR),

                '!' => self.makeToken(if (self.match('=')) .BANG_EQUAL else .BANG),
                '=' => self.makeToken(if (self.match('=')) .EQUAL_EQUAL else .EQUAL),
                '<' => self.makeToken(if (self.match('=')) .LESS_EQUAL else .LESS),
                '>' => self.makeToken(if (self.match('=')) .GREATER_EQUAL else .GREATER),

                '"' => self.string(),

                else => self.makeErrorToken("unexpected character"),
            };
        } else {
            return null;
        }
    }

    inline fn atEnd(self: *Scanner) bool {
        return @ptrToInt(self.cur) >= @ptrToInt(self.end);
    }

    inline fn advance(self: *Scanner) ?u8 {
        if (self.atEnd()) return null;
        defer self.cur += 1;
        return self.cur[0];
    }
    inline fn match(self: *Scanner, expected: u8) bool {
        if (self.atEnd()) return false;
        if (self.cur[0] != expected) return false;
        self.cur += 1;
        return true;
    }
    inline fn peek(self: *Scanner) ?u8 {
        if (self.atEnd()) return null;
        return self.cur[0];
    }
    inline fn peekNext(self: *Scanner) ?u8 {
        if (@ptrToInt(self.cur + 1) >= @ptrToInt(self.end)) return null;
        return self.cur[1];
    }

    inline fn currentLexeme(self: *Scanner) []const u8 {
        const len = @ptrToInt(self.cur) - @ptrToInt(self.beg);
        return self.beg[0..len];
    }

    fn skipWhitespace(self: *Scanner) void {
        skip: while (self.peek()) |ch| {
            switch (ch) {
                ' ', '\r', '\t' => self.cur += 1,
                '\n' => {
                    self.line += 1;
                    self.cur += 1;
                },
                '/' => {
                    if (self.peekNext() == @as(u8, '/')) {
                        // A comment goes until the end of the line.
                        comment: while (self.peek()) |cch| : (_ = self.advance()) {
                            if (cch == '\n') break :comment;
                        }
                    } else {
                        break :skip;
                    }
                },
                else => break :skip,
            }
        }
    }

    fn identifier(self: *Scanner) Token {
        const checkKeyword = struct {
            fn fun(lexeme: []const u8, kw: []const u8, tt: TokenType) TokenType {
                if (std.mem.eql(u8, lexeme, kw)) return tt;
                return .IDENTIFIER;
            }
        }.fun;
        const identifierType = struct {
            fn fun(lexeme: []const u8) TokenType {
                switch (lexeme[0]) {
                    'a' => return checkKeyword(lexeme[1..], "nd", .AND),
                    'c' => return checkKeyword(lexeme[1..], "lass", .CLASS),
                    'e' => return checkKeyword(lexeme[1..], "lse", .ELSE),
                    'f' => {
                        if (lexeme.len > 1) {
                            switch (lexeme[1]) {
                                'a' => return checkKeyword(lexeme[2..], "lse", .FALSE),
                                'o' => return checkKeyword(lexeme[2..], "r", .FOR),
                                'u' => return checkKeyword(lexeme[2..], "n", .FUN),
                                else => {},
                            }
                        }
                    },
                    'i' => return checkKeyword(lexeme[1..], "f", .IF),
                    'n' => return checkKeyword(lexeme[1..], "il", .NIL),
                    'o' => return checkKeyword(lexeme[1..], "r", .OR),
                    'p' => return checkKeyword(lexeme[1..], "rint", .PRINT),
                    'r' => return checkKeyword(lexeme[1..], "eturn", .RETURN),
                    's' => return checkKeyword(lexeme[1..], "uper", .SUPER),
                    't' => {
                        if (lexeme.len > 1) {
                            switch (lexeme[1]) {
                                'h' => return checkKeyword(lexeme[2..], "is", .THIS),
                                'r' => return checkKeyword(lexeme[2..], "ue", .TRUE),
                                else => {},
                            }
                        }
                    },
                    'v' => return checkKeyword(lexeme[1..], "ar", .VAR),
                    'w' => return checkKeyword(lexeme[1..], "hile", .WHILE),
                    else => {},
                }
                return .IDENTIFIER;
            }
        }.fun;

        id: while (self.peek()) |ch| : (_ = self.advance()) {
            if (!isAlpha(ch) and !isDigit(ch)) break :id;
        }
        return self.makeToken(identifierType(self.currentLexeme()));
    }
    fn number(self: *Scanner) Token {
        num: while (self.peek()) |ch| : (_ = self.advance()) {
            if (!isDigit(ch)) break :num;
        }

        // Look for a fractional part.
        frac: {
            if ((self.peek() orelse break :frac) == '.' and
                isDigit(self.peekNext() orelse break :frac))
            {
                // Consume the '.'.
                _ = self.advance();

                num: while (self.peek()) |ch| : (_ = self.advance()) {
                    if (!isDigit(ch)) break :num;
                }
            }
        }

        return self.makeToken(.NUMBER);
    }
    fn string(self: *Scanner) Token {
        str: while (self.peek()) |ch| : (_ = self.advance()) {
            if (ch == '"') break :str;
            if (ch == '\n') self.line += 1;
        }

        if (self.atEnd()) return self.makeErrorToken("unterminated string");

        // Eat closing quote.
        _ = self.advance();

        return self.makeToken(.STRING);
    }

    fn makeToken(self: *Scanner, tt: TokenType) Token {
        @setCold(false);

        return Token{
            .tt = tt,
            .line = self.line,
            .lexeme = self.currentLexeme(),
        };
    }

    fn makeErrorToken(self: *Scanner, comptime msg: []const u8) Token {
        @setCold(true);

        return Token{
            .tt = .ERROR,
            .line = self.line,
            .lexeme = msg,
        };
    }
};

fn isAlpha(c: u8) bool {
    return (c >= 'A' and c <= 'Z') or (c >= 'a' and c <= 'z') or (c == '_');
}

fn isAlphanumeric(c: u8) bool {
    return isAlpha(c) or isDigit(c);
}

fn isDigit(c: u8) bool {
    return c >= '0' and c <= '9';
}
