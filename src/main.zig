//! A Regular Expression engine for zig, built entirely in zig.
//! 
//! ```zig
//! const regex = Regex();
//! var re = regex.init("[ab|c]{1,2}(a|b|c)+", std.testing.allocator);
//! defer re.deinit();
//! ````
//! 
//! In the immortal words of Victor of Vasselheim:
//! 
//! > *"Learn from my mistakes!"*

const std = @import("std");
const testing = std.testing;
const LinkedList = std.SinglyLinkedList;
const ArrayList = std.ArrayList;
const util = @import("./util.zig");
const char = util.char;
const string = util.string;

/// Errors related to Regex
pub const RegexError = error{
    GenericRegexError,
    FailedToParseExpression,
    InvalidExpression,
    InvalidExpressionRanOutOfMemory,
};

const OPEN_BRACKETS: [3]char = .{'{', '[', '('};
const CLOSE_BRACKETS: [3]char = .{'}', ']', ')'};

/// Creates an object capable of parsing regular expressions.
/// 
/// Store the Regex instance by calling `const regex = Regex();`
/// 
/// Initialise with `var re = regex.init(<some_regular_expression>, <allocator>);`
/// 
/// Remember to include `defer re.deinit();`
/// 
/// e.g.
/// 
/// ```zig
/// const regex = Regex();
/// var re = regex.init("[ab|c]{1,2}(a|b|c)+", std.testing.allocator);
/// defer re.deinit();
/// ```
pub fn Regex() type {
    return struct {
        /// Stack for parsing regular expressions.
        stack: ArrayList(char),
        /// The regular expression to test against.
        exp: string,

        /// Initialise the regex engine with an `expression`.
        pub fn init(expression: string, allocator: std.mem.Allocator) Self {
            return Self{
                .stack = ArrayList(char).init(allocator),
                .exp = expression,
            };
        }

        /// De-initialise the regex engine to avoid memory leaks.
        pub fn deinit(self: *Self) void {
            self.stack.deinit();
        }

        /// Checks that the given regular expression is valid, containing the correct opening
        /// and closing brackets.
        fn validExp(self: *Self) !bool {
            for (self.exp) |ch| {
                std.log.info("{c}", .{ch});
                if (std.mem.containsAtLeast(char, &OPEN_BRACKETS, 1, &[1]char{ch})) {
                    self.stack.append(ch) catch return std.mem.Allocator.Error.OutOfMemory;
                }
                else if (std.mem.containsAtLeast(char, &CLOSE_BRACKETS, 1, &[1]char{ch})) {
                    if (ch == '}') {
                        if (self.stack.popOrNull() != '{') {
                            return false;
                        }
                    }
                    else if (ch == ']') {
                        if (self.stack.popOrNull() != '[') {
                            return false;
                        }
                    }
                    else if (ch == ')') {
                        if (self.stack.popOrNull() != '(') {
                            return false;
                        }
                    }
                }
            }
            if (self.stack.items.len != 0) {
                return false;
            }
            return true;
        }
        const Self = @This();
    };
}

test "Regex Test" {
    const re = Regex().init("{}[]{{()}}[]{}", std.testing.allocator);
    defer re.deinit();
    try std.testing.expectEqualStrings("{}[]{{()}}[]{}", re.exp);
}
test "Regex String Parseable" {
    std.log.info("Creating Regex object...", .{});
    const regex = Regex();
    std.log.info("Initialising...", .{});
    var re = regex.init("{}[]{{()}}[]{}", std.testing.allocator);
    defer re.deinit();
    try std.testing.expect(re.validExp() catch false);
}
test "Regex String Not Parseable" {
    var re = Regex().init("{}[]{{()}}[]{}]", std.testing.allocator);
    defer re.deinit();
    try std.testing.expectEqual(false, re.validExp() catch true);
}
test "Regex String with literals Parseable" {
    var re = Regex().init("abc{}[]{{(ab|c)}}[]{}", std.testing.allocator);
    defer re.deinit();
    try std.testing.expectEqual(false, re.validExp() catch true);
}
test "Regex String with literals Not Parseable" {
    var re = Regex().init("{}[bc*]{{(acc)}}[]{}]", std.testing.allocator);
    defer re.deinit();
    try std.testing.expectEqual(false, re.validExp() catch true);
}