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
    ASTInsertionError
};

/// Opening brackets and braces.
const OPEN_BRACKETS: [3]char = .{'{', '[', '('};
/// Closing brackets and braces.
const CLOSE_BRACKETS: [3]char = .{'}', ']', ')'};
/// All decimal digits from 0 to 9 inclusive.
const ALL_DIGITS: [10]char = .{for ('0'..'9') |i| i};
/// Decimal digits from 1 to 9 inclusive.
const DIGITS_NO_0: [9]char = ALL_DIGITS[1..];
/// All Hexadecimal digits from 0 to F inclusive.
const HEX_DIGITS: [16]char = ALL_DIGITS ++ .{for ('A'..'F') |i| i};
/// All lower case letters from 'a' to 'z' inclusive.
const LOWER_CASE_LETTERS: [26]char = .{for ('a'..'z') |l| l};
/// All upper case letters from 'A' to 'Z' inclusive.
const UPPER_CASE_LETTERS: [26]char = .{for ('A'..'Z') |l| l};
/// All letters from 'a' to 'z' and 'A' to 'Z' inclusive.
const ALL_LETTERS: [52]char = LOWER_CASE_LETTERS ++ UPPER_CASE_LETTERS;
/// Common whitespace characters: `' '`, `'\n'`, `'\t'`, and `'\r'`.
const WHITESPACE: [4]char = .{' ', '\n', '\t', '\r'};
/// All prinatble characters from ' ' to '~' inclusive. See [the ASCII table](https://www.asciitable.com/) for reference.
const ALL_VISIBLE_CHARACTERS: [95]char = .{for (' '..'~') |c| c};

const ASTNodeTypes = enum{
    Expression,
    Alternation,
    CharacterSet,
    Repetition,
    Literal,
    Group,
};

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
        /// Abstract Syntax Tree for the stored regular expression.
        ast: AST,

        /// Structure for storing AST nodes.
        const AST = struct {
            /// The type of node this is.
            nodeType: ASTNodeTypes,
            /// The position of the node's value in the original expression.
            pos: i64,
            /// The value of the node, usually a slice of the original expression.
            value: string,
            /// The next node in the AST.
            next: ?*AST = null,
            /// The parent node of this node.
            prev: ?*AST = null,
            /// The child node of this node.
            child: ?*AST = null,
            /// The number of nodes that are decended from this one.
            descendantNodeCount: i64 = 0,
            /// Repetition data.
            repeat: struct {
                /// Minimum number of occurrances.
                min: i64,
                /// Maximum number of occrrances.
                max: i64,
            } = .{ .min = 1, .max = 1},

            /// Insert an AST node into an existing AST.
            pub fn insert(self: *AST, newNode: *AST) !void {
                if (newNode.pos <= self.descendantNodeCount) {
                    return RegexError.ASTInsertionError;
                }
                self.descendantNodeCount += 1;
                if (self.child) |child| {
                    if (child.pos + child.descendantNodeCount == newNode.pos-1) {
                        child.insert(newNode);
                        return;
                    }
                }
                if (self.next) |next| {
                    if (next.pos + next.descendantNodeCount == newNode.pos-1) {
                        next.insert(newNode);
                        return;
                    }
                }
                switch (newNode.nodeType) {
                    .Expression => {
                        self.child = newNode;
                    },
                    _ => {
                        self.next = newNode;
                    }
                }
                newNode.prev = self;
            }
        };

        /// Initialise the regex engine with an `expression`.
        pub fn init(expression: string, allocator: std.mem.Allocator) !Self {
            var output: Self = Self{
                .stack = ArrayList(char).init(allocator),
                .exp = expression,
                .ast = AST{
                    .nodeType = ASTNodeTypes.Expression,
                    .pos = 0,
                    .value = expression,
                },
            };
            errdefer output.deinit();
            if (!(output.validExp() catch false)) {
                return RegexError.InvalidExpression;
            }
            return output;
        }

        /// De-initialise the regex engine to avoid memory leaks.
        pub fn deinit(self: *Self) void {
            self.stack.deinit();
        }

        // pub fn match(self: *Self, sample: string) !bool {
        //     // Check that the stored expression is valid, return an error if not.
        //     if (!self.validExp() catch return RegexError.InvalidExpressionRanOutOfMemory) {
        //         return RegexError.InvalidExpression;
        //     }
            
        //     // 
        // }

        /// Checks that the given regular expression is valid, containing the correct sequence of opening
        /// and closing brackets.
        fn validExp(self: *Self) !bool {
            for (self.exp) |ch| {
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

        // fn generateAST(self: *Self) !void {
        //     var i: i64 = 0;
        //     while (i < self.exp.len) {
        //         switch (self.exp[i]) {
        //             '(' => {
        //                 self.ast.insert(AST{
        //                     .pos = i+1,
        //                     .nodeType = ASTNodeTypes.Group,
        //                     .value = self.exp[i],
        //                 });
        //             },
        //             _ => { continue; },
        //         }
        //     }
        // }

        const Self = @This();
    };
}

test "Regex Test" {
    var re = try Regex().init("{}[]{{()}}[]{}", std.testing.allocator);
    defer re.deinit();
    try std.testing.expectEqualStrings("{}[]{{()}}[]{}", re.exp);
}
test "Regex String Parseable" {
    std.log.info("Creating Regex object...", .{});
    const regex = Regex();
    std.log.info("Initialising...", .{});
    var re = try regex.init("{}[]{{()}}[]{}", std.testing.allocator);
    defer re.deinit();
    try std.testing.expect(re.validExp() catch false);
}
test "Regex String Not Parseable" {
    try std.testing.expectError(RegexError.InvalidExpression, Regex().init("{}[]{{()}}[]{}]", std.testing.allocator));
}
test "Regex String with literals Parseable" {
    var re = try Regex().init("abc{}[]{{(ab|c)}}[]{}", std.testing.allocator);
    defer re.deinit();
    try std.testing.expectEqual(true, re.validExp() catch false);
}
test "Regex String with literals Not Parseable" {
    try std.testing.expectError(RegexError.InvalidExpression, Regex().init("{}[bc*]{{(acc)}}[]{}]", std.testing.allocator));
}