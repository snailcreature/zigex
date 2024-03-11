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
const valueRange = util.valueRange;

/// Errors related to Regex
pub const RegexError = error{
    GenericRegexError,
    FailedToParseExpression,
    InvalidExpression,
    InvalidExpressionRanOutOfMemory,
    ASTInsertionError,
    ASTInvalidRepetition,
};

/// Opening brackets and braces.
const OPEN_BRACKETS: [3]char = .{'{', '[', '('};
/// Closing brackets and braces.
const CLOSE_BRACKETS: [3]char = .{'}', ']', ')'};
/// All decimal digits from 0 to 9 inclusive.
const ALL_DIGITS: [10]char = valueRange('0', '9').*;
/// Decimal digits from 1 to 9 inclusive.
const DIGITS_NO_0: [9]char = ALL_DIGITS[1..];
/// All Hexadecimal digits from 0 to F inclusive.
const HEX_DIGITS: [16]char = ALL_DIGITS ++ valueRange('A', 'F').*;
/// All lower case letters from 'a' to 'z' inclusive.
const LOWER_CASE_LETTERS: [26]char = valueRange('a', 'z').*;
/// All upper case letters from 'A' to 'Z' inclusive.
const UPPER_CASE_LETTERS: [26]char = valueRange('A', 'Z').*;
/// All letters from 'a' to 'z' and 'A' to 'Z' inclusive.
const ALL_LETTERS: [52]char = LOWER_CASE_LETTERS ++ UPPER_CASE_LETTERS;
/// Common whitespace characters: `' '`, `'\n'`, `'\t'`, and `'\r'`.
const WHITESPACE: [4]char = .{' ', '\n', '\t', '\r'};
/// All prinatble characters from ' ' to '~' inclusive. See [the ASCII table](https://www.asciitable.com/) for reference.
const ALL_VISIBLE_CHARACTERS: [95]char = valueRange(' ', '~').*;
/// All punctuation characters.
const PUNCT: [32]char = valueRange('!', '/').* ++ valueRange(':', '@').* ++ valueRange('[', '`').* ++ valueRange('{', '~').*;

const ASTNodeTypes = enum{
    Expression,
    Alternation,
    CharacterSet,
    Repetition,
    Literal,
    Group,
    GroupClose,
    OneOfRange,
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
        /// Nodes used in the AST.
        astNodes: [128]?AST,

        /// Structure for storing AST nodes.
        const AST = struct {
            /// The type of node this is.
            nodeType: ASTNodeTypes,
            /// The position of the node's value in the original expression.
            pos: usize = 0,
            /// The value of the node, usually a slice of the original expression.
            value: string,
            /// The next node in the AST.
            next: ?*AST = null,
            /// The parent node of this node.
            prev: ?*AST = null,
            /// The child node of this node.
            child: ?*AST = null,
            /// The number of nodes that are decended from this one.
            descendantNodeCount: usize = 0,
            /// Repetition data.
            repeat: struct {
                /// Minimum number of occurrances.
                min: i64,
                /// Maximum number of occrrances.
                max: i64,
            } = .{ .min = 1, .max = 1},

            /// Insert an AST node into an existing AST.
            pub fn insert(self: *AST, newNode: *AST, pos: usize) !void {
                if (self.pos == 0 and pos <= self.descendantNodeCount) {
                    return RegexError.ASTInsertionError;
                }
                self.descendantNodeCount += 1;
                if (self.next) |next| {
                    if (next.pos + next.descendantNodeCount == pos-1) {
                        next.insert(newNode, pos) catch |err| { return err; };
                        return;
                    }
                }
                if (self.nodeType == ASTNodeTypes.Group and newNode.nodeType == ASTNodeTypes.GroupClose) {
                    self.next = newNode;
                    newNode.prev = self;
                    return;
                }
                if (self.child) |child| {
                    if (child.pos + child.descendantNodeCount == pos-1) {
                        child.insert(newNode, pos) catch |err| { return err; };
                        return;
                    }
                }
                switch (newNode.nodeType) {
                    .Expression => {
                        self.child = newNode;
                    },
                    else => {
                        self.next = newNode;
                    }
                }
                newNode.pos = pos;
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
                    .value = expression,
                },
                .astNodes = .{null} ** 128,
            };
            errdefer output.deinit();
            if (!(output.validExp() catch |err| { return err; })) {
                return RegexError.InvalidExpression;
            }
            output.generateAST() catch |err| { return err; };
            return output;
        }

        /// De-initialise the regex engine to avoid memory leaks.
        pub fn deinit(self: *Self) void {
            self.stack.deinit();
        }

        /// Checks that the given regular expression is valid, containing the correct sequence of opening
        /// and closing brackets.
        fn validExp(self: *Self) !bool {
            for (self.exp) |ch| {
                if (std.mem.containsAtLeast(char, &OPEN_BRACKETS, 1, &[1]char{ch})) {
                    self.stack.append(ch) catch |err| { return err; };
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

        /// Generate the AST for the stored expression.
        fn generateAST(self: *Self) !void {
            var i: usize = 0;
            var nodes: usize = 0;
            while (i < self.exp.len) {
                switch (self.exp[i]) {
                    '(' => {
                        self.astNodes[nodes] = AST{
                            .nodeType = ASTNodeTypes.Group,
                            .value = "(",
                        };
                        self.astNodes[nodes+1] = AST{
                            .nodeType = ASTNodeTypes.Expression,
                            .value = "",
                        };
                        self.ast.insert(&self.astNodes[nodes].?, nodes+1) catch |err| { return err; };
                        self.ast.insert(&self.astNodes[nodes+1].?, nodes+2) catch |err| { return err; };
                        i += 1;
                        nodes += 2;
                        continue;
                    },
                    ')' => {
                        self.astNodes[nodes] = AST{
                            .nodeType = ASTNodeTypes.GroupClose,
                            .value = ")",
                        };
                        self.ast.insert(&self.astNodes[nodes].?, nodes+1) catch |err| { return err; };
                    },
                    '{' => {
                        var x: usize = i+1;
                        if (x >= self.exp.len) {
                            if (self.exp[x] == '}') {
                                return RegexError.ASTInvalidRepetition;
                            }
                            while (x < self.exp.len and (self.exp[x] != ',' or self.exp[x] != '}')) {
                                x += 1;
                            }
                            std.log.warn("{s}\n", .{self.exp[i+1..x]});
                            var min: i64 = std.fmt.parseInt(i64, self.exp[i+1..x], 10) catch |err| { return err; };
                            var max = min;
                            const comma: usize = x;
                            if (self.exp[x] != ',') {
                                while (x < self.exp.len and self.exp[x] != '}') {
                                    x += 1;
                                }
                                max = std.fmt.parseInt(i64, self.exp[comma+1..x], 10) catch |err| { return err; };
                            }
                            else {
                                max = -1;
                            }
                            self.astNodes[nodes] = AST{
                                .nodeType = ASTNodeTypes.Repetition,
                                .value = "",
                                .repeat = .{
                                    .min = min,
                                    .max = max,
                                },
                            };
                            self.ast.insert(&self.astNodes[nodes].?, i+1) catch |err| { return err; };
                        }
                        i += x+1;
                        nodes += 1;
                        continue;
                    },
                    '?' => {
                        self.astNodes[nodes] = AST{
                            .nodeType = ASTNodeTypes.Repetition,
                            .value = "?",
                            .repeat = .{
                                .min = 0,
                                .max = 1,
                            },
                        };
                        self.ast.insert(&self.astNodes[nodes].?, nodes+1) catch |err| { return err; };
                    },
                    '*' => {
                        self.astNodes[nodes] = AST{
                            .nodeType = ASTNodeTypes.Repetition,
                            .value = "*",
                            .repeat = .{
                                .min = 0,
                                .max = -1,
                            },
                        };
                        self.ast.insert(&self.astNodes[nodes].?, nodes+1) catch |err| { return err; };
                    },
                    '+' => {
                        self.astNodes[nodes] = AST{
                            .nodeType = ASTNodeTypes.Repetition,
                            .value = "+",
                            .repeat = .{
                                .min = 1,
                                .max = -1,
                            },
                        };
                        self.ast.insert(&self.astNodes[nodes].?, nodes+1) catch |err| { return err; };
                    },
                    '|' => {
                        self.astNodes[nodes] = AST{
                            .nodeType = ASTNodeTypes.Alternation,
                            .value = "|",
                        };
                        self.ast.insert(&self.astNodes[nodes].?, nodes+1) catch |err| { return err; };
                    },
                    '\\' => {
                        const val: string = switch (self.exp[i+1]) {
                            's' => &WHITESPACE,
                            'd' => &ALL_DIGITS,
                            'h' => &HEX_DIGITS,
                            'c' => &LOWER_CASE_LETTERS,
                            'C' => &UPPER_CASE_LETTERS,
                            'l' => &ALL_LETTERS,
                            'p' => &PUNCT,
                            'a' => &ALL_VISIBLE_CHARACTERS,
                            else => self.exp[i+1..i+2],
                        };
                        self.astNodes[nodes] = AST{
                            .nodeType = ASTNodeTypes.OneOfRange,
                            .value = val,
                        };
                        self.ast.insert(&self.astNodes[nodes].?, nodes+1) catch |err| { return err; };
                        i += 2;
                        nodes += 1;
                        continue;
                    },
                    '[' => {
                        var x = i+1;
                        while (self.exp[x] != ']') {
                            x+=1;
                        }
                        self.astNodes[nodes] = AST{
                            .nodeType = ASTNodeTypes.OneOfRange,
                            .value = self.exp[i+1..x],
                        };
                        self.ast.insert(&self.astNodes[nodes].?, nodes+1) catch |err| { return err; };
                        i = x+1;
                        nodes += 1;
                        continue;
                    },
                    else => {
                        self.astNodes[nodes] = AST{
                            .nodeType = ASTNodeTypes.Literal,
                            .value = self.exp[i..i+1],
                        };
                        self.ast.insert(&self.astNodes[nodes].?, nodes+1) catch |err| { return err; };
                    },
                }
                i += 1;
                nodes += 1;
            }
        }

        const Self = @This();
    };
}

test "Regex Test" {
    var re = try Regex().init("abc{2}[abc](ab|c)[de]{3,}", std.testing.allocator);
    defer re.deinit();
    try std.testing.expectEqualStrings("abc{2}[abc](ab|c)[de]{3,}", re.exp);
}
test "Regex String Parseable" {
    std.log.info("Creating Regex object...", .{});
    const regex = Regex();
    std.log.info("Initialising...", .{});
    var re = try regex.init("abc{2}[abc](ab|c)[de]{3,}", std.testing.allocator);
    defer re.deinit();
    try std.testing.expect(re.validExp() catch false);
}
test "Regex String Not Parseable" {
    try std.testing.expectError(RegexError.InvalidExpression, Regex().init("abc{2}[abc](ab|c)[de]{3,}]", std.testing.allocator));
}
test "Regex String with literals Parseable" {
    var re = try Regex().init("abc{2}[abc](ab|c)[de]{3,}", std.testing.allocator);
    defer re.deinit();
    try std.testing.expectEqual(true, re.validExp() catch false);
}
test "Regex String with literals Not Parseable" {
    try std.testing.expectError(RegexError.InvalidExpression, Regex().init("abc{2}[abc](ab|c)[de]{3,}]", std.testing.allocator));
}
test "Regex String with repetition markers Parseable" {
    var re = try Regex().init("a?b*c+", std.testing.allocator);
    defer re.deinit();
}
test "Regex String with alternation Parseable" {
    var re = try Regex().init("a|(bc)|d", std.testing.allocator);
    defer re.deinit();
}