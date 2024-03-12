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
    FSAFirstNodeFailedToBeCreated,
    FSAUnknownASTNodeType,
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

pub const ASTNodeTypes = enum{
    Expression,
    Alternation,
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

        fsa: ?FSA() = null,

        /// Structure for storing Abstract Syntax Tree nodes.
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
            } = .{ .min = 1, .max = 1 },
            extraInfo: struct {
                option1: ?*AST,
                option2: ?*AST,
            } = .{ .option1 = null, .option2 = null },

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
            
            /// Clean up the AST into its simplest form ready for compilation
            pub fn cleanUp(self: *AST) void {
                // Remove group closures and repetition markers
                switch (self.nodeType) {
                    .GroupClose => {
                        if (self.prev) |parent| {
                            parent.next = self.next;
                        }
                        if (self.next) |next| {
                            next.prev = self.prev;
                        }
                    },
                    .Repetition => {
                        if (self.prev) |parent| {
                            parent.repeat = self.repeat;
                            parent.next = self.next;
                        }
                        if (self.next) |next| {
                            next.prev = self.prev;
                        }
                    },
                    else => {},
                }
                // Clean up children
                if (self.child) |child| {
                    child.cleanUp();
                }
                // Clean up next sibling
                if (self.next) |sibling| {
                    sibling.cleanUp();
                }
                // Resolve alternations
                if (self.nodeType == ASTNodeTypes.Alternation) {
                    if (self.prev.?.prev) |ancestor| {
                        ancestor.next = self;
                    }
                    if (self.next.?.next) |descendant| {
                        descendant.prev = self;
                    }
                    self.extraInfo = .{
                        .option1 = self.prev.?,
                        .option2 = self.next.?,
                    };
                    const tmpPrev = self.prev;
                    const tmpNext = self.next;
                    self.prev = self.prev.?.prev;
                    self.next = self.next.?.next;
                    if (tmpPrev) |prev| {
                        prev.next = null;
                        prev.prev = null;
                    }
                    if (tmpNext) |next| {
                        next.next = null;
                        next.prev = null;
                    }
                }
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
            output.ast.cleanUp();

            output.fsa = FSA().init(&output.ast) catch |err| { return err; };
            errdefer output.fsa.?.deinit();

            return output;
        }

        /// De-initialise the regex engine to avoid memory leaks.
        pub fn deinit(self: *Self) void {
            self.stack.deinit();
            if (self.fsa != null) {
                self.fsa.?.deinit();
            }
        }

        pub fn match(self: *Self, input: string) bool {
            return self.fsa.?.match(input) catch false;
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

const FSANodeTypes = enum{
    Entry,
    State,
    Terminal,
};

/// Transition identifier for any character other than a specified one.
const epsilon: char = 0;
/// Transition identifer for moving forward through the given string
/// without consuming a character.
const technicalMove: char = 127;

/// Finite State Automaton for compiling and matching regular expressions.
pub fn FSA() type {
    return struct {
        /// Entry point for the FSA.
        entryNode: ?*Node = null,
        /// The main terminal Node.
        mainTerminalNode: ?*Node = null,
        /// The current Node being evaluated.
        currentNode: ?*Node = null,
        /// The AST to work through.
        ast: *Regex().AST,
        /// Memory for Nodes.
        nodes: [256]?Node = .{ .{ .nodeType = FSANodeTypes.Entry, } } ++ .{null} ** 254 ++ .{ .{ .nodeType = FSANodeTypes.Terminal, } },
        /// Number of nodes currently stored.
        nodeCount: usize = 1,

        /// A state in the FSA.
        const Node = struct {
            nodeType: FSANodeTypes,
            transitions: [256]?*Node = .{null} ** 256,
        };

        pub fn init(ast: *Regex().AST) !Self {
            var output = Self{
                .ast = ast,
            };
            errdefer output.deinit();
            if (output.nodes[0] == null) {
                return RegexError.FSAFirstNodeFailedToBeCreated;
            }
            output.entryNode = &output.nodes[0].?;
            output.mainTerminalNode = &output.nodes[255].?;
            output.currentNode = output.entryNode;

            output.generateFSA(output.ast, output.entryNode.?, output.mainTerminalNode.?) catch |err| { return err; };
            if (output.entryNode) |entry| {
                output.currentNode = entry;
            }
            return output;
        }

        pub fn deinit(self: *Self) void {
            defer _ = self;
        }

        /// Takes in a string and attempts to match it to the Regular Expression.
        pub fn match(self: *Self, input: string) !bool {
            defer {
                if (self.entryNode) |entry| {
                    self.currentNode = entry;
                }
            }
            _ = self.traverse(input) catch |err| { return err; };
            return self.currentNode.?.nodeType == FSANodeTypes.Terminal;
        }

        /// Takes a string as input and returns the first substring that matches or `null` if none match.
        pub fn matchFirst(self: *Self, input: string) ?string {
            _ = self;
            _ = input;
            return null;
        }

        /// Generate the FSA structure recursively from provided AST.
        /// 
        /// DEVNOTE: When reaching group or alternation, redefine entry and terminal node
        /// to current and next node, rather than previous parameters.
        fn generateFSA(self: *Self, astNode: *Regex().AST, entryNode: *Node, terminalNode: *Node) !void {
            if (astNode.child == null and astNode.next == null) {
                self.currentNode.?.transitions[epsilon] = terminalNode;
                self.currentNode = terminalNode;
                return;
            }
            switch (astNode.nodeType) {
                .Expression => {
                    self.nodes[self.nodeCount] = .{
                        .nodeType = FSANodeTypes.State,
                    };
                    self.currentNode.?.transitions[technicalMove] = &self.nodes[self.nodeCount].?;
                    self.currentNode = &self.nodes[self.nodeCount].?;
                    self.nodeCount += 1;
                    if (astNode.next) |astNext| {
                        self.generateFSA(astNext, entryNode, terminalNode) catch |err| { return err; };
                    }
                },
                .Group => {
                    const newEntry: Node = .{
                        .nodeType = FSANodeTypes.State,
                    };
                    const newTerminal: Node = .{
                        .nodeType = FSANodeTypes.State,
                    };
                    self.nodes[self.nodeCount] = newEntry;
                    const newEntryPos = self.nodeCount;
                    self.nodes[self.nodeCount+1] = newTerminal;
                    const newTerminalPos = self.nodeCount + 1;
                    self.currentNode.?.transitions[technicalMove] = &self.nodes[self.nodeCount].?;
                    
                    // Repeats
                    if (astNode.repeat.min == 0) {
                        self.nodes[self.nodeCount].?.transitions[epsilon] = &self.nodes[self.nodeCount+1].?;
                    }
                    if (astNode.repeat.max == -1) {
                        self.nodes[self.nodeCount+1].?.transitions[epsilon] = &self.nodes[self.nodeCount].?;
                    }
                    
                    self.currentNode = &self.nodes[self.nodeCount].?;
                    self.nodeCount += 2;
                    if (astNode.child) |child| {
                        self.generateFSA(child, &self.nodes[newEntryPos].?, &self.nodes[newTerminalPos].?) catch |err| { return err; };
                    }
                    self.nodes[self.nodeCount] = .{
                        .nodeType = FSANodeTypes.State,
                    };
                    self.nodes[newTerminalPos].?.transitions[technicalMove] = &self.nodes[self.nodeCount].?;
                    self.currentNode = &self.nodes[self.nodeCount].?;
                    self.nodeCount += 1;
                    if (astNode.next) |next| {
                        self.generateFSA(next, entryNode, terminalNode) catch |err| { return err; };
                    }
                },
                .Alternation => {
                    self.nodes[self.nodeCount] = .{
                        .nodeType = FSANodeTypes.State,
                    };
                    const pathA = &self.nodes[self.nodeCount].?;

                    self.nodes[self.nodeCount+1] = .{
                        .nodeType = FSANodeTypes.State,
                    };
                    const pathB = &self.nodes[self.nodeCount+1].?;

                    self.nodes[self.nodeCount+2] = .{
                        .nodeType = FSANodeTypes.State,
                    };
                    const newTerminal = &self.nodes[self.nodeCount+2].?;

                    self.nodeCount += 3;
                    if (astNode.extraInfo.option1) |optionA| {
                        self.currentNode = pathA;
                        self.generateFSA(optionA, pathA, newTerminal) catch |err| { return err; };
                    }
                    if (astNode.extraInfo.option2) |optionB| {
                        self.currentNode = pathB;
                        self.generateFSA(optionB, pathB, newTerminal) catch |err| { return err; };
                    }
                },
                .OneOfRange => {
                    self.nodes[self.nodeCount] = .{
                        .nodeType = FSANodeTypes.State,  
                    };
                    for (astNode.value) |ch| {
                        self.currentNode.?.transitions[ch] = &self.nodes[self.nodeCount].?;
                    }

                    // Repeats
                    if (astNode.repeat.min == 0) {
                        self.currentNode.?.transitions[epsilon] = &self.nodes[self.nodeCount].?;
                    }
                    if (astNode.repeat.max == -1) {
                        self.nodes[self.nodeCount].?.transitions[epsilon] = self.currentNode.?;
                    }

                    self.currentNode = &self.nodes[self.nodeCount].?;
                    self.nodeCount += 1;
                    if (astNode.next) |next| {
                        self.generateFSA(next, entryNode, terminalNode) catch |err| { return err; };
                    }
                },
                .Literal => {
                    self.nodes[self.nodeCount] = .{
                        .nodeType = FSANodeTypes.State,
                    };
                    self.currentNode.?.transitions[astNode.value[0]] = &self.nodes[self.nodeCount].?;

                    // Repeats
                    if (astNode.repeat.min == 0) {
                        self.currentNode.?.transitions[epsilon] = &self.nodes[self.nodeCount].?;
                    }
                    if (astNode.repeat.max == -1) {
                        self.nodes[self.nodeCount].?.transitions[epsilon] = self.currentNode.?;
                    }

                    self.currentNode = &self.nodes[self.nodeCount].?;
                    self.nodeCount += 1;
                    if (astNode.next) |next| {
                        self.generateFSA(next, entryNode, terminalNode) catch |err| { return err; };
                    }
                },
                else => {
                    return RegexError.FSAUnknownASTNodeType;
                }
            }
        }

        /// Tranverse the FSA, attempting transitions based on the first character of the input string.
        fn traverse(self: *Self, input: string) !?string {
            // If we've reached the terminal, there's a match.
            if (self.currentNode.?.nodeType == FSANodeTypes.Terminal) {
                return "";
            }
            // If we've exhausted the input string, return null, triggering an attempted back track.
            if (input.len <= 0) {
                return null;
            }

            const currentNode = self.currentNode.?;
            // Attempt transition based on the first character.
            var transistion = self.currentNode.?.transitions[input[0]];
            if (transistion) |trans| {
                self.currentNode = trans;
                const result = self.traverse(input[1..]) catch |err| { return err; };
                // If exploring that path succeeds, return the result.
                if (result) |res| {
                    var buff: [128]u8 = .{0} ** 128;
                    return std.fmt.bufPrint(&buff, "{c}{s}", .{input[0], res}) catch |err| { return err; };
                    // return input[0..1] ++ res;
                }
            }
            // Otherwise, backtrack and attempt the epsilon path.
            self.currentNode = currentNode;
            transistion = self.currentNode.?.transitions[epsilon];
            if (transistion) |trans| {
                self.currentNode = trans;
                const result = self.traverse(input) catch |err| { return err; };
                if (result) |res| {
                    return res;
                }
            }

            // If epsilon leads nowhere, try a technical move
            self.currentNode = currentNode;
            transistion = self.currentNode.?.transitions[technicalMove];
            if (transistion) |trans| {
                self.currentNode = trans;
                const result = self.traverse(input) catch |err| { return err; };
                if (result) |res| {
                    return res;
                }
            }

            // If all paths fail, return null.
            return null;
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
test "Attempt match on \"bc\"" {
    var re = try Regex().init("a|(bc)|d", std.testing.allocator);
    defer re.deinit();
    try std.testing.expect(re.match("bc"));
}