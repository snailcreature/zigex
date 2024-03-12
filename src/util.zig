//! Utility types and functions to help with parsing regular expressions.

const std = @import("std");

/// A type alias for single characters.
pub const char = u8;
/// A type alias for strings of characters.
pub const string = []const char;

/// Generate an array of characters when given the ASCII values for the start and end points of the range.
pub fn valueRange(comptime start: char, comptime finish: char) *[finish+1 - start]char {
    if (finish < start) return [0]char{};
    const size: usize = @as(usize, finish)+1 - @as(usize, start);
    var output: [size]char = .{0} ** size;
    for (0..output.len, start..finish+1) |v, a| {
        const val: u8 = @intCast(a);
        output[v] = val;
    }
    return &output;
}

test "Generate Value Range" {
    const valRan = valueRange('a', 'e').*;
    const expected = [5]u8{'a', 'b', 'c', 'd', 'e'};
    try std.testing.expectEqual(expected, valRan);
    try std.testing.expectEqualStrings("abcde", &valRan);
}
test "Char Array to String" {
    const valRan = valueRange('0', '9').*;
    const valStr: string = &valRan;
    try std.testing.expectEqualStrings("0123456789", valStr);
}