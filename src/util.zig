//! Utility types and functions to help with parsing regular expressions.

/// A type alias for single characters.
pub const char = u8;
/// A type alias for strings of characters.
pub const string = []const char;

/// Generate an array of characters when given the ASCII values for the start and end points of the range.
pub fn valueRange(comptime start: char, comptime finish: char) *[finish+1 - start]char {
    comptime {
        if (finish < start) return [0]char{};
        const size: usize = finish+1 - start;
        var output: [size]u8 = undefined;
        for (start..finish+1) |v| {
            output[v-start] = v;
        }
        return &output;
    }
}