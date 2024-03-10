# zigex (WIP)

A Regular Expression engine for zig, built entirely in zig.

Import the `Regex` function into your project and set up a regular expression machine:

```zig
const regex = Regex();
var re = regex.init("[ab|c]{1,2}(a|b|c)+", std.testing.allocator);
defer re.deinit();
```