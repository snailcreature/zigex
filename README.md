# zigex (WIP)

A Regular Expression engine for zig, built entirely in zig.

Import the `Regex` function into your project and set up a regular expression machine:

```zig
const regex = Regex();
var re = regex.init("(ab|c){1,2}(a|b|c)+", std.testing.allocator);
defer re.deinit();
```
> `"(ab|c){1,2}(a|b|c)+"` compiles as `(a and (b or c)) 1 or 2 times then ((a or b) or c) 1 or more times`.

- [X] Expression storage and validation (bracket alignment) (COMPLETED: 09/03/2024)
- [ ] Expression Parser (WIP)
- [ ] Expression Compiler
- [ ] Expression Matcher