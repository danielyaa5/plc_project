# PLC Project 1
## an interpreter for a very simple Java/C-ish language

### Structure
- variables
- assignment statements
- mathematical expressions
- comparison operators
- boolean operators
- `if` statements
- `while` statements
- `return` statements

braces (`{` and `}`) are not supported.

Example:

```java
var x;
x = 10;
var y = 3 * x + 5;
while (y % x != 3)
  y = y + 1;
if (x > y)
  return x;
else if (x * x > y)
  return x * x;
else if (x * (x + x) > y)
  return x * (x + x);
else 
  return y - 1;  
```

The following are implemented:

- Mathematical operations: `+`, `-`, `*`, `/`, `%` (including the unary `-`).
- Comparison operators: `==`, `!=`, `<`, `>`, `<=`. `>=`
- Boolean operators: `&&`, `||`, `!`
- Variables may store values of type `int` as well as `true` and `false`
- Short-circuit evaluation of `&&` or `||` are *not* implemented.
- The program does not necessarily detects an error if there is a mismatching type

### Style
The coding style will follow [Scheme wiki's style guide](http://community.schemewiki.org/?scheme-style).

## Credit
- Daniel Yakobian `djy18`
- Justin Wang `jsw104`
- Hun Jae Lee `hxl224`
