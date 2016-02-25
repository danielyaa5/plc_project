# PLC Project 2
## an interpreter for a very simple Java/C-ish language

### Structure
- variables (all variables store either integer value or a boolean value)
- assignment statements
- mathematical expressions
- comparison operators
- boolean operators
- `if` statements
- `while` statements
- `return` statements
- `goto`-type constructs: 
	- `break` --> `(break)`
	- `continue` --> `(continue)`
	- (true) `return`
	- `throw` --> `(throw e)`
- `continue`
- blocks `{ ... }`

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

Example 2:

```java
if (i < j) {				=>   (if (< i j) (begin (= i (+ i 1)) (= j (+ j 1))))
  i = i + 1;
  j = j - 1;
}

try {						=>   (try body (catch (e) body) (finally body))
  body
}
catch (e) {
  body
}
finally {
  body
}
```

Note that either the finally or the catch block may be empty:

```
try {                     =>  (try body (catch (e) body) ())
  body
}
catch (e) {
  body
}
```

#### Note:

- As with C and Java, a block of code can appear anywhere and not only as the body of an if statement or a loop.
- As with C and Java, the break and continue apply to the immediate loop they are inside. There are no labels in our interpreter, and so there will be no breaking out of multiple loops with one break statement.
- As there is no type checking in our language, only one catch statement per try block is allowed.

#### The following are implemented:

- Mathematical operations: `+`, `-`, `*`, `/`, `%` (including the unary `-`).
- Comparison operators: `==`, `!=`, `<`, `>`, `<=`. `>=`
- Boolean operators: `&&`, `||`, `!`
- Variables may store values of type `int` as well as `true` and `false`
- Short-circuit evaluation of `&&` or `||` are *not* implemented.
- The program does not necessarily detects an error if there is a mismatching type

### Implementing `Goto` constructs
Use continuations to properly implement return, break, continue, and throw. For each, there are two options:

- Make the interpreter tail-recursive with continuation passing style (note that only the M_state functions must be tail recursive), or
- Use `call/cc` 

Use cps for some of the constructs and call/cc for others

## Naming convention
- Function name ending with `?` returns boolean value; e.g. `statement?` will check whether the given parameter is a statement or not, and return either `#t` or `#f`.
- "M-State" functions will be the following format: `M_state-doSomething`

## Credit
- Daniel Yakobian `djy18`
- Justin Wang `jsw104`
- Hun Jae Lee `hxl224`
