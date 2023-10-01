# Let there be Light

`Lux sit.`

LuxsiT is a compiled language written in OCaml. It compiles down to x86_64 Nasm Assembly.

## Current functionality

### Variable declarations

```
let x;
let y = 1;
let z = 2;
let sum = z + y;
```

### Variable mutability
```
let x = 5;
let y = 2;
x = x + y;
exit x;
```

### Parenthesized expressions

```
let x = 3 / 2 * (3 - (3 / 2)) + 4;
let y = x * (x + x) / x;
```

### Printing an integer
```
let x = 1234;
println x * 54 + (45 - 3);
```

### Exit statements
```
let x = 0;
let y = 234;
exit x * y;
```

### If statements and scopes
```
let x = 1;

if x < 5 then
  let tmp = 34;
  println tmp;
;

exit x;
```
Note: If I put `exit tmp;` outside of the `if` statement, then it would not compile as `tmp` is out of scope.


### While loops
```
let x = 0;

while x < 10 do
  println x;
  x = x + 1;
;

exit 0;
```
