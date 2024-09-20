## Usage

From the root directory, enter ``` cabal install --installdir=<path> ``` to install the executable at your location of choice.  Then, you can interpret a program by providing its path as a command-line argument to the executable.

i.e.
```
cabal install --installdir=.
./glang Program.g
```

## Glang

Glang is a functional programming language in the vein of the ML family. Its implementation lowers programs to bytecode, which is then interpreted.  Glang is non-strictly-evaluated, meaning that an argument to a function isn't evaluated unless it composes the body of the function. 

A program in Glang specifies an entrypoint (a main function), as well as a series of definitions binding identifiers to expressions.  Expressions are defined in a manner analgous to the lambda calculus - an expression is either a function, a value, or the application of a function to another expression.

Syntactically, a function is written as a series of parameters separated by periods, then an expression.  The application of a function is written as a series of arguments separated by single spaces, preceded by the identifier of the function.

```
foo = x. y. x + y + 3

result = foo 4 5
```

Here's an example program, which calculates the ninth term (zero-indexed) of the fibonacci sequence:
```
fib = n. cond ((n == 0) | (n == 1)) n (fib (n-1) + fib (n-2)) 

main = print (fib 9)
```
Currently, there are only two built-in datatypes - floats and boolean values, which are expressed as ```True``` or ```False```.
```
num = 3.815
bool = False
```
The language contains several built-in operations, which deviate from user-defined functions in that they are evaluated strictly - this means that the arguments passed to them are always fully evaluated.  The infix operations, in order of precedence, are:
```
== (equals)
& (logical AND)
| (logical OR)
*, / (multiplication, division)
+, - (addition, subtraction)

-- Example functions using infix operations
arith = x. y. z. x * y + 3
logic = b1. b2. b1 & b2
```
Furthermore, the language includes a ```print``` operation for I/O.  It returns its argument, meaning that you can think of it as an identity function that has a side-effect of printing its argument to standard output.
```
-- prints out 5
x = 5
main = print x
```
For control flow, you can use the ```cond``` operation.  ```cond``` takes three arguments - a boolean value, the expression that is returned if the boolean value is ```True```, and the expression that is returned if it is ```False```.
```
-- prints out 8 
cflow = x. y. (x == y) (x + 3) (y * 4)
main = print (cflow 5 5) 
```
Note that cond is only strict in the first (boolean) argument, and is non-strict in the others.

Here's an example of non-strict evaluation, demonstrating infinite functional lists:
```
-- prints out 4
cons = a. b. f. f a b

head = l. l (x. y. x)
tail = l. l (x. y. y)

inf = x. cons x (inf x)

main = print (head (tail (inf 4)))
```




