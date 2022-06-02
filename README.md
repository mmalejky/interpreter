## Introduction
Interpreter of my own imperative language called Barraquito based on C written in Haskell.

Grammar of the language is in file ```barraquito.cf```. It is an input file written in
[BNF](https://en.wikipedia.org/wiki/Backus%E2%80%93Naur_form) notation,
for [BNFC](http://bnfc.digitalgrammars.com/) converter.

Sample programs demonstrating the usage and features of the language, as well as testing script
are in ```test``` folder.

## Barraquito language description
Barraquito (*.bq* extension) is an interpreted, statically typed imperative programming language.
It's grammatically and syntactically based on C, but with reference parameters similar to C++,
and no main function as in Python.

## Language features
- basics: literals, variables, assignments, artihmetics, comparisons
- statically typed, three types: int, bool and string,
- functions can be nested and recursive,
- every function return and argument parameter type is possible,
- parameters are passed by value or by reference (with & notation, as in C++),
- shadowing and static binding of variable and function names,
- language is block based, where block is a list of function definitions followed by a list of statements, all surrounded by curly brackets,
- program is basically a block without curly braces, so every block can be treated as a subprogram
- the starting point of program is the first statement of the program block,
- block determines the scope,
- functions defined at the beginning of the block are visible only in this block
  and its sub-blocks,
- variables are visible only below their declarations,
- block is a normal statement, and so can be put anywhere,
- available conditional statements are: if and if {...} else {...}
- while loop statement, as well as loop control statements: break and continue are available
- condidtional and loop statements are block-based,
- printing of every type is possible,
- execution error handling e.g. dividing by zero is present,
- the rest is (I hope) self-explanatory from the grammar and the examples
