# my-language-interpreter
Interpreter of imperative language based on C written in Haskell


Description of Barraquito language (.bq extension):
- based on C grammar, but slightly altered
- statically typed, three types: int, bool and string,
- functions can be nested and recursive,
- parameters are passed by value or by reference (with & notation, as in C++),
- shadowing and static binding of variable and function names,
- program is basically a block without curly braces,
- the starting point of program is the first statement of the program block,
- a block is a list of function definitions followed by a list of statements,
- block determines the scope,
- functions defined at the beginning of the block are visible only in this block
  and its sub-blocks,
- variables are visible only below their declarations,
- break and continue are possible inside a loop,
- blocks can be nested,
- if, if else and while statements are block-based,
- the rest is (I hope) self-explanatory from the grammar and the examples


# Polish version
  Na 15 punktów
- 01 (trzy typy)
- 02 (literały, arytmetyka, porównania)
- 03 (zmienne, przypisanie)
- 04 (print)
- 05 (while, if)
- 06 (funkcje lub procedury, rekurencja)
- 07 (przez zmienną / przez wartość / in/out)
- NIE 08 (zmienne read-only i pętla for)

- Na 20 punktów
- 09 (przesłanianie i statyczne wiązanie)
- 10 (obsługa błędów wykonania)
- 11 (funkcje zwracające wartość)

- Na 30 punktów
- 12 (4) (statyczne typowanie)
- 13 (2) (funkcje zagnieżdżone ze statycznym wiązaniem)
- NIE 14 (1/2) (rekordy/listy/tablice/tablice wielowymiarowe)
- NIE 15 (2) (krotki z przypisaniem)
- 16 (1) (break, continue)
- NIE 17 (4) (funkcje wyższego rzędu, anonimowe, domknięcia)
- NIE 18 (3) (generatory)

Razem: 27 pkt

English version

- 15 points
- three types: int, string, bool
- literals, artihmetics, comparisons
- variables, assignments
- printing every type
- while loop, if and if ... else statements
- functions, recursion
- passing arguments by value and by reference
- 20 points
- variable and function names shadowing, with static binding
- execution error handling
- every return and argument type (3) is possible
- 30 points
- static typing
- nested functions
- loop control statements

Sum: 27 points
