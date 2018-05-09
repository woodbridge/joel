## Joel: a quick csv processing language

### Team Members
- Mari Husain: mh3685@columbia.edu 
- Justin Woodbridge: jrw2190@columbia.edu 
- Josh Learn: jrl2196@columbia.edu 
- Nadav Gov-Ari: ng2604@columbia.edu

### Requirements
Requires python 3.6.4 and LLI with LLVM 3.8.1 or 6.0.0

#### How to Compile the Compiler
To run the script to compile the compiler, simply run ``python3 build.py``. If all went well, you should see ``SUCCESSFULLY BUILT COMPILER`` on the screen. Otherwise, you should see a ``FAIL`` message.

#### How to Hand-Compile and Run the Test Programs
- Compile the compiler (as above).

##### To Generate/Execute Code
- To just generate code for the program and then run it immediately, the command ``./toplevel.native -c <filename> | <lli-path>``, where ``<filename>`` is the name of the Joel file you want to compile (for example, hello.joel) and ``<lli-path>`` is the path to your copy of lli (for example, ``lli`` or ``/usr/local/opt/llvm@3.8/bin/lli-3.8``)
- You should see the output of your Joel program on the screen.

##### To Compile to an Executable
- These instructions will assume your program is named ``test.joel``. Replace all instances of ``test`` with the name of whatever program you want to compile.
- run ``gcc -c c-lib.c``
- run ``./toplevel.native test.joel > test.ll``
- run ``llc test.ll``
- run ``gcc -c test.s``
- run ``g++ c-lib.o test.o -o test``
- You should now have an executable named ``test``.

#### To Run the Test Suite:
- First, locate information about your copy of LLI. For example, running ``lli --version`` reveals that my LLI is running with LLVM 6.0.0. If your LLI runs something other than LLVM 6.0.0 or LLVM 3.8.1, please locate a copy of LLI on your system that does run it (ex. ``'/usr/local/opt/llvm@3.8/bin/lli-3.8'``)
- If your LLI does not run the correct LLVM version, open extended-testsuite.py in a text editor. Go to the top of the file and change ``LLI = "lli"`` to whatever path points to your copy of lli.
- Save extended-testsuite.py.
- To run the test suite, run ``python3 extended-testsuite.py``. Make sure you are in the ``joel`` top-level directory!!
- If all went well, you should see ``TEST SUITE PASSED`` on the screen. Otherwise, you should see a ``TEST SUITE FAILED`` message and the test(s) that failed and the lines that differ between the gold standard and the actual output.


#### The Tests in Our Test Suite

##### Positive Tests
1 test-and-or.joel: Tests boolean and (&) and or (|) operators.
2 test-binop.joel: Tests binary operations (+, -, *, /).
3 test-bool.joel: Tests boolean literals.
4 test-for.joel: Tests for loops.
5 test-foreach.joel: Test for-each loopsl
6 test-function-assign.joel: Tests assignment of the result of a function to a variable.
7 test-function-noargs.joel: Tests functions that take no argument.
8 test-function-table.joel: Tests functions that take and return tables.
9 test-function-void.joel: Tests void functions.
10 test-function.joel: Tests functions.
11 test-if.joel: Tests if statements.
12 test-in.joel: Tests input()
13 test-incr-assign.joel: Tests the increment-assignment operator (+=).
14 test-list-empty.joel: Tests empty lists.
15 test-list-reassign.joel: Tests re-assignment of lists to other lists.
16 test-list-varassign.joel: Tests list variable assignment.
17 test-list.joel: Tests lists.
18 test-neg.joel: Tests the negative (-) unary operator.
19 test-not.joel: Tests the not (!) unary operator.
20 test-num.joel: Tests the num type.
21 test-postop.joel: Tests postop (++ and --) operators.
22 test-scope.joel: Tests scoping rules - both global and local variables, which may be declared anywhere within the scope (not just at the beginning of the scope) and re-defined.
23 test-string.joel: Tests string literals.
24 test-table-alter.joel: Tests table alteration.
25 test-table.joel: Tests tables.
26 test-while.joel: Tests the while loop. 

##### Negative Tests
1 fail-and.joel: Attempts to use and on a boolean and a string.
2 fail-assign.joel: Attempts to assign a string to a variable of num type.
3 fail-function-argument.joel: Attempts to pass an argument to a function that takes none.
4 fail-function-return.joel: Attempts to return a string from a num type function.
5 fail-if.joel: Attempts to use a string as an if condition.
6 fail-in.joel: Attempts to read in a file that does not exist.
7 fail-list-emptyassign.joel: Attempts to assign an empty list of nums to a string list.
8 fail-list-varassign.joel: Attempts to assign a num list to a variable holding a string list.
9 fail-list.joel: Attempts to create a mixed-type list.
10 fail-multiplication.joel: Attempts to multiply two strings.
11 fail-neg.joel: Attempts to negate a boolean.
12 fail-not.joel: Attempts to not a num.
13 fail-or.joel: Attempts to or a boolean and a string.
14 fail-postop.joel: Attempts to use a postop on a string.
15 fail-return.joel: Attempts to return outside a function.
16 fail-scope.joel: Attempts to access a variable whose name has gone out of scope.
17 fail-table-function.joel: Attempts to pass the wrong type of table to a function.
18 fail-table-wrongtype.joel: Attempts to assign a table of the wrong type to a variable.
19 fail-table.joel: Attempts to create a mixed-column-type table.
