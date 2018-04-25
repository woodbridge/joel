## Joel: a quick csv processing language

### Team Members
- Mari Husain: mh3685@columbia.edu 
- Justin Woodbridge: jrw2190@columbia.edu 
- Josh Learn: jrl2196@columbia.edu 
- Nadav Gov-Ari: ng2604@columbia.edu

### Extended Test Suite
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
- run ``./toplevel.native test.joel > test.ll``
- run ``llc test.ll``
- run ``gcc -c test.s -o test.o``
- run ``g++ test.o -o test``
- You should now have an executable named ``test``.

#### To Run the Test Suite:
- First, locate information about your copy of LLI. For example, running ``lli --version`` reveals that my LLI is running with LLVM 6.0.0. If your LLI runs something other than LLVM 6.0.0 or LLVM 3.8.1, please locate a copy of LLI on your system that does run it (ex. ``'/usr/local/opt/llvm@3.8/bin/lli-3.8'``)
- If your LLI does not run the correct LLVM version, open extended-testsuite.py in a text editor. Go to the top of the file and change ``LLI = "lli"`` to whatever path points to your copy of lli.
- Save extended-testsuite.py.
- To run the test suite, run ``python3 extended-testsuite.py``. 
- If all went well, you should see ``TEST SUITE PASSED`` on the screen. Otherwise, you should see a ``TEST SUITE FAILED`` message and the test(s) that failed and the lines that differ between the gold standard and the actual output.


#### The Tests in Our Test Suite

##### Positive Tests
1. ``test-and-or.joel``: Tests boolean and (&) and or (|) operators.
2. ``test-while.joel``: Tests the while loop. 
3. ``test-incr-assign.joel``: Tests the increment-assignment (in this case, the +=) operator.
4. ``test-string.joel``: Tests string literals.
5. ``test-scope.joel``: Tests scoping rules - both global and local variables, which may be declared anywhere within the scope (not just at the beginning of the scope) and re-defined.
6. ``test-binop.joel``: Tests binary operations (+, -, *, /).
7. ``test-if.joel``: Tests if statements.

##### Negative Tests
1. ``fail-assign.joel``: Attempts to assign a string to a variable of num type. Tests InvalidAssignment error.
2. ``fail-scope.joel``: Attempts to access a variable whose name has gone out of scope. Tests UndefinedId error.
3. ``fail-multiplication.joel``: Attempts to multiply two strings. Tests InvalidBinaryOperation error.
