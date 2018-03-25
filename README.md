## Joel: a quick csv processing language

### Team Members
- Mari Husain: mh3685@columbia.edu 
- Justin Woodbridge: jrw2190@columbia.edu 
- Josh Learn: jrl2196@columbia.edu 
- Nadav Gov-Ari: ng2604@columbia.edu

### Hello World!

#### Our Simple Program
The following is our "hello world" program:
```/* Hello, world! */``
out((
  1, 2, 3;
  4, 5, 6;
  7, 8, 9
));```

The program first creates a 3x3 table containing the numbers one through nine, then prints it to stdout.

#### How to Compile the Compiler (Requires python3)
To run the script to compile the compiler, simply run ``python3 build.py``. If all went well, you should see ``SUCCESSFULLY BUILT COMPILER`` on the screen. Otherwise, you should see a ``FAIL`` message.

#### How to Hand-Compile and Run the Hello Program
- Compile the compiler (as above).
- Run the command ``./toplevel.native -c <filename> | <lli-path>``, where ``<filename>`` is the name of the Joel file you want to compile (for example, hello.joel) and ``<lli-path>`` is the path to your copy of lli (for example, ``/usr/local/opt/llvm@3.8/bin/lli-3.8``)
- You should see the output of your Joel program on the screen.

#### How to Run the Test Suite (Requires python3):
- First, locate your copy of lli. For example, mine is at ``/usr/local/opt/llvm@3.8/bin/lli-3.8``.
- Open test-hello.py in a text editor. At the top of the file, change ``PATH = '/usr/local/opt/llvm@3.8/bin/lli-3.8'`` to whatever path points to your copy of lli.
- Save test-hello.py.
- To run the test suite, run ``python3 test-hello.py``. 
- If all went well, you should see ``TEST SUITE PASSED`` on the screen. Otherwise, you should see a ``TEST SUITE FAILED`` message and the number of lines that differ between the gold standard and the actual output.


#### How the Test Script Validates Correctness
Our python script compiles hello.joel with the same commands shown in the "how to hand-compile" section. We then redirect its output (the output of the Joel program) to a file called "hello.out" and compare this output line-by-line to the gold standard, "hello.standard", using a python function that walks through the two files line-by-line, using string comparison to determine whether the lines are different. Our program tracks the number of differing lines. If no lines differ, our program passes the test; if at least one line differs, it fails. This is because the output in hello.out should match hello.standard perfectly.