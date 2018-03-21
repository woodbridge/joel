## Joel: a quick csv processing language

### Team Members
- Mari Husain: mh3685@columbia.edu 
- Justin Woodbridge: jrw2190@columbia.edu 
- Josh Learn: jrl2196@columbia.edu 
- Nadav Gov-Ari: ng2604@columbia.edu

### Testing the parser/scanner

#### To build the toplevel driver:
- ``ocamlbuild toplevel.native``

#### To run the toplevel driver on an individual file:
- ``./toplevel.native <testfile.joel>``

#### To run the test suite (Requires python3):
- ``python3 test.py``

### Syntax We Still Need to Add
- Function calls
- Syntax (?) to declare and manipulate table (at this point, we're unsure if we should move this from the standard library to the language itself).