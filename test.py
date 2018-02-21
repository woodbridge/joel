import glob
import subprocess


# test all files matching a given pattern - e.g. "tests/test-*.joel"
def test(filestring):
	files = glob.glob(filestring)
	for file in files:
		result = subprocess.run(['./toplevel.native', file], 
			stdout=subprocess.PIPE, stderr=subprocess.PIPE).stdout.decode('utf-8')

		print("Running... " + file)

		if result == "Success":
			print("PASS")
		else:
			print("FAIL")

# run passing test cases
def test_pass():
	print("\n")
	print("*** POSITIVE TEST CASES ***")
	print("(These should PASS)")

	test("tests/test-*.joel")
	
# run failing test cases
def test_fail():
	print("\n")
	print("*** NEGATIVE TEST CASES ***")
	print("(These should FAIL)")

	test("tests/fail-*.joel")

# MAIN
subprocess.run(['ocamlbuild', 'toplevel.native'], 
	stdout=subprocess.PIPE, stderr = subprocess.PIPE)
test_pass()
test_fail()