import glob
import subprocess

# test all files matching a given pattern - e.g. "tests/test-*.joel"
def test(files):
	passes = 0

	for file in files:
		result = subprocess.run(['./toplevel.native', file],
			stdout=subprocess.PIPE, stderr=subprocess.PIPE).stdout.decode('utf-8')

		print("Running... " + file)

		if result == "Success":
			print("PASS")
			passes = passes + 1
		else:
			print("FAIL")
	return passes

# run passing test cases
def test_pass():
	print("\n")
	print("*** POSITIVE TEST CASES ***")
	print("(These should PASS)")

	files = glob.glob("tests/test-*.joel")
	if len(files) == test(files):
		return True

# run failing test cases
def test_fail():
	print("\n")
	print("*** NEGATIVE TEST CASES ***")
	print("(These should FAIL)")

	files = glob.glob("tests/fail-*.joel")
	if test(files) == 0:
		return True

# MAIN
subprocess.run(['ocamlbuild', 'toplevel.native'],
	stdout=subprocess.PIPE, stderr = subprocess.PIPE)

# Check that the tests we should pass do pass, and the ones we should fail do fail.
if test_pass() and test_fail():
	print("\nTEST SUITE PASSED\n")
else:
	print("\nTEST SUITE FAILED\n")
