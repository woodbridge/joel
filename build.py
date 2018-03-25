import subprocess
import os

# MAIN
run = subprocess.run(['ocamlbuild', '-use-ocamlfind', '-pkgs', 'llvm,llvm.analysis', '-cflags', '-w,+a-4', 'toplevel.native'],
	stdout=subprocess.PIPE, stderr = subprocess.PIPE)

out = run.stdout.decode('utf-8')
err = run.stderr.decode('utf-8')

if len(out) == 0 and len(err) == 0:
	print("SUCCESSFULLY BUILT COMPILER")
else:
	print("FAIL")
	print(out)
	print(err)

