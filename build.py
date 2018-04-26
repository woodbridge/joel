import subprocess
import os

# MAIN

try:
  run = subprocess.run(['ocamlbuild', '-use-ocamlfind', '-pkgs', 'llvm,llvm.analysis', '-cflags', '-w,+a-4', 'toplevel.native'],
  	stdout=subprocess.PIPE, stderr = subprocess.PIPE, check=True)

  stdout = run.stdout.decode().split('\n')
  for line in stdout: print (line)

  print('SUCCESSFULLY BUILT COMPILER')
except subprocess.CalledProcessError as e:
  print("FAILED TO BUILD COMPILER")

  stderr = e.stderr.decode().split('\n')
  for line in stderr: print(line)

  stdout =  e.stdout.decode().split('\n')
  for line in stdout: print(line)