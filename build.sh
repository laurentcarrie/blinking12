#!/bin/sh

set -e 
set -x


exefile=auto-ftp
rm -f $exefile
rm -f *.o *.cmo *.cmx *.cma
ocamlfind ocamlopt -g -warn-error all -o $exefile auto_ftp.mli auto_ftp.ml main.ml -linkpkg -package unix,str,extlib

