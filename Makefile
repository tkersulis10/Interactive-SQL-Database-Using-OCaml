build:
	dune build

utop:
	OCAMLRUNPARAM=b dune utop src

cli:
	OCAMLRUNPARAM=b dune exec CLI/main.exe


