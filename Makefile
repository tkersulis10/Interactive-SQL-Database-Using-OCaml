.PHONY: test check

build:
	dune build

utop:
	OCAMLRUNPARAM=b dune utop src

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

cli:
	OCAMLRUNPARAM=b dune exec CLI/main.exe

zip:
	rm -f dbms.zip
	zip -r dbms.zip . -x@exclude.lst

