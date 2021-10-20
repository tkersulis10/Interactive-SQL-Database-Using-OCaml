build:
	dune build

utop:
	OCAMLRUNPARAM=b dune utop src

cli:
	OCAMLRUNPARAM=b dune exec CLI/main.exe

zip:
	rm -f dbms.zip
	zip -r dbms.zip . -x@exclude.lst

