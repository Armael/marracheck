all:
	dune build src/test.exe

top:
	dune utop lib

test:
	dune build src/test.exe
	_build/default/src/test.exe
