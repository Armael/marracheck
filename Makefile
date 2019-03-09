all:
	dune build src/test.exe
	dune build src/worker.exe

top:
	dune utop lib

test: all
	_build/default/src/test.exe
