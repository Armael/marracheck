all:
	dune build src/compute_cover.exe
	dune build src/worker.exe

top:
	dune utop lib

test: all
	_build/default/src/compute_cover.exe
