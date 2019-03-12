all:
	dune build src/compute_cover.exe
	dune build src/worker.exe
	dune build src/opam_bin_cache.exe
	dune build src/marracheck.exe

top:
	dune utop lib

test: all
	_build/default/src/compute_cover.exe
