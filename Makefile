all:
	dune build src/compute_cover.exe
	dune build src/worker.exe
	dune build src/opam_bin_cache.exe
	dune build src/marracheck.exe

top:
	dune utop lib

test: all
	_build/default/src/compute_cover.exe

clean:
	dune clean

.PHONY: compute-cover
compute-cover:
	dune build src/compute_cover.exe
	LD_LIBRARY_PATH="$$LD_LIBRARY_PATH:$$(opam config var lib)/z3" dune exec src/compute_cover.exe
# Gabriel: I don't why this LD_LIBRARY_PATH is necessary, but
# otherwise execution fails with a "libz3.so not found".
