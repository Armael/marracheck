DUNE_BUILD=dune build --profile release

all:
	$(DUNE_BUILD) src/compute_cover.exe
	$(DUNE_BUILD) src/worker.exe
	$(DUNE_BUILD) src/opam_bin_cache.exe
	$(DUNE_BUILD) src/marracheck.exe

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
