DUNE_BUILD=dune build --profile=release

all:
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
	$(DUNE_BUILD) src/compute_cover.exe
