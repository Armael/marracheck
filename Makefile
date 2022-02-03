DUNE_BUILD=dune build

all:
	$(DUNE_BUILD) src/opam_bin_cache.exe
	$(DUNE_BUILD) src/marracheck.exe
	$(DUNE_BUILD) src/compute_cover.exe

top:
	dune utop lib

test: all
	_build/default/src/compute_cover.exe

clean:
	dune clean
