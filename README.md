# Marracheck

*Note: this is at the moment still a WIP project that is developed erratically
when the authors find time to do so.*

Design notes (in French): [marracheck-design-notes](https://gitlab.com/gasche/marracheck-design-notes).

## Build

### As an opam package

To just obtain the `marracheck` binary, pinning the repository using opam should work.

``` sh
opam pin add marracheck.dev https://github.com/Armael/marracheck.git
```

Afterwards, **one still needs to install separately the external solver**, see
instructions below.


### Local development copy

After cloning this repository, run:

```sh
git submodule update --init --recursive # fetch the vendored copy of the opam libs that we use
opam install --deps-only .  # or, to create a fresh local switch: opam switch create --deps-only . ocaml-base-compiler.4.09.1
dune build
dune exec -- src/marracheck.exe --help
# (right now the only interesting marracheck command is `run`)
```

Then, **install the external solver**; see instructions below.

## External solver

Right now marracheck needs an extra external solver to be installed and
available in the `$PATH`. This is obviously not ideal and will be improved in
the future.

Instructions:

``` sh
git clone https://github.com/sbjoshi/Open-WBO-Inc
cd Open-WBO-Inc
make r
```

This will build an `open-wbo-inc_release` binary, that you need to copy
somewhere where it can be found through `$PATH`.

## Running marracheck

Finally, marracheck requires an opam repository available locally (for example
with `git clone https://github.com/ocaml/opam-repository`). Then marracheck
can be run with:

```sh
dune exec -- src/marracheck.exe run \
  /path/to/opam-repository \
  /path/to/marracheck-state \
  ocaml-base-compiler.4.13.1
```

This will attempt to compile all versions of all packages compatible with ocaml
4.13.1.  The `/path/to/marracheck-state` directory will contain a binary
`cache` directory of the packages, a standard `opamroot`, and a `switches`
directory that tracks the progress of marracheck.  The behavior of opam invoked
by marracheck can be configured with environment variables (see
`opam env --help` for a full list.)
