# install ocaml from apt
brew install --with-toolchain llvm
brew install ocaml
brew install opam
opam init
opam update
opam upgrade
opam install llvm ocamlfind

