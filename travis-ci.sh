# install ocaml from apt
sudo apt-get install -y llvm
opam install llvm.3.6 ocamlfind
eval `opam config env`

