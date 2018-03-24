# install ocaml from apt
apt-get install -y ocaml m4 llvm
apt-get update
opam init
opam update
opam upgrade
opam install llvm ocamlfind
eval `opam config env`

