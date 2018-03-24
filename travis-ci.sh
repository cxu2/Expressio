# install ocaml from apt
apt-get update opam
apt-get install -y ocaml m4 llvm
opam init
opam update
opam upgrade
opam install llvm ocamlfind
eval `opam config env`

