# install ocaml from apt
sudo apt-get install -y ocaml m4 llvm opam
sudo apt-get update
opam init
opam install llvm ocamlfind
eval `opam config env`

