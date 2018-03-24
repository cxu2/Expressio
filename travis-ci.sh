# install ocaml from apt
add-apt-repository ppa:avsm/ppa
sudo apt-get update
sudo apt-get install -y ocaml m4 llvm opam
opam init
opam install llvm ocamlfind
eval `opam config env`

