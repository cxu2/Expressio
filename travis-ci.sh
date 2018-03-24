# install ocaml from apt
# sudo apt-get update -qq
sudo apt-get install -y ocaml m4 llvm opam
opam init
opam install llvm ocamlfind
eval `opam config env`

