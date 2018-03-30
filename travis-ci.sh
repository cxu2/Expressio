export OPAM_VERSION=1.2.2
export OPAM_PACKAGES='ocamlfind llvm.3.6'

# install ocaml from apt
sudo apt-get install -y ocaml m4 llvm
sudo apt-get install -y opam=1.2.2
opam init
opam install llvm.3.6 ocamlfind
eval `opam config env`

