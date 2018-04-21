export OPAM_VERSION=1.2.2
export OPAM_PACKAGES='ocamlfind llvm.3.6'

# install ocaml from apt
yes Yes | wget https://raw.github.com/ocaml/opam/master/shell/opam_installer.sh -O - | sh -s /usr/local/bin/
sudo apt-get update
sudo apt-get install -y ocaml m4 llvm
opam init
opam install llvm.3.6 ocamlfind
eval `opam config env`

