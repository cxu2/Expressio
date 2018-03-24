# install ocaml from apt
wget https://raw.github.com/ocaml/opam/master/shell/opam_installer.sh -O - | sh -s /usr/local/bin/
sudo apt-get update
sudo apt-get install -y ocaml m4 llvm
opam init
opam update
opam upgrade
opam install llvm ocamlfind
eval `opam config env`

