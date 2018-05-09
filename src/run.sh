make clean && make
./express.native $1 > run.ll
/usr/local/opt/llvm/bin/llc run.ll > run.s
cc -o run run.s dfa.o builtin.o
./run

