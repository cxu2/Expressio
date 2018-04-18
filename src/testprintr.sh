#!/bin/bash

./express.native tests/test-printr.xp > testll.ll && /usr/local/opt/llvm/bin/llc testll.ll > testll.s && cc -o testll testll.s builtin.o && ./testll 
