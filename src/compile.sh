#!/bin/bash
NAME=`echo $1 | cut -d'.' -f1`
make clean && make && cp $1 $1.cp && ./replace.sh $1 stdlib.mk stdlib2.mk && ./express.native newfile.xp > $NAME.ll && llc $NAME.ll > $NAME.s && cc -o $NAME $NAME.s builtin.o ; mv $1.cp $1 ; rm newfile.xp
