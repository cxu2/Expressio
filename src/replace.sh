#!/bin/sh

#./replace.sh FILENAME stdlib.mk stdlib2.mk
touch newfile.xp
grep = $1 
t=$?
echo $t
once=0
while read p; do
	echo $p | grep "main" 2> /dev/null
	if [ $? -eq 0 ]; then
		echo $p >> newfile.xp
  		cat $2 >> newfile.xp
  	else
		echo $p | grep "=" 2> /dev/null
		if [ $? -eq 0 ] && [ $once -eq 0 ]; then
			cat $3 >> newfile.xp
			echo $p >> newfile.xp
			once=1
		else
		echo $p >> newfile.xp
		fi
    fi
done <$1
#b=$(basename "$1")
#mv newfile.xp $b