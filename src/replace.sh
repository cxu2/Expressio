#!/bin/sh
touch newfile.xp
while read p; do
	echo $p | grep "main" 2> /dev/null
	if [ $? -eq 0 ]; then
  		cat $2 >> newfile.xp
  		echo $p >> newfile.xp
  	else
        echo $p >> newfile.xp
    fi
done <$1
#b=$(basename "$1")
#mv newfile.xp $b