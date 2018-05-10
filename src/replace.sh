# David Han         <dth2126@columbia.edu>
#!/bin/sh

#./replace.sh FILENAME stdlib.mk stdlib2.mk
touch newfile.xp
grep = $1 
t=$?
once=0
while read p; do
	echo $p | grep "main" 1> /dev/null
	if [ $? -eq 0 ]; then
		echo $p >> newfile.xp
  		cat $2 >> newfile.xp
  	else
		echo $p | grep "=" 1> /dev/null
		if [ $? -eq 0 ] && [ $once -eq 0 ]; then
			cat $3 >> newfile.xp
			echo $p >> newfile.xp
			once=1
		else
		echo $p >> newfile.xp
		fi
    fi
done <$1
