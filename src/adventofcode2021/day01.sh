#!/usr/bin/env bash

j=0
s=-1
for i in $(cat $1)
do
    if [ $i -gt $j ]
    then
        s=$(($s+1))
        echo "$j - $i"
    fi
    j=$i
done
echo $s
