#!/bin/bash

if ! [ "$1" -eq "$1" ] 2> /dev/null
then
	echo
	echo This test will give results for genotype lengths 4 to 16.
	echo
	echo "    ./half_test.sh <number of runs to average over>"
	echo
	exit
fi

for i in {4..16}
do
	length=${i}
	block=$(( ${i} / 2 ))

	sum=0
	for j in $(seq 1 $1)
	do
		result=`sbcl --script main.lisp $length $block`
		sum=$(( $sum + $result ))
		echo -ne "  [n=$i, i=$j]: $sum + ${result}            \r"
	done
	output=$(( sum / ${1} ))
	echo -ne "Average (of $1) for size $i: ${output} cycles\n"
done
