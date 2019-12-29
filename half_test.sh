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

function calc()
{
	echo $1 | bc -l
}

for i in {4..16}
do
	length=${i}
	block=$(( ${i} / 2 ))

	asum=0
	hsum=0
	hoffset=0 # a zero count of the results
	for j in $(seq 1 $1)
	do
		result=`sbcl --script main.lisp $length $block`
		asum=`calc "$asum + $result"`
		if ! [ "$result" -eq "0" ]; then
			hsum=`calc "$hsum + 1/${result}"`
		else
			hoffset=$(( hoffset + 1 ))
		fi
		echo -ne "  [n=$i, i=$j]: $asum + ${result}            \r"
	done
	output=`calc "scale=2; ${asum}/${1}"`
	houtput=`calc "scale=2; (${1} - ${hoffset})/${hsum}"`
	echo -ne "$1 runs with genome length $i: \tArithmetic: ${output} cycles, \t Harmonic: ${houtput} cycles. \t Instant solutions: ${hoffset}\n"
done
