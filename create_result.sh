#!/bin/bash
if [ $# -gt 0 ]
then
	make build
	clang -c bin/rt-support.c -o rt-support.o
	_build/default/bin/microcc.exe "$@"
	llc -filetype=obj a.bc
	clang a.o rt-support.o -o a.out &&
	./a.out > ${1%.*}.out
	rm a.bc a.o a.out
	f="$(cat ${1%.*}.out)"
	echo -e "\n"$f
else
	echo -e "Wrong amount of arguments\nUsage: create_result.sh \e[4m\e[5mfilename\e[25m\e[24m"
fi
