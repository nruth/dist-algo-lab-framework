#!/usr/bin/env sh
for i in $(seq 1 $1); do
  xterm -e erl -pa ebin -s stack boot -sname n$i@localhost &
done
