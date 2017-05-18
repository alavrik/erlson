#!/bin/sh

set -ex

for i in erlc shell
do
        diff -u ../r18/erl_parse.yrl ../r18/erl_parse_$i.yrl | patch -p2 -o erl_parse_$i.yrl
done

