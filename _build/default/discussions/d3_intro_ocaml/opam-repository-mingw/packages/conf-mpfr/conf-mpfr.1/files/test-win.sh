#!/usr/bin/env dash

set -ex
cc=$(ocamlc -config | awk '/^bytecomp_c_compiler/ {for(i=2;i<=NF;i++) printf "%s ", $i}')
$cc $CFLAGS test.c -lgmp -lmpfr
