#! /bin/sh
OCAMLRUNPARAM=b
export OCAMLRUNPARAM

PLACE=${1:-`uname -n`}
RESP=${2:-30001}
CA=${3:-30002}

rm -f resp.ks ca.ks

./resp -p $RESP > resp.txt 2>&1 &
pids=$!
echo Responder started
trap 'kill $pids' 0 1 2 3 6 9 15

sleep 4

./ca -p $CA > ca.txt 2>&1 &
pids="$pids $!"
echo CA started

sleep 4

echo Starting initiator

./init -r ${PLACE}:${RESP} -c ${PLACE}:${CA} ca.ks
