#! /bin/sh
OCAMLRUNPARAM=b
export OCAMLRUNPARAM

PLACE=${1:-`uname -n`}
RESP=${2:-30001}
CA=${3:-30002}

if test ! -f ca.ks -o ! -f resp.ks -o ! -f init.ks
then
  echo Key generation started
  ./mkks ca resp init 
fi

./resp -p ${RESP} resp.ks > resp.txt 2>&1 &
pids=$!
echo Responder started
trap 'kill $pids' 0 1 2 3 6 9 15

./ca -p ${CA} ca.ks > ca.txt 2>&1 &
pids="$pids $!"
echo CA started

sleep 4

echo Initiator started
./init -r ${PLACE}:${RESP} -c ${PLACE}:${CA} init.ks
