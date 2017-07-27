NUM_FAIL=0
NUM_PASS=0
for I in fail/*.rkt; do
  if raco make -v ${I} >& /dev/null ; then
    echo "FAIL ${I}";
    NUM_FAIL=$((${NUM_FAIL} + 1))
  else
    NUM_PASS=$((${NUM_PASS} + 1))
  fi
done
echo "${NUM_PASS}/$((${NUM_FAIL} + ${NUM_PASS})) tests passed"
