for I in pass/*; do
  raco make -v ${I} && racket ${I} > /dev/null
done;
echo "HEY check 'primitives.rkt' by hand"
