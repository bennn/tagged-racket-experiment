for I in pass/*; do
  raco make -v ${I} && racket ${I} > /dev/null
done;
