autoreconf -i

# Although we don't use automake, we have to run this in order to
# create or regenerate install-sh (PR automake/546).
automake --add-missing >/dev/null 2>&1 ||:

./configure "$@"
