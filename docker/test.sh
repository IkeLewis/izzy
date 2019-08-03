#!/bin/bash -ex

# First test: does it compile?
cd /root/izzy-test
if [ ! -d lib ]; then
    mkdir lib
fi
# Does the C code compile?
cd /root/izzy-test/izzy/src
make clean
make
# Does the guile code compile?
cd /root/izzy-test/izzy/scheme
izzy/izzy.scm /root/izzy-test/kie-pipe





