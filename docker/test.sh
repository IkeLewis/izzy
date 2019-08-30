#!/bin/bash -ex

# Does the C code compile?
cd /root/izzy
if [ ! -d lib ]; then
    mkdir lib
fi
# Does the C code compile?
cd /root/izzy/src
make clean
make
make install
# Does the guile code compile and pass unit testing?
cd /root/izzy/scheme
izzy/izzy.scm -allow-stale-input /root/izzy/test/usb-canned-input /root/izzy/test/unix-server-socket
