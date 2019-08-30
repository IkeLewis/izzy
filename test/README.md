Creating Canned Input
=====================

The following example shows how to capture input from a USB keyboard
and how to save it as canned input.

```
$ cd ~/izzy/test
$ gcc -o read_ie ../src/read_ie.c
$ chmod +x read_ie
```
Note: the device path for your keyboard will likely be different.

```
$ rm -f usb-canned-input && sudo ./read_ie /dev/input/event12 usb-canned-input
```
Type some characters on the keyboard; when you've finished type ctrl-c.
Optionally, run

```
$ rm read_ie
```
To remove the read_ie executable.