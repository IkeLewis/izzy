[![Build Status](https://travis-ci.org/IkeLewis/izzy.svg?branch=master)](https://travis-ci.org/IkeLewis/izzy)

IZZY
====

Izzy is a project that may enable more efficient use of a traditional
keyboard.  It could be used in call centers, development environments,
etc.  The project grew out of my own frustration with entering Emacs
commands which tend to use many control and alt (or meta) key
sequences.

Licensing
---------

Licensing has not yet been established for the project and will depend
on a variety of factors.  At this point, both commercial and
non-commercial use are strictly prohibited without my expressed
written consent and a valid product key.

Disclaimer
----------

There is no warranty for the program, to the extent permitted by
applicable law.  Except when otherwise stated in writing the copyright
holders and/or other parties provide the program “as is” without
warranty of any kind, either expressed or implied, including, but not
limited to, the implied warranties of merchantability and fitness for
a particular purpose.  The entire risk as to the quality and
performance of the program is with you.  Should the program prove
defective, you assume the cost of all necessary servicing, repair or
correction.

Limitation of Liability
-----------------------

In no event unless required by applicable law or agreed to in writing
will any copyright holder, or any other party who modifies and/or
conveys the program as permitted above, be liable to you for damages,
including any general, special, incidental or consequential damages
arising out of the use or inability to use the program (including but
not limited to loss of data or data being rendered inaccurate or
losses sustained by you or third parties or a failure of the program
to operate with any other programs), even if such holder or other
party has been advised of the possibility of such damages.

How Izzy Works
--------------

The flow diagram below shows how izzy processes input on my prototype
system.

PS2 Device -\><br>
Linux Raw PS/2 Driver -\><br>
izzy -\><br>
Emacs, VIM, Auto Hot Key (AHK), etc<br>

After a user presses a key on a PS/2 keyboard, an input event is
written to a device file by the Linux kernel.  Izzy reads the input
event from the raw device's file descriptor, and then dispatches it to
handlers, which may in turn transform it and then forward the
resulting input event to other applications such as Emacs, VIM, AHK,
etc.

Towards USB Support
-------------------

Currently, there are at least three different approaches to
implementing USB support:

1. Modify a USB HID Boot Protocol keyboard driver [4](#4).

2. Modify a generic USB driver [5](#5).

3. Implement a driver on top of the raw USB driver [6](#6).

Each of the above approaches has it's pros and cons; the first
approach requires the least implementation effort but won't work with
keyboards and keypads that don't support the USB boot protocol.  The
second approach would allow the use of keyboards and keypads that
don't support the USB boot protocol but would require removing code
that isn't for USB keyboards and keypads (e.g. code for mice, pens,
touchscreens, touch pads, system controls, consumer controls, wireless
radio controls, and system-multi axis devices).  The third approach
would likely result in the highest quality driver, but could require
considerable implementation effort.  Since the vast majority of
keyboards support the USB Boot Protocol, it is my opinion that the
first approach should be taken.

Windows Users
-------------

Currently, izzy is designed to run only on Linux.  However, it is
still possible (in theory) to use izzy with Windows.  One potential
solution is to connect a Raspberry Pi (running izzy) to an Arduino
(configured as a USB input forwarding device).  (This approach is
likely overly complex; hopefully, it is also possible to simply use
just a Raspberry Pi.)

Keyboard<br>
\|<br>
Raspberry Pi (running izzy)<br>
\|<br>
Arduino (configured as a USB input forwarding device)<br>
\|<br>
Windows Computer<br>

One advantage of using izzy in this way is that no software needs to
be installed on the Windows computer.  A downside of this approach is
that access to the Raspberry Pi is required to update izzy or to add a
new handler, which may be less convenient.

Ideas for Improving Code Quality
--------------------------------

* Add clarifying comments to the code.

* Perform extensive pseudo-randomized testing.

* Consider adding additional type checking.

* Consider using a more robust typing system.

* Consider specifying some requirements of izzy in higher order logic.

Early Design Goals
------------------

* Simplicity

* Modularity

* Allow input handlers to be added dynamically in languages other than
scheme

* Allow ordinary keys to be used as modifiers

* Allow users to leverage existing input handling software like Auto Hot
Key (AHK)

* Embrace the Linux/UNIX design philosophy

Notation
--------

\<key\>_ : the key has been pressed down<br>
\<key\>  : the key has been pressed and released<br>
\<key\>* : the key has been released<br>

Ordinary Keys May be Used as Modifiers
--------------------------------------

On a traditional keyboard, with izzy it is possible to use the 'A' key
as both a control modifier and a non-modifier; for example, when the
'A' key is held and another key is pressed and then released, the 'A'
key behaves as a control modifier. So A_ P A* would be interpreted as
ctrl+P.  When the 'A' key is quickly pressed and then released
however, it behaves as an ordinary key. If the 'A' key is held down
for a long period of time and then released, nothing will be entered.
One potential drawback of this approach is that the 'A' key can't be
held down to enter multiple A's.  Instead, a simple key sequence (as
is used in Emacs) could be used to repeat a key; for example alt_ 1 0
0 alt* A, could be used to quickly enter 100 A's.

Installation
------------
TODO

Launching Izzy
--------------

```
$ izzy.scm [-allow-stale-input] <input-device-path> <unix-socket-path>
```

Start an izzy server for the specified input device listening for
connections on `unix-socket-path'.

Reviewers
---------

This project has been kindly reviewed at least in part by Dan S. of
DXC and David L. of Carbonite.

Dedication
----------

The project is inspired by and dedicated to my late aunt Izzy Sanders
who let me use her computer before I had one of my own and who
encouraged me to pursue a career in IT as a youth.


<a name="references"></a>References
-----------------------------------

1. <a name="1"></a> Device Class Definition for HID 1.11
https://www.usb.org/document-library/device-class-definition-hid-111

2. <a name="2"></a> Human Interface Devices (HID)
https://www.kernel.org/doc/html/latest/hid/index.html

3. <a name="3"></a> The Linux Input Documentation
https://www.kernel.org/doc/html/latest/input/index.html

4. <a name="4"></a> A Linux USB HID Boot Protocol Driver (usbkbd.c)
https://git.kernel.org/pub/scm/linux/kernel/git/stable/linux.git/tree/drivers/hid/usbhid/usbkbd.c?h=v5.3.1

5. <a name="5"></a> A Linux Generic HID Input Driver (hid-input.c)
https://git.kernel.org/pub/scm/linux/kernel/git/stable/linux.git/tree/drivers/hid/hid-input.c?h=v5.3.1

6. <a name="6"></a> A Linux Raw HID Driver (hidraw.c)
https://git.kernel.org/pub/scm/linux/kernel/git/stable/linux.git/tree/drivers/hid/hidraw.c?h=v5.3.1

7. <a name="7"></a> A Linux Raw Driver for Serial Input Devices
https://git.kernel.org/pub/scm/linux/kernel/git/stable/linux.git/tree/drivers/input/serio/serio_raw.c?h=v5.3.1