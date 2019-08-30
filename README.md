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
on a variety of factors; most likely, it will be free for
non-commercial use.

How Izzy Works
--------------

The flow diagram below shows how izzy processes input on my prototype
system.

PS2 Device -\>
Linux Raw PS/2 Driver -\>
izzy -\>
Emacs, VIM, Auto Hot Key (AHK), etc

After a user presses a key on a PS/2 keyboard, an input event is
written to a device file by the Linux kernel.  Izzy reads the input
event from the raw device's file descriptor, and then dispatches it to
handlers, which may in turn transform it and then forward the
resulting input event to other applications such as Emacs, VIM, AHK,
etc.

Windows Users
-------------

Currently, izzy is designed to run only on Linux.  However, it is
still possible (in theory) to use izzy with Windows.  One potential
solution is to connect a Raspberry Pi (running izzy) to an Arduino
(configured as a USB input forwarding device).  (This approach is
likely overly complex; hopefully, it is also possible to simply use
just a Raspberry Pi.)

Keyboard
\|
Raspberry Pi (running izzy)
\|
Arduino (configured as a USB input forwarding device)
\|
Windows Computer

One advantage of using izzy in this way is that no software needs to
be installed on the Windows computer.  A downside of this approach is
that access to the Raspberry Pi is required to update izzy or to add a
new handler, which may be less convenient.

Ideas for Improving Code Quality
--------------------------------

Add clarifying comments to the code.

Perform extensive pseudo-randomized testing.

Consider adding additional type checking.

Consider using a more robust typing system.

Consider specifying some requirements of izzy in higher order logic.

Early Design Goals
------------------

Simplicity

Modularity

Allow input handlers to be added dynamically in languages other than
scheme

Allow ordinary keys to be used as modifiers

Allow users to leverage existing input handling software like Auto Hot
Key (AHK)

Embrace the Linux/UNIX design philosophy

Notation
--------

\<key\>_ : the key has been pressed down
\<key\>  : the key has been pressed and released
\<key\>* : the key has been released

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
izzy.scm [-allow-stale-input] <input-device-path> <unix-socket-path>

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