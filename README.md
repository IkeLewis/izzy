IZZY
====

Izzy is a project that may enable more efficient use of a traditional
keyboard.  It could be used in call centers, development environment,
etc.  The project grew out of my own frustration (in 2016, I believe)
with entering Emacs commands which tend to use many control and alt
(or meta) key sequences.

Licensing
---------

Licensing has not yet been established for the project and will depend
on a variety of factors; most likely, it will be free for
non-commercial use.

Izzy is Complementary to Auto Hot Key (AHK)!
--------------------------------------------

There has been some confusion about how izzy works.  One of the early
design goals of izzy is to leverage existing mature input handling
software like AHK.  Izzy operates at a lower level than AHK and is
thus entirely complementary to it.  The flow diagram below shows how
input can be processed on my prototype system:

PS2 Device -\>
Raw PS/2 Driver -\>
izzy -\>
AHK, Emacs, vi, etc

Reviewers
---------

This project has been kindly reviewed at least in part by Dan S. of
DXC and David L. of Carbonite.

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

How Input Events are Currently Handled
--------------------------------------

The current process flow for input events is quite simple:

1) an input event is read from the keyboard using libread_ie.
2) the input event is transformed via scheme
3) the transformed input event is injected via a call to libuinject.

Note, I haven't looked at the above code in some time, so the 3 steps
may not be 100% accurate.

Current Design
--------------

Unfortunately, the current design is suboptimal; better designs exist
and may be implemented; whether or not such designs will be
implemented depends on the interests of potential stakeholders and the
open source community, of course.

Towards a better design
-----------------------

Each handler connects to the dispatcher.
After the handler authenticates itself to the dispatcher, it is
appended to the list of handlers.
Once an input event is received it is placed in the queue.
The dispatcher removes the first input event from the queue and sends
it to each handler on the list.
Each handler can then choose to ignore, observe, or handle the input
event.

Key benefits of this approach:
Multiple applications could receive/respond to the same input event,
if desired.
Handlers can be written in any programming language, not just scheme.
Handlers are required to authenticate over an encrypted connection.
New handlers can be added on the fly without disrupting existing
production code.
Basic logging/auditing could be implemented via a handler.

Higher Level Features
---------------------
Support for keymaps and macros could be implemented via separate
modules.

Dedication
----------

The project is inspired by and dedicated to my late aunt Izzy Sanders
who let me use her computer before I had one of my own and who
encouraged me to pursue a career in IT as a youth.

About Me
--------

Although many significant people in my life passed away long ago,
some days it feels like they just passed. Currently I'm struggling
with anxiety and depression, and I'm seeking treatment.  Thoughts and
prayers are greatly appreciated.
