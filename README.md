IZZY
====

Izzy is a project that may enable more efficient use of a traditional
keyboard.  It could be used in call centers, development enviroments,
etc.  The project grew out of my own frustration (in 2016, I believe)
with entering Emacs commands which tend to use many control and alt
(or meta) key sequences.

Licensing
---------

Licensing has not yet been established for the project and will depend
on a variety of factors; most likely, it will be free for non-commercial use.

Reviewers
---------

This project has been kindly reviewed by __________.

Notation
--------

<key>_ : the key has been pressed down
<key>  : the key has been pressed and released
<key>* : the key has been released

An Example of How It Works
--------------------------

On a traditional keyboard, it is possible to use the 'A' key as both a
control modifier and a non-modifier; when the 'A' key is held and
another key is pressed and then released, the 'A' key behaves as a
control modifer. So A_ P A* would be interpreted as ctrl+P.  When the
'A' key is quickly pressed and then released however, it behaves as an
ordinary key. If the 'A' key is held down for a long period of time
and then released, nothing will be entered.  One potential drawback of
this approach is that the 'A' key can't be held down to enter multiple
A's.

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

Features
--------

Currently logging is supported to the console and support for auditing
may be added, depending on interest.

Dedication
----------

The project is inspired by and dedicated to my late aunt Izzy Sanders
who let me use her computer before I had one of my own and who
encouraged me to pursue a career in IT as a youth.

About Me
--------

Although many signfificant people in my life passed away long ago,
some days it feels like they just passed. Currently I'm struggling
with anxiety and depression, and I'm seeking treatment.  Thoughts and
prayers are greatly appreciated.
