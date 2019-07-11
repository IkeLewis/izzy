Meta - MTA, Control - CTL, Shift - SHF, Alt - ALT, Hyper - HYP, Super - SUP
Caps Lock - CPL

A - "a" used as a modifier
a - "a" used as a letter


<b-prevn> = <bi1> | <bi2> | ... | <bin>
<moi> = <mo1>_ | <mo2>_ | ... | <mon>

For a given key, a keystroke consists of a key press followed by a key
release.  While a key is fully pressed and held down it is said to be
in the down state.  While a key is fully released it is said to be in
the up state.

State variables: modifiers, translate-repeats, K

Let K be the current sequence of kernel-input events.  Let <kie> be the
current kernel-input event.

REPEAT
======

If K matches <mo1>_ <mo2>_ ... <mon>_ <bi1>_, then add <kie> to
both K and the list of modifiers.

Else if K matches <alt>_ <alt>^ <alt>_, then add <kie> to K.

Else if K matches <mo1>_ <mo2>_ ... <mon>_ <bi1>_ <bi2>_, then add <kie> to K.  

Otherwise, ignore the repeat.

PRESS
=====

Append <kie> to K.

If K matches <alt>_ <alt>^ <bi1>_, set K to <bi1>_.

RELEASE
=======

(1) If it's a stray release (K matches (~<kie>_)*), ignore <kie>.
Read another kernel-input event.

(2) Else if K matches <alt>_ <alt>^, read another kernel-input
event.

(4) Else if <kie> is a modifier release and K matches <m1>_ <m2>_
... <mn>_, then remove its corresponding press from the list of
modifiers and from K.  Read another kernel-input event.

(5) Otherwise, append <kie> to K.

MATCH:

If a complete input event has been obtained (K matches one of the
patterns below)

; reset K
; disable repeats for <bi2>

<ie> = <m1>_ <m2>_ ... <mn>_ <o1>_ 
<ie> = <m1>_ <m2>_ ... <mn>_ <bi1>_ <o1>_ 

<ie> = <alt>_ <alt>^ <alt>_ <alt>^ 
<ie> = <m1>_ <m2>_ ... <mn>_ <bi1>_ <bi2>_ (<b-prev2> | <mi>)^ 
<ie> = <m1>_ <m2>_ ... <mn>_ <bi1>_ (<b-prev1> | <mi>)^

<ie> = <m1>_ <m2>_ ... <mn>_ <bi1>t ; remove <bi1>t from K
<ie> = <m1>_ <m2>_ ... <mn>_ <bi1>_ <bi2>_ <bi2>t ; remove <bi1>t from K

<ie> = <m1>_ <m2>_ ... <mn>_ <bi1>_ <bi2>_ <bi3>_ ; disable repeats for <bi2>


a_ t_ t^ -> C-t
a_ t_ a^ -> at (remove t from the list of down-keys)
;_ x_ ;^ -> C-x (remove x from the list of down-keys)
;_ x_ x^ -> C-x
