SASL
====
I forked this from dpiponi/SASL because SKI machines fascinate me.
I converted it to C++ in a big untested step and now I'm fixing it.

This code uses the visitor pattern.

Force in favor of the visitor pattern: Some code has to treat members of a class 
heirarchy uniformly but the code doesn't properly belong in the classes. Instead
of RTTI you can make visitors.

