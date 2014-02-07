lisp-in-small-pieces
====================

I'm working my way through "Lisp in Small Pieces", by Christian Queinnec.
The book discusses the implementation of Lisps, and develops interpreters
and compilers demonstrating various techniques and features of Lisps.
It's a lot of fun to follow along.

This repository holds the code I've written while studying the book. I skip a few
chapters (in the sense of not writing any code; I still read them!), and I diverge
and divert from the book according to taste.

All the code is in Scheme, and much of it uses SISC-specific libraries
(`generic-procedures`, `oo`, `type-system`). It would
be adaptable to other Schemes, I imagine, since they tend to have analogues of
those libraries. But why wouldn't you be writing your own code instead.
