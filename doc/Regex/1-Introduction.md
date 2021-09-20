# Introduction
a regular expression is a template specifying a class of strings. a
regular expression matcher is an tool that determines whether a string
belongs to a class specified by a regular expression.  this is a
common task of a user input validation code, and the use of regular
expressions can greatly simplify and speed up development of such
code.  as an example, here is how to verify that a string is a valid
hexadecimal number in smalltalk notation, using this matcher package:

	astring matchesregex: '16r[[:xdigit:]]+'

(coding the same 'the hard way' is an exercise to a curious reader).

this matcher is offered to the smalltalk community in hope it will be
useful. it is free in terms of money, and to a large extent — in terms
of rights of use. refer to 'boring stuff' section for legalese.

the 'what's new in this release' section describes the functionality
introduced in 1.1 release.

the 'syntax' section explains the recognized syntax of regular
expressions.

the 'usage' section explains matcher capabilities that go beyond what
`String>>matchesregex:` method offers.

the 'implementation notes' sections says a few words about what is
under the hood.

happy hacking,

— vassili bykov
<vassili@objectpeople.com> <vassili@magma.ca>

august 6, 1996
april 4, 1999!

