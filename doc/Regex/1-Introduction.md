# Introduction

A regular expression is a template specifying a class of strings. A regular expression matcher is a tool that determines whether a string belongs to a class specified by a regular expression. This is a common task of a user input validation code, and the use of regular expressions can greatly simplify and speed up the development of such code. As an example, here is how to verify that a string is a valid hexadecimal number in Smalltalk notation, using this matcher package:

```st
	aString matchesRegex: '16r[[:xdigit:]]+'
```

(coding the same 'the hard way' is an exercise for a curious reader).

This matcher is offered to the Smalltalk community in the hope it will be useful. It is free in terms of money, and to a large extent — in terms of rights of use. Refer to 'boring stuff' section for legalese.

The 'syntax' section explains the recognized syntax of regular expressions.

The 'usage' section explains matcher capabilities that go beyond what `String>>#matchesRegex:` method offers.

The 'implementation notes' section says a few words about what is under the hood.

Happy hacking,

— vassili bykov
<vassili@magma.ca>

August 6, 1996
April 4, 1999!
