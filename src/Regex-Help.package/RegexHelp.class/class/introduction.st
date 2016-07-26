pages
introduction
	"This method was automatically generated. Edit it using:"
	"RegexHelp edit: #introduction"
	^HelpTopic
		title: 'Introduction'
		contents: 
'A regular expression is a template specifying a class of strings. A
regular expression matcher is an tool that determines whether a string
belongs to a class specified by a regular expression.  This is a
common task of a user input validation code, and the use of regular
expressions can GREATLY simplify and speed up development of such
code.  As an example, here is how to verify that a string is a valid
hexadecimal number in Smalltalk notation, using this matcher package:

	aString matchesRegex: ''16r[[:xdigit:]]+''

(Coding the same ``the hard way'''' is an exercise to a curious reader).

This matcher is offered to the Smalltalk community in hope it will be
useful. It is free in terms of money, and to a large extent--in terms
of rights of use. Refer to `Boring Stuff'' section for legalese.

The ''What''s new in this release'' section describes the functionality
introduced in 1.1 release.

The `Syntax'' section explains the recognized syntax of regular
expressions.

The `Usage'' section explains matcher capabilities that go beyond what
String>>matchesRegex: method offers.

The `Implementation notes'' sections says a few words about what is
under the hood.

Happy hacking,

--Vassili Bykov 
<vassili@objectpeople.com> <vassili@magma.ca>

August 6, 1996
April 4, 1999!' readStream nextChunkText