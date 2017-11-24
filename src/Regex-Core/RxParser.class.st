"
-- Regular Expression Matcher v 1.1 (C) 1996, 1999 Vassili Bykov
--
The regular expression parser. Translates a regular expression read from a stream into a parse tree. ('accessing' protocol). The tree can later be passed to a matcher initialization method.  All other classes in this category implement the tree. Refer to their comments for any details.

Instance variables:
	input		<Stream> A stream with the regular expression being parsed.
	lookahead	<Character>
"
Class {
	#name : #RxParser,
	#superclass : #Object,
	#instVars : [
		'input',
		'lookahead'
	],
	#classVars : [
		'BackslashConstants',
		'BackslashSpecials'
	],
	#category : #'Regex-Core'
}

{ #category : #DOCUMENTATION }
RxParser class >> a: x introduction: xx [ 
" 
A regular expression is a template specifying a class of strings. A
regular expression matcher is an tool that determines whether a string
belongs to a class specified by a regular expression.  This is a
common task of a user input validation code, and the use of regular
expressions can GREATLY simplify and speed up development of such
code.  As an example, here is how to verify that a string is a valid
hexadecimal number in Smalltalk notation, using this matcher package:

	aString matchesRegex: '16r[[:xdigit:]]+'

(Coding the same ``the hard way'' is an exercise to a curious reader).

This matcher is offered to the Smalltalk community in hope it will be
useful. It is free in terms of money, and to a large extent--in terms
of rights of use. Refer to `Boring Stuff' section for legalese.

The 'What's new in this release' section describes the functionality
introduced in 1.1 release.

The `Syntax' section explains the recognized syntax of regular
expressions.

The `Usage' section explains matcher capabilities that go beyond what
String>>matchesRegex: method offers.

The `Implementation notes' sections says a few words about what is
under the hood.

Happy hacking,

--Vassili Bykov 
<vassili@objectpeople.com> <vassili@magma.ca>

August 6, 1996
April 4, 1999
"

	self error: 'comment only'
]

{ #category : #DOCUMENTATION }
RxParser class >> b: x whatsNewInThisRelease: xx [
"
VERSION 1.3.1 (September 2008)
1. Updated documentation of character classes, making clear the problems of locale - an area for future improvement

VERSION 1.3 (September 2008)
1. \w now matches underscore as well as alphanumerics, in line with most other regex libraries (and our documentation!).  
2. \W rejects underscore as well as alphanumerics
3. added tests for this at end of testSuite
4. updated documentation and added note to old incorrect comments in version 1.1 below

VERSION 1.2.3 (November 2007)

1. Regexs with ^ or $ applied to copy empty strings caused infinite loops, e.g. ('' copyWithRegex: '^.*$' matchesReplacedWith: 'foo'). Applied a similar correction to that from version 1.1c, to #copyStream:to:(replacingMatchesWith:|translatingMatchesUsing:).
2. Extended RxParser testing to run each test for #copy:translatingMatchesUsing: as well as #search:.
3. Corrected #testSuite test that a dot does not match a null, which was passing by luck with Smalltalk code in a literal array.
4. Added test to end of test suite for fix 1 above.

VERSION 1.2.2 (November 2006)

There was no way to specify a backslash in a character set. Now [\\] is accepted.

VERSION 1.2.1	(August 2006)

1. Support for returning all ranges (startIndex to: stopIndex) matching a regex - #allRangesOfRegexMatches:, #matchingRangesIn:
2. Added hint to usage documentation on how to get more information about matches when enumerating
3. Syntax description of dot corrected: matches anything but NUL since 1.1a

VERSION 1.2	(May 2006)

Fixed case-insensitive search for character sets.

VERSION 1.1c	(December 2004)

Fixed the issue with #matchesOnStream:do: which caused infinite loops for matches 
that matched empty strings.

VERSION 1.1b	(November 2001)

Changes valueNowOrOnUnwindDo: to ensure:, plus incorporates some earlier fixes.

VERSION 1.1a	(May 2001)

1. Support for keeping track of multiple subexpressions.
2. Dot (.) matches anything but NUL character, as it should per POSIX spec.
3. Some bug fixes.

VERSION 1.1	(October 1999)

Regular expression syntax corrections and enhancements:

1. Backslash escapes similar to those in Perl are allowed in patterns:

	\w	any word constituent character (equivalent to [a-zA-Z0-9_]) *** underscore only since 1.3 ***
	\W	any character but a word constituent (equivalent to [^a-xA-Z0-9_] *** underscore only since 1.3 ***
	\d	a digit (same as [0-9])
	\D	anything but a digit
	\s 	a whitespace character
	\S	anything but a whitespace character
	\b	an empty string at a word boundary
	\B	an empty string not at a word boundary
	\<	an empty string at the beginning of a word
	\>	an empty string at the end of a word

For example, '\w+' is now a valid expression matching any word.

2. The following backslash escapes are also allowed in character sets
(between square brackets):

	\w, \W, \d, \D, \s, and \S.

3. The following grep(1)-compatible named character classes are
recognized in character sets as well:

	[:alnum:]
	[:alpha:]
	[:cntrl:]
	[:digit:]
	[:graph:]
	[:lower:]
	[:print:]
	[:punct:]
	[:space:]
	[:upper:]
	[:xdigit:]

For example, the following patterns are equivalent:

	'[[:alnum:]_]+' '\w+'  '[\w]+' '[a-zA-Z0-9_]+' *** underscore only since 1.3 ***

4. Some non-printable characters can be represented in regular
expressions using a common backslash notation:

	\t	tab (Character tab)
	\n	newline (Character lf)
	\r	carriage return (Character cr)
	\f	form feed (Character newPage)
	\e	escape (Character esc)

5. A dot is corectly interpreted as 'any character but a newline'
instead of 'anything but whitespace'.

6. Case-insensitive matching.  The easiest access to it are new
messages CharacterArray understands: #asRegexIgnoringCase,
#matchesRegexIgnoringCase:, #prefixMatchesRegexIgnoringCase:.

7. The matcher (an instance of RxMatcher, the result of
String>>asRegex) now provides a collection-like interface to matches
in a particular string or on a particular stream, as well as
substitution protocol. The interface includes the following messages:

	matchesIn: aString
	matchesIn: aString collect: aBlock
	matchesIn: aString do: aBlock

	matchesOnStream: aStream
	matchesOnStream: aStream collect: aBlock
	matchesOnStream: aStream do: aBlock

	copy: aString translatingMatchesUsing: aBlock
	copy: aString replacingMatchesWith: replacementString

	copyStream: aStream to: writeStream translatingMatchesUsing: aBlock
	copyStream: aStream to: writeStream replacingMatchesWith: aString

Examples:

	'\w+' asRegex matchesIn: 'now is the time'

returns an OrderedCollection containing four strings: 'now', 'is',
'the', and 'time'.

	'\<t\w+' asRegexIgnoringCase
		copy: 'now is the Time'
		translatingMatchesUsing: [:match | match asUppercase]

returns 'now is THE TIME' (the regular expression matches words
beginning with either an uppercase or a lowercase T).

ACKNOWLEDGEMENTS

Since the first release of the matcher, thanks to the input from
several fellow Smalltalkers, I became convinced a native Smalltalk
regular expression matcher was worth the effort to keep it alive. For
the contributions, suggestions, and bug reports that made this release 
possible, I want to thank:

	Felix Hack
	Peter Hatch
	Alan Knight
	Eliot Miranda
	Thomas Muhr
	Robb Shecter
	David N. Smith
	Francis Wolinski

and anyone whom I haven't yet met or heard from, but who agrees this
has not been a complete waste of time.

--Vassili Bykov
October 3, 1999
"

	self error: 'comment only'
]

{ #category : #DOCUMENTATION }
RxParser class >> c: x syntax: xx [
" 

[You can select and `print it' examples in this method. Just don't
forget to cancel the changes.]

The simplest regular expression is a single character.  It matches
exactly that character. A sequence of characters matches a string with
exactly the same sequence of characters:

	'a' matchesRegex: 'a'				-- true
	'foobar' matchesRegex: 'foobar'		-- true
	'blorple' matchesRegex: 'foobar'		-- false

The above paragraph introduced a primitive regular expression (a
character), and an operator (sequencing). Operators are applied to
regular expressions to produce more complex regular expressions.
Sequencing (placing expressions one after another) as an operator is,
in a certain sense, `invisible'--yet it is arguably the most common.

A more `visible' operator is Kleene closure, more often simply
referred to as `a star'.  A regular expression followed by an asterisk
matches any number (including 0) of matches of the original
expression. For example:

	'ab' matchesRegex: 'a*b'		 		-- true
	'aaaaab' matchesRegex: 'a*b'	 	-- true
	'b' matchesRegex: 'a*b'		 		-- true
	'aac' matchesRegex: 'a*b'	 		-- false: b does not match

A star's precedence is higher than that of sequencing. A star applies
to the shortest possible subexpression that precedes it. For example,
'ab*' means `a followed by zero or more occurrences of b', not `zero
or more occurrences of ab':

	'abbb' matchesRegex: 'ab*'	 		-- true
	'abab' matchesRegex: 'ab*'		 	-- false

To actually make a regex matching `zero or more occurrences of ab',
`ab' is enclosed in parentheses:

	'abab' matchesRegex: '(ab)*'		 	-- true
	'abcab' matchesRegex: '(ab)*'	 	-- false: c spoils the fun

Two other operators similar to `*' are `+' and `?'. `+' (positive
closure, or simply `plus') matches one or more occurrences of the
original expression. `?' (`optional') matches zero or one, but never
more, occurrences.

	'ac' matchesRegex: 'ab*c'	 		-- true
	'ac' matchesRegex: 'ab+c'	 		-- false: need at least one b
	'abbc' matchesRegex: 'ab+c'		 	-- true
	'abbc' matchesRegex: 'ab?c'		 	-- false: too many b's

As we have seen, characters `*', `+', `?', `(', and `)' have special
meaning in regular expressions. If one of them is to be used
literally, it should be quoted: preceded with a backslash. (Thus,
backslash is also special character, and needs to be quoted for a
literal match--as well as any other special character described
further).

	'ab*' matchesRegex: 'ab*'		 	-- false: star in the right string is special
	'ab*' matchesRegex: 'ab\*'	 		-- true
	'a\c' matchesRegex: 'a\\c'		 	-- true

The last operator is `|' meaning `or'. It is placed between two
regular expressions, and the resulting expression matches if one of
the expressions matches. It has the lowest possible precedence (lower
than sequencing). For example, `ab*|ba*' means `a followed by any
number of b's, or b followed by any number of a's':

	'abb' matchesRegex: 'ab*|ba*'	 	-- true
	'baa' matchesRegex: 'ab*|ba*'	 	-- true
	'baab' matchesRegex: 'ab*|ba*'	 	-- false

A bit more complex example is the following expression, matching the
name of any of the Lisp-style `car', `cdr', `caar', `cadr',
... functions:

	c(a|d)+r

It is possible to write an expression matching an empty string, for
example: `a|'.  However, it is an error to apply `*', `+', or `?' to
such expression: `(a|)*' is an invalid expression.

So far, we have used only characters as the 'smallest' components of
regular expressions. There are other, more `interesting', components.

A character set is a string of characters enclosed in square
brackets. It matches any single character if it appears between the
brackets. For example, `[01]' matches either `0' or `1':

	'0' matchesRegex: '[01]'		 		-- true
	'3' matchesRegex: '[01]'		 		-- false
	'11' matchesRegex: '[01]'		 		-- false: a set matches only one character

Using plus operator, we can build the following binary number
recognizer:

	'10010100' matchesRegex: '[01]+'	 	-- true
	'10001210' matchesRegex: '[01]+'	 	-- false

If the first character after the opening bracket is `^', the set is
inverted: it matches any single character *not* appearing between the
brackets:

	'0' matchesRegex: '[^01]'		  		-- false
	'3' matchesRegex: '[^01]'		 		-- true

For convenience, a set may include ranges: pairs of characters
separated with `-'. This is equivalent to listing all characters
between them: `[0-9]' is the same as `[0123456789]'.

Special characters within a set are `^', `-', and `]' that closes the
set. Below are the examples of how to literally use them in a set:

	[01^]		-- put the caret anywhere except the beginning
	[01-]		-- put the dash as the last character
	[]01]		-- put the closing bracket as the first character 
	[^]01]			(thus, empty and universal sets cannot be specified)

Regular expressions can also include the following backquote escapes
to refer to popular classes of characters:

	\w	any word constituent character (same as [a-zA-Z0-9_])
	\W	any character but a word constituent
	\d	a digit (same as [0-9])
	\D	anything but a digit
	\s 	a whitespace character (same as [:space:] below)
	\S	anything but a whitespace character

These escapes are also allowed in character classes: '[\w+-]' means
'any character that is either a word constituent, or a plus, or a
minus'.

Character classes can also include the following grep(1)-compatible
elements to refer to:

	[:alnum:]		any alphanumeric character (same as [a-zA-Z0-9])
	[:alpha:]		any alphabetic character (same as [a-zA-Z])
	[:cntrl:]		any control character. (any character with code < 32)
	[:digit:]		any decimal digit (same as [0-9])
	[:graph:]		any graphical character. (any character with code >= 32).
	[:lower:]		any lowercase character (including non-ASCII lowercase characters)
	[:print:]		any printable character. In this version, this is the same as [:graph:]
	[:punct:]		any punctuation character:  . , ! ? ; : ' - ( ) ` and double quotes
	[:space:]		any whitespace character (space, tab, CR, LF, null, form feed, Ctrl-Z, 16r2000-16r200B, 16r3000)
	[:upper:]		any uppercase character (including non-ASCII uppercase characters)
	[:xdigit:]		any hexadecimal character (same as [a-fA-F0-9]).

Note that many of these are only as consistent or inconsistent on issues
of locale as the underlying Smalltalk implementation. Values shown here
are for VisualWorks 7.6.

Note that these elements are components of the character classes,
i.e. they have to be enclosed in an extra set of square brackets to
form a valid regular expression.  For example, a non-empty string of
digits would be represented as '[[:digit:]]+'.

The above primitive expressions and operators are common to many
implementations of regular expressions. The next primitive expression
is unique to this Smalltalk implementation.

A sequence of characters between colons is treated as a unary selector
which is supposed to be understood by Characters. A character matches
such an expression if it answers true to a message with that
selector. This allows a more readable and efficient way of specifying
character classes. For example, `[0-9]' is equivalent to `:isDigit:',
but the latter is more efficient. Analogously to character sets,
character classes can be negated: `:^isDigit:' matches a Character
that answers false to #isDigit, and is therefore equivalent to
`[^0-9]'.

As an example, so far we have seen the following equivalent ways to
write a regular expression that matches a non-empty string of digits:

	'[0-9]+'
	'\d+'
	'[\d]+'
	'[[:digit:]]+'
	:isDigit:+'

The last group of special primitive expressions includes: 

	.	matching any character except a NULL; 
	^	matching an empty string at the beginning of a line; 
	$	matching an empty string at the end of a line.
	\b	an empty string at a word boundary
	\B	an empty string not at a word boundary
	\<	an empty string at the beginning of a word
	\>	an empty string at the end of a word

	'axyzb' matchesRegex: 'a.+b'		-- true
	'ax zb' matchesRegex: 'a.+b'			-- true (space is matched by `.')
	'ax
zb' matchesRegex: 'a.+b'				-- true (carriage return is matched by `.')

Again, the dot ., caret ^ and dollar $ characters are special and should be quoted
to be matched literally.

	EXAMPLES

As the introductions said, a great use for regular expressions is user
input validation. Following are a few examples of regular expressions
that might be handy in checking input entered by the user in an input
field. Try them out by entering something between the quotes and
print-iting. (Also, try to imagine Smalltalk code that each validation
would require if coded by hand).  Most example expressions could have
been written in alternative ways.

Checking if aString may represent a nonnegative integer number:

	'' matchesRegex: ':isDigit:+'
or
	'' matchesRegex: '[0-9]+'
or
	'' matchesRegex: '\d+'

Checking if aString may represent an integer number with an optional
sign in front:

	'' matchesRegex: '(\+|-)?\d+'

Checking if aString is a fixed-point number, with at least one digit
is required after a dot:

	'' matchesRegex: '(\+|-)?\d+(\.\d+)?'

The same, but allow notation like `123.':

	'' matchesRegex: '(\+|-)?\d+(\.\d*)?'

Recognizer for a string that might be a name: one word with first
capital letter, no blanks, no digits.  More traditional:

	'' matchesRegex: '[A-Z][A-Za-z]*'

more Smalltalkish:

	'' matchesRegex: ':isUppercase::isAlphabetic:*'

A date in format MMM DD, YYYY with any number of spaces in between, in
XX century:

	'' matchesRegex: '(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)[ ]+(\d\d?)[ ]*,[ ]*19(\d\d)'

Note parentheses around some components of the expression above. As
`Usage' section shows, they will allow us to obtain the actual strings
that have matched them (i.e. month name, day number, and year number).

For dessert, coming back to numbers: here is a recognizer for a
general number format: anything like 999, or 999.999, or -999.999e+21.

	'' matchesRegex: '(\+|-)?\d+(\.\d*)?((e|E)(\+|-)?\d+)?'

"

	self error: 'comment only'
]

{ #category : #DOCUMENTATION }
RxParser class >> d: x usage: xx [
" 
The preceding section covered the syntax of regular expressions. It
used the simplest possible interface to the matcher: sending
#matchesRegex: message to the sample string, with regular expression
string as the argument.  This section explains hairier ways of using
the matcher.

	PREFIX MATCHING AND CASE-INSENSITIVE MATCHING

A CharacterArray (an EsString in VA) also understands these messages:

	#prefixMatchesRegex: regexString
	#matchesRegexIgnoringCase: regexString
	#prefixMatchesRegexIgnoringCase: regexString

#prefixMatchesRegex: is just like #matchesRegex, except that the whole
receiver is not expected to match the regular expression passed as the
argument; matching just a prefix of it is enough.  For example:

	'abcde' matchesRegex: '(a|b)+'		-- false
	'abcde' prefixMatchesRegex: '(a|b)+'	-- true

The last two messages are case-insensitive versions of matching.

	ENUMERATION INTERFACE

An application can be interested in all matches of a certain regular
expression within a String.  The matches are accessible using a
protocol modelled after the familiar Collection-like enumeration
protocol:

	#regex: regexString matchesDo: aBlock

Evaluates a one-argument <aBlock> for every match of the regular
expression within the receiver string.

	#regex: regexString matchesCollect: aBlock

Evaluates a one-argument <aBlock> for every match of the regular
expression within the receiver string. Collects results of evaluations
and anwers them as a SequenceableCollection.

	#allRegexMatches: regexString

Returns a collection of all matches (substrings of the receiver
string) of the regular expression.  It is an equivalent of <aString
regex: regexString matchesCollect: [:each | each]>.

	#allRangesOfRegexMatches: regexString

Returns a collection of all character ranges (startIndex to: stopIndex)
that match the regular expression.

	REPLACEMENT AND TRANSLATION

It is possible to replace all matches of a regular expression with a
certain string using the message:

	#copyWithRegex: regexString matchesReplacedWith: aString

For example:

	'ab cd ab' copyWithRegex: '(a|b)+' matchesReplacedWith: 'foo'

A more general substitution is match translation:

	#copyWithRegex: regexString matchesTranslatedUsing: aBlock

This message evaluates a block passing it each match of the regular
expression in the receiver string and answers a copy of the receiver
with the block results spliced into it in place of the respective
matches.  For example:

	'ab cd ab' copyWithRegex: '(a|b)+' matchesTranslatedUsing: [:each | each asUppercase]

All messages of enumeration and replacement protocols perform a
case-sensitive match.  Case-insensitive versions are not provided as
part of a CharacterArray protocol.  Instead, they are accessible using
the lower-level matching interface.

	LOWER-LEVEL INTERFACE

Internally, #matchesRegex: works as follows:

1. A fresh instance of RxParser is created, and the regular expression
string is passed to it, yielding the expression's syntax tree.

2. The syntax tree is passed as an initialization parameter to an
instance of RxMatcher. The instance sets up some data structure that
will work as a recognizer for the regular expression described by the
tree.

3. The original string is passed to the matcher, and the matcher
checks for a match.

	THE MATCHER

If you repeatedly match a number of strings against the same regular
expression using one of the messages defined in CharacterArray, the
regular expression string is parsed and a matcher is created anew for
every match.  You can avoid this overhead by building a matcher for
the regular expression, and then reusing the matcher over and over
again. You can, for example, create a matcher at a class or instance
initialization stage, and store it in a variable for future use.

You can create a matcher using one of the following methods:

	- Sending #forString:ignoreCase: message to RxMatcher class, with
the regular expression string and a Boolean indicating whether case is
ignored as arguments.

	- Sending #forString: message.  It is equivalent to <... forString:
regexString ignoreCase: false>.

A more convenient way is using one of the two matcher-created messages
understood by CharacterArray.

	- <regexString asRegex> is equivalent to <RxMatcher forString:
regexString>.

	- <regexString asRegexIgnoringCase> is equivalent to <RxMatcher
forString: regexString ignoreCase: true>.

Here are four examples of creating a matcher:

	hexRecognizer := RxMatcher forString: '16r[0-9A-Fa-f]+'
	hexRecognizer := RxMatcher forString: '16r[0-9A-Fa-f]+' ignoreCase: false
	hexRecognizer := '16r[0-9A-Fa-f]+' asRegex
	hexRecognizer := '16r[0-9A-F]+' asRegexIgnoringCase

	MATCHING

The matcher understands these messages (all of them return true to
indicate successful match or search, and false otherwise):

matches: aString

	True if the whole target string (aString) matches.

matchesPrefix: aString

	True if some prefix of the string (not necessarily the whole
	string) matches.

search: aString

	Search the string for the first occurrence of a matching
	substring. (Note that the first two methods only try matching from
	the very beginning of the string). Using the above example with a
	matcher for `a+', this method would answer success given a string
	`baaa', while the previous two would fail.

matchesStream: aStream
matchesStreamPrefix: aStream
searchStream: aStream

	Respective analogs of the first three methods, taking input from a
	stream instead of a string. The stream must be positionable and
	peekable.

All these methods answer a boolean indicating success. The matcher
also stores the outcome of the last match attempt and can report it:

lastResult

	Answers a Boolean -- the outcome of the most recent match
	attempt. If no matches were attempted, the answer is unspecified.

	SUBEXPRESSION MATCHES

After a successful match attempt, you can query the specifics of which
part of the original string has matched which part of the whole
expression.

A subexpression is a parenthesized part of a regular expression, or
the whole expression. When a regular expression is compiled, its
subexpressions are assigned indices starting from 1, depth-first,
left-to-right. For example, `((ab)+(c|d))?ef' includes the following
subexpressions with these indices:

	1:	((ab)+(c|d))?ef
	2:	(ab)+(c|d)
	3:	ab
	4:	c|d

After a successful match, the matcher can report what part of the
original string matched what subexpression. It understandards these
messages:

subexpressionCount

	Answers the total number of subexpressions: the highest value that
	can be used as a subexpression index with this matcher. This value
	is available immediately after initialization and never changes.

subexpression: anIndex

	An index must be a valid subexpression index, and this message
	must be sent only after a successful match attempt. The method
	answers a substring of the original string the corresponding
	subexpression has matched to.

subBeginning: anIndex
subEnd: anIndex

	Answer positions within the original string or stream where the
	match of a subexpression with the given index has started and
	ended, respectively.

This facility provides a convenient way of extracting parts of input
strings of complex format. For example, the following piece of code
uses the 'MMM DD, YYYY' date format recognizer example from the
`Syntax' section to convert a date to a three-element array with year,
month, and day strings (you can select and evaluate it right here):

	| matcher |
	matcher := RxMatcher forString: '(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)[ ]+(:isDigit::isDigit:?)[ ]*,[ ]*(19|20)(:isDigit::isDigit:)'.
	(matcher matches: 'Aug 6, 1996')
		ifTrue: 
			[Array 
				with: (matcher subexpression: 5)
				with: (matcher subexpression: 2)
				with: (matcher subexpression: 3)]
		ifFalse: ['no match']

(should answer ` #('96' 'Aug' '6')').

	ENUMERATION AND REPLACEMENT

The enumeration and replacement protocols exposed in CharacterArray
are actually implemented by the matcher.  The following messages are
understood:

	#matchesIn: aString
	#matchesIn: aString do: aBlock
	#matchesIn: aString collect: aBlock
	#copy: aString replacingMatchesWith: replacementString
	#copy: aString translatingMatchesUsing: aBlock
	#matchingRangesIn: aString

	#matchesOnStream: aStream
	#matchesOnStream: aStream do: aBlock
	#matchesOnStream: aStream collect: aBlock
	#copy: sourceStream to: targetStream replacingMatchesWith: replacementString
	#copy: sourceStream to: targetStream translatingMatchesWith: aBlock

Note that in those methods that take a block, the block may refer to the rxMatcher itself, 
e.g. to collect information about the position the match occurred at, or the
subexpressions of the match. An example can be seen in #matchingRangesIn:

	ERROR HANDLING

Exception signaling objects (Signals in VisualWorks, Exceptions in VisualAge) are
accessible through RxParser class protocol. To handle possible errors, use
the protocol described below to obtain the exception objects and use the
protocol of the native Smalltalk implementation to handle them.

If a syntax error is detected while parsing expression,
RxParser>>syntaxErrorSignal is raised/signaled.

If an error is detected while building a matcher,
RxParser>>compilationErrorSignal is raised/signaled.

If an error is detected while matching (for example, if a bad selector
was specified using `:<selector>:' syntax, or because of the matcher's
internal error), RxParser>>matchErrorSignal is raised

RxParser>>regexErrorSignal is the parent of all three.  Since any of
the three signals can be raised within a call to #matchesRegex:, it is
handy if you want to catch them all.  For example:

VisualWorks:

	RxParser regexErrorSignal
		handle: [:ex | ex returnWith: nil]
		do: ['abc' matchesRegex: '))garbage[']

VisualAge:

	['abc' matchesRegex: '))garbage[']
		when: RxParser regexErrorSignal
		do: [:signal | signal exitWith: nil]

"

	self error: 'comment only'
]

{ #category : #'exception signaling' }
RxParser class >> doHandlingMessageNotUnderstood: aBlock [
	"MNU should be trapped and resignaled as a match error in a few places in the matcher.
	This method factors out this dialect-dependent code to make porting easier."
	^ aBlock
		on: MessageNotUnderstood
		do: [:ex | RxParser signalMatchException: 'invalid predicate selector']
]

{ #category : #DOCUMENTATION }
RxParser class >> e: x implementationNotes: xx [
"	
	Version:		1.1
	Released:		October 1999
	Mail to:		Vassili Bykov <vassili@parcplace.com>, <v_bykov@yahoo.com>
	Flames to:		/dev/null

	WHAT IS ADDED

The matcher includes classes in two categories:
	VB-Regex-Syntax
	VB-Regex-Matcher
and a few CharacterArray methods in `VB-regex' protocol.  No system
classes or methods are modified.

	WHAT TO LOOK AT FIRST

String>>matchesRegex: -- in 90% cases this method is all you need to
access the package.

RxParser -- accepts a string or a stream of characters with a regular
expression, and produces a syntax tree corresponding to the
expression. The tree is made of instances of Rxs<whatever> classes.

RxMatcher -- accepts a syntax tree of a regular expression built by
the parser and compiles it into a matcher: a structure made of
instances of Rxm<whatever> classes. The RxMatcher instance can test
whether a string or a positionable stream of characters matches the
original regular expression, or search a string or a stream for
substrings matching the expression. After a match is found, the
matcher can report a specific string that matched the whole
expression, or any parenthesized subexpression of it.

All other classes support the above functionality and are used by
RxParser, RxMatcher, or both.

	CAVEATS

The matcher is similar in spirit, but NOT in the design--let alone the
code--to the original Henry Spencer's regular expression
implementation in C.  The focus is on simplicity, not on efficiency.
I didn't optimize or profile anything.  I may in future--or I may not:
I do this in my spare time and I don't promise anything.

The matcher passes H. Spencer's test suite (see 'test suite'
protocol), with quite a few extra tests added, so chances are good
there are not too many bugs.  But watch out anyway.

	EXTENSIONS, FUTURE, ETC.

With the existing separation between the parser, the syntax tree, and
the matcher, it is easy to extend the system with other matchers based
on other algorithms. In fact, I have a DFA-based matcher right now,
but I don't feel it is good enough to include it here.  I might add
automata-based matchers later, but again I don't promise anything.

	HOW TO REACH ME

As of today (December 20, 2000), you can contact me at
<vassili@parcplace.com>. If this doesn't work, look around
comp.lang.smalltalk or comp.lang.lisp.  
"

	self error: 'comment only'
]

{ #category : #DOCUMENTATION }
RxParser class >> f: x boringStuff: xx [
"
The Regular Expression Matcher (``The Software'') 
is Copyright (C) 1996, 1999 Vassili Bykov.  
It is provided to the Smalltalk community in hope it will be useful.

1. This license applies to the package as a whole, as well as to any
   component of it. By performing any of the activities described
   below, you accept the terms of this agreement.

2. The software is provided free of charge, and ``as is'', in hope
   that it will be useful, with ABSOLUTELY NO WARRANTY. The entire
   risk and all responsibility for the use of the software is with
   you.  Under no circumstances the author may be held responsible for
   loss of data, loss of profit, or any other damage resulting
   directly or indirectly from the use of the software, even if the
   damage is caused by defects in the software.

3. You may use this software in any applications you build.

4. You may distribute this software provided that the software
   documentation and copyright notices are included and intact.

5. You may create and distribute modified versions of the software,
   such as ports to other Smalltalk dialects or derived work, provided
   that: 

   a. any modified version is expressly marked as such and is not
   misrepresented as the original software; 

   b. credit is given to the original software in the source code and
   documentation of the derived work; 

   c. the copyright notice at the top of this document accompanies
   copyright notices of any modified version.  "

	self error: 'comment only'
]

{ #category : #'class initialization' }
RxParser class >> initialize [
	"self initialize"
	self
		initializeBackslashConstants;
		initializeBackslashSpecials
]

{ #category : #'class initialization' }
RxParser class >> initializeBackslashConstants [
	"self initializeBackslashConstants"

	(BackslashConstants := Dictionary new)
		at: $e put: Character escape;
		at: $n put: Character lf;
		at: $r put: Character cr;
		at: $f put: Character newPage;
		at: $t put: Character tab
]

{ #category : #'class initialization' }
RxParser class >> initializeBackslashSpecials [
	"Keys are characters that normally follow a \, the values are
	associations of classes and initialization selectors on the instance side
	of the classes."
	"self initializeBackslashSpecials"

	(BackslashSpecials := Dictionary new)
		at: $w put: (Association key: RxsPredicate value: #beWordConstituent);
		at: $W put: (Association key: RxsPredicate value: #beNotWordConstituent);
		at: $s put: (Association key: RxsPredicate value: #beSpace);
		at: $S put: (Association key: RxsPredicate value: #beNotSpace);
		at: $d put: (Association key: RxsPredicate value: #beDigit);
		at: $D put: (Association key: RxsPredicate value: #beNotDigit);
		at: $b put: (Association key: RxsContextCondition value: #beWordBoundary);
		at: $B put: (Association key: RxsContextCondition value: #beNonWordBoundary);
		at: $< put: (Association key: RxsContextCondition value: #beBeginningOfWord);
		at: $> put: (Association key: RxsContextCondition value: #beEndOfWord)
]

{ #category : #utilities }
RxParser class >> parse: aString [
	"Parse the argument and return the result (the parse tree).
	In case of a syntax error, the corresponding exception is signaled."

	^self new parse: aString
]

{ #category : #preferences }
RxParser class >> preferredMatcherClass [
	"The matcher to use. For now just one is available, but in
	principle this determines the matchers built implicitly,
	such as by String>>asRegex, or String>>matchesRegex:.
	This might seem a bit strange place for this preference, but
	Parser is still more or less `central' thing in the whole package."

	^RxMatcher
]

{ #category : #utilities }
RxParser class >> safelyParse: aString [
	"Parse the argument and return the result (the parse tree).
	In case of a syntax error, return nil.
	Exception handling here is dialect-dependent."
	^ [self new parse: aString] on: RegexSyntaxError do: [:ex | nil]
]

{ #category : #'exception signaling' }
RxParser class >> signalCompilationException: errorString [
	RegexCompilationError new signal: errorString
]

{ #category : #'exception signaling' }
RxParser class >> signalMatchException: errorString [
	RegexMatchingError new signal: errorString
]

{ #category : #'exception signaling' }
RxParser class >> signalSyntaxException: errorString [
	RegexSyntaxError new signal: errorString
]

{ #category : #'exception signaling' }
RxParser class >> signalSyntaxException: errorString at: errorPosition [
	RegexSyntaxError signal: errorString at: errorPosition
]

{ #category : #'recursive descent' }
RxParser >> atom [
	"An atom is one of a lot of possibilities, see below."

	| atom |
	(lookahead = #epsilon 
	or: [ lookahead = $| 
	or: [ lookahead = $)
	or: [ lookahead = $*
	or: [ lookahead = $+ 
	or: [ lookahead = $? ]]]]])
		ifTrue: [ ^RxsEpsilon new ].
		
	lookahead = $( 
		ifTrue: [
			"<atom> ::= '(' <regex> ')' "
			self match: $(.
			atom := self regex.
			self match: $).
			^atom ].
	
	lookahead = $[
		ifTrue: [
			"<atom> ::= '[' <characterSet> ']' "
			self match: $[.
			atom := self characterSet.
			self match: $].
			^atom ].
	
	lookahead = $: 
		ifTrue: [
			"<atom> ::= ':' <messagePredicate> ':' "
			self match: $:.
			atom := self messagePredicate.
			self match: $:.
			^atom ].
	
	lookahead = $. 
		ifTrue: [
			"any non-whitespace character"
			self next.
			^RxsContextCondition new beAny].
	
	lookahead = $^ 
		ifTrue: [
			"beginning of line condition"
			self next.
			^RxsContextCondition new beBeginningOfLine].
	
	lookahead = $$ 
		ifTrue: [
			"end of line condition"
			self next.
			^RxsContextCondition new beEndOfLine].
		
	lookahead = $\ 
		ifTrue: [
			"<atom> ::= '\' <character>"
			self next.
			lookahead = #epsilon 
				ifTrue: [ self signalParseError: 'bad quotation' ].
			(BackslashConstants includesKey: lookahead)
				ifTrue: [
					atom := RxsCharacter with: (BackslashConstants at: lookahead).
					self next.
					^atom].
			self ifSpecial: lookahead
				then: [:node | self next. ^node]].
		
	"If passed through the above, the following is a regular character."
	atom := RxsCharacter with: lookahead.
	self next.
	^atom
]

{ #category : #'recursive descent' }
RxParser >> branch [
	"<branch> ::= e | <piece> <branch>"

	| piece branch |
	piece := self piece.
	(lookahead = #epsilon 
	or: [ lookahead = $| 
	or: [ lookahead = $) ]])
		ifTrue: [ branch := nil ]
		ifFalse: [ branch := self branch ].
	^RxsBranch new 
		initializePiece: piece 
		branch: branch
]

{ #category : #'recursive descent' }
RxParser >> characterSet [
	"Match a range of characters: something between `[' and `]'.
	Opening bracked has already been seen, and closing should
	not be consumed as well. Set spec is as usual for
	sets in regexes."

	| spec errorMessage |
	errorMessage := ' no terminating "]"'.
	spec := self inputUpTo: $] nestedOn: $[ errorMessage: errorMessage.
	(spec isEmpty 
	or: [spec = '^']) 
		ifTrue: [
			"This ']' was literal." 
			self next.
			spec := spec, ']', (self inputUpTo: $] nestedOn: $[ errorMessage: errorMessage)].
	^self characterSetFrom: spec
]

{ #category : #private }
RxParser >> characterSetFrom: setSpec [
	"<setSpec> is what goes between the brackets in a charset regex
	(a String). Make a string containing all characters the spec specifies.
	Spec is never empty."

	| negated spec |
	spec := ReadStream on: setSpec.
	spec peek = $^
		ifTrue: 	[negated := true.
				spec next]
		ifFalse:	[negated := false].
	^RxsCharSet new
		initializeElements: (RxCharSetParser on: spec) parse
		negated: negated
]

{ #category : #private }
RxParser >> ifSpecial: aCharacter then: aBlock [
	"If the character is such that it defines a special node when follows a $\,
	then create that node and evaluate aBlock with the node as the parameter.
	Otherwise just return."

	| classAndSelector |
	classAndSelector := BackslashSpecials at: aCharacter ifAbsent: [^self].
	^aBlock value: (classAndSelector key new perform: classAndSelector value)
]

{ #category : #private }
RxParser >> inputUpTo: aCharacter errorMessage: aString [
	"Accumulate input stream until <aCharacter> is encountered
	and answer the accumulated chars as String, not including
	<aCharacter>. Signal error if end of stream is encountered,
	passing <aString> as the error description."

	| accumulator |
	accumulator := WriteStream on: (String new: 20).
	[lookahead ~= aCharacter and: [lookahead ~= #epsilon]]
		whileTrue: [
			accumulator nextPut: lookahead.
			self next].
	lookahead = #epsilon
		ifTrue: [ self signalParseError: aString ].
	^accumulator contents
]

{ #category : #private }
RxParser >> inputUpTo: aCharacter nestedOn: anotherCharacter errorMessage: aString [ 
	"Accumulate input stream until <aCharacter> is encountered
	and answer the accumulated chars as String, not including
	<aCharacter>. Signal error if end of stream is encountered,
	passing <aString> as the error description."

	| accumulator nestLevel |
	accumulator := WriteStream on: (String new: 20).
	nestLevel := 0.
	[lookahead ~= aCharacter or: [nestLevel > 0]] whileTrue: 
			[#epsilon = lookahead ifTrue: [self signalParseError: aString].
			accumulator nextPut: lookahead.
			lookahead = anotherCharacter ifTrue: [nestLevel := nestLevel + 1].
			lookahead = aCharacter ifTrue: [nestLevel := nestLevel - 1].
			self next].
	^accumulator contents
]

{ #category : #private }
RxParser >> inputUpToAny: aDelimiterString errorMessage: aString [
	"Accumulate input stream until any character from <aDelimiterString> is encountered
	and answer the accumulated chars as String, not including the matched characters from the
	<aDelimiterString>. Signal error if end of stream is encountered,
	passing <aString> as the error description."

	| accumulator |
	accumulator := WriteStream on: (String new: 20).
	[(aDelimiterString includes: lookahead) not and: [lookahead ~= #epsilon]]
		whileTrue: [
			accumulator nextPut: lookahead.
			self next ].
	lookahead = #epsilon
		ifTrue: [ self signalParseError: aString ].
	^accumulator contents
]

{ #category : #'recursive descent' }
RxParser >> lookAround [
	"Parse a lookaround expression after: (?<lookround>) 
	<lookround> ::= !<regex> | =<regex>"
	| lookaround |
	(lookahead = $!
	or: [ lookahead = $=])
		ifFalse: [ ^ self signalParseError: 'Invalid lookaround expression ?', lookahead asString ].
	self next.
	lookaround := RxsLookaround with: self regex.
	lookahead = $!
		ifTrue: [ lookaround beNegative ].
	^ lookaround
	
]

{ #category : #private }
RxParser >> match: aCharacter [
	"<aCharacter> MUST match the current lookeahead.
	If this is the case, advance the input. Otherwise, blow up."

	aCharacter ~= lookahead 
		ifTrue: [^self signalParseError].	"does not return"
	self next
]

{ #category : #'recursive descent' }
RxParser >> messagePredicate [
	"Match a message predicate specification: a selector (presumably
	understood by a Character) enclosed in :'s ."

	| spec negated |
	spec := self inputUpTo: $: errorMessage: ' no terminating ":"'.
	negated := false.
	spec first = $^ 
		ifTrue: [
			negated := true.
			spec := spec copyFrom: 2 to: spec size].
	^RxsMessagePredicate new 
		initializeSelector: spec asSymbol
		negated: negated
]

{ #category : #private }
RxParser >> next [
	"Advance the input storing the just read character
	as the lookahead."

	input atEnd
		ifTrue: [lookahead := #epsilon]
		ifFalse: [lookahead := input next]
]

{ #category : #accessing }
RxParser >> parse: aString [
	"Parse input from a string <aString>.
	On success, answers an RxsRegex -- parse tree root.
	On error, raises `RxParser syntaxErrorSignal' with the current
	input stream position as the parameter."

	^self parseStream: (ReadStream on: aString)
]

{ #category : #accessing }
RxParser >> parseStream: aStream [
	"Parse an input from a character stream <aStream>.
	On success, answers an RxsRegex -- parse tree root.
	On error, raises `RxParser syntaxErrorSignal' with the current
	input stream position as the parameter."

	| tree |
	input := aStream.
	lookahead := nil.
	self match: nil.
	tree := self regex.
	self match: #epsilon.
	^tree
]

{ #category : #'recursive descent' }
RxParser >> piece [
	"<piece> ::= <atom> | <atom>* | <atom>+ | <atom>? | <atom>{<number>,<number>}"

	| atom |
	atom := self atom.
	
	lookahead = $*
		ifTrue: [ 
			self next.
			atom isNullable
				ifTrue: [ self signalNullableClosureParserError ].
			^ RxsPiece new initializeStarAtom: atom ].

	lookahead = $+
		ifTrue: [ 
			self next.
			atom isNullable
				ifTrue: [ self signalNullableClosureParserError ].
			^ RxsPiece new initializePlusAtom: atom ].

	lookahead = $?
		ifTrue: [ 
			self next.
			atom isNullable
				ifTrue: [ 
					^ self lookAround ].
			^ RxsPiece new initializeOptionalAtom: atom ].
	
	lookahead = ${
		ifTrue: [
			^ self quantifiedAtom: atom ].
		
	^ RxsPiece new initializeAtom: atom
]

{ #category : #'recursive descent' }
RxParser >> quantifiedAtom: atom [
	"Parse a quanitifer expression which can have one of the following forms
		{<min>,<max>}    match <min> to <max> occurences
		{<minmax>}       which is the same as with repeated limits: {<number>,<number>}
		{<min>,}         match at least <min> occurences
		{,<max>}         match maximally <max> occurences, which is the same as {0,<max>}"
	| min max |
	self next.
	lookahead = $,
		ifTrue: [ min := 0 ]
		ifFalse: [
			max := min := (self inputUpToAny: ',}' errorMessage: ' no terminating "}"') asUnsignedInteger ].
	lookahead = $,
		ifTrue: [
			self next.
			max := (self inputUpToAny: ',}' errorMessage: ' no terminating "}"') asUnsignedInteger ].	
	self match: $}.
	atom isNullable
		ifTrue: [ self signalNullableClosureParserError ].
	(max isNotNil and: [ max < min ])
		ifTrue: [ self signalParseError: ('wrong quantifier, expected ', min asString, ' <= ', max asString) ].
	^ RxsPiece new 
		initializeAtom: atom
		min: min
		max: max
]

{ #category : #'recursive descent' }
RxParser >> regex [
	"<regex> ::= e | <branch> `|' <regex>"

	| branch regex |
	branch := self branch.
	
	(lookahead = #epsilon 
	or: [ lookahead = $) ])
		ifTrue: [ regex := nil ]
		ifFalse: [
			self match: $|.
			regex := self regex ].
		
	^RxsRegex new initializeBranch: branch regex: regex
]

{ #category : #private }
RxParser >> signalNullableClosureParserError [
	self signalParseError: ' nullable closure'.
]

{ #category : #private }
RxParser >> signalParseError [

	self class 
		signalSyntaxException: 'Regex syntax error' at: input position
]

{ #category : #private }
RxParser >> signalParseError: aString [

	self class signalSyntaxException: aString at: input position
]
