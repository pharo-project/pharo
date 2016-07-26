DOCUMENTATION
b: x whatsNewInThisRelease: xx
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