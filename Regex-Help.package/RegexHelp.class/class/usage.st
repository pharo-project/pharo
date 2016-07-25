pages
usage
	"This method was automatically generated. Edit it using:"
	"RegexHelp edit: #usage"
	^HelpTopic
		title: 'Usage'
		contents: 
'The preceding section covered the syntax of regular expressions. It
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

	''abcde'' matchesRegex: ''(a|b)+''		-- false
	''abcde'' prefixMatchesRegex: ''(a|b)+''	-- true

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

	''ab cd ab'' copyWithRegex: ''(a|b)+'' matchesReplacedWith: ''foo''

A more general substitution is match translation:

	#copyWithRegex: regexString matchesTranslatedUsing: aBlock

This message evaluates a block passing it each match of the regular
expression in the receiver string and answers a copy of the receiver
with the block results spliced into it in place of the respective
matches.  For example:

	''ab cd ab'' copyWithRegex: ''(a|b)+'' matchesTranslatedUsing: [:each | each asUppercase]

All messages of enumeration and replacement protocols perform a
case-sensitive match.  Case-insensitive versions are not provided as
part of a CharacterArray protocol.  Instead, they are accessible using
the lower-level matching interface.

	LOWER-LEVEL INTERFACE

Internally, #matchesRegex: works as follows:

1. A fresh instance of RxParser is created, and the regular expression
string is passed to it, yielding the expression''s syntax tree.

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

	hexRecognizer := RxMatcher forString: ''16r[0-9A-Fa-f]+''
	hexRecognizer := RxMatcher forString: ''16r[0-9A-Fa-f]+'' ignoreCase: false
	hexRecognizer := ''16r[0-9A-Fa-f]+'' asRegex
	hexRecognizer := ''16r[0-9A-F]+'' asRegexIgnoringCase

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
	matcher for `a+'', this method would answer success given a string
	`baaa'', while the previous two would fail.

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
left-to-right. For example, `((ab)+(c|d))?ef'' includes the following
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
uses the ''MMM DD, YYYY'' date format recognizer example from the
`Syntax'' section to convert a date to a three-element array with year,
month, and day strings (you can select and evaluate it right here):

	| matcher |
	matcher := RxMatcher forString: ''(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)[ ]+(:isDigit::isDigit:?)[ ]*,[ ]*(19|20)(:isDigit::isDigit:)''.
	(matcher matches: ''Aug 6, 1996'')
		ifTrue: 
			[Array 
				with: (matcher subexpression: 5)
				with: (matcher subexpression: 2)
				with: (matcher subexpression: 3)]
		ifFalse: [''no match'']

(should answer ` #(''96'' ''Aug'' ''6'')'').

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
was specified using `:<selector>:'' syntax, or because of the matcher''s
internal error), RxParser>>matchErrorSignal is raised

RxParser>>regexErrorSignal is the parent of all three.  Since any of
the three signals can be raised within a call to #matchesRegex:, it is
handy if you want to catch them all.  For example:

VisualWorks:

	RxParser regexErrorSignal
		handle: [:ex | ex returnWith: nil]
		do: [''abc'' matchesRegex: ''))garbage['']

VisualAge:

	[''abc'' matchesRegex: ''))garbage['']
		when: RxParser regexErrorSignal
		do: [:signal | signal exitWith: nil]
!' readStream nextChunkText