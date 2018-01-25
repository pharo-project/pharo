"
This class provides tests for the regular expression parser.
"
Class {
	#name : #RxParserTest,
	#superclass : #TestCase,
	#category : #'Regex-Tests-Core'
}

{ #category : #tests }
RxParserTest >> DoesNotWorktestBackQuotesEscape [
	
	"Regular expressions can also include the following backquote escapes
to refer to popular classes of characters:
	\w	any word constituent character (same as [a-zA-Z0-9:=])
	\W	any character but a word constituent
	\d	a digit (same as [0-9])
	\D	anything but a digit
	\s 	a whitespace character
	\S	anything but a whitespace character
These escapes are also allowed in character classes: '[\w+-]' means
'any character that is either a word constituent, or a plus, or a
minus'."
	
	self assert: ('one word' matchesRegex: '\w').	
		
	self assert: ('one' matchesRegex: '\w').		  	
	
]

{ #category : #tests }
RxParserTest >> test [

	self assert: ('\<t\w+' asRegexIgnoringCase
		copy: 'now is the Time'
		translatingMatchesUsing: [:match | match asUppercase]) = 'now is THE TIME'.

	"the regular expression matches words beginning with either an uppercase or a lowercase T"
]

{ #category : #tests }
RxParserTest >> testCadrMatching [
	"A bit more complex example is the following expression, matching the
name of any of the Lisp-style `car', `cdr', `caar', `cadr',
... functions:"

	self assert: ( 'car' matchesRegex: 'c(a|d)+r').
	self assert: ( 'cdr' matchesRegex: 'c(a|d)+r').
	self assert: ( 'caar' matchesRegex: 'c(a|d)+r').
	self assert: ( 'cadr' matchesRegex: 'c(a|d)+r').
	self assert: ( 'caddar' matchesRegex: 'c(a|d)+r').
]

{ #category : #tests }
RxParserTest >> testCharacterSet [		
	"So far, we have used only characters as the 'smallest' components of
regular expressions. There are other, more `interesting', components.
A character set is a string of characters enclosed in square
brackets. It matches any single character if it appears between the
brackets. For example, `[01]' matches either `0' or `1':"

	self assert: ('0' matchesRegex: '[01]').	 	
	self deny: ('3' matchesRegex: '[01]'). 	 
	self deny: ('11' matchesRegex: '[01]').	"-- false: a set matches only one character"
	self deny: ('01' matchesRegex: '[01]').

]

{ #category : #tests }
RxParserTest >> testCharacterSetBinaryNumber [	
	"Using plus operator, we can build the following binary number
recognizer:"
	self assert: ('10010100' matchesRegex: '[01]+').	 	
	self deny: ('10001210' matchesRegex: '[01]+')	 
]

{ #category : #tests }
RxParserTest >> testCharacterSetInversion [	
	"If the first character after the opening bracket is `^', the set is
inverted: it matches any single character *not* appearing between the
brackets:"
	
	self deny: ('0' matchesRegex: '[^01]').		  	
	"0 appears in 01 so there is no match"
	
	self assert: ('3' matchesRegex: '[^01]').
	"3 is not in 01 so it matches"
	
	
	self deny: ('30' matchesRegex: '[^01]').		
	self deny: ('33333333333333333333333330' matchesRegex: '[^01]').	
	"there is one zero so it does not match"
]

{ #category : #tests }
RxParserTest >> testCharacterSetRange [
	"For convenience, a set may include ranges: pairs of characters
separated with `-'. This is equivalent to listing all characters
between them: `[0-9]' is the same as `[0123456789]'. "	

	self assert: ('0' matchesRegex: '[0-9]').	
	self assert: ('9' matchesRegex: '[0-9]').	
	self deny: ('a' matchesRegex: '[0-9]').
	self deny: ('01' matchesRegex: '[0-9]').	
	self assert: ('01442128629839374565' matchesRegex: '[0-9]+').	
	
]

{ #category : #tests }
RxParserTest >> testLookaround [
	self assert: ('A'   matchesRegex: '(?!ABC).*').
	self assert: ('AB'  matchesRegex: '(?!ABC).*').
	self deny:   ('ABC' matchesRegex: '(?!ABC).*').
]

{ #category : #tests }
RxParserTest >> testMatchesInwW [
	"1. Backslash escapes similar to those in Perl are allowed in patterns:
	\w	any word constituent character (equivalent to [a-zA-Z0-9:=])
	\W	any character but a word constituent (equivalent to [^a-xA-Z0-9:=]"

	self assert: ('\w+' asRegex matchesIn: 'now is the time') asArray = #('now' 'is' 'the' 'time').
	self assert: ('\W+' asRegex matchesIn: 'now is the time') asArray = #(' ' ' ' ' ').
	
	"why do we get that"
	self assert: ('\w' asRegex matchesIn: 'now') asArray = #('n' 'o' 'w').
]

{ #category : #tests }
RxParserTest >> testOrOperator [
	"The last operator is `|' meaning `or'. It is placed between two
regular expressions, and the resulting expression matches if one of
the expressions matches. It has the lowest possible precedence (lower
than sequencing). For example, `ab*|ba*' means `a followed by any
number of b's, or b followed by any number of a's':"

	self assert: ('abb' matchesRegex: 'ab*|ba*').  	
	self assert: ('baa' matchesRegex: 'ab*|ba*').	 	
	self deny: ('baab' matchesRegex: 'ab*|ba*').
	

	"It is possible to write an expression matching an empty string, for
example: `a|'.  However, it is an error to apply `*', `+', or `?' to
such expression: `(a|)*' is an invalid expression."

	self should: ['(a|)*' asRegex] raise: Error.

]

{ #category : #tests }
RxParserTest >> testQuantifier [
	self deny:   (''     matchesRegex: 'a{2}').
	self deny:   ('a'    matchesRegex: 'a{2}').
	self assert: ('aa'   matchesRegex: 'a{2}').
	self deny:   ('aaa'  matchesRegex: 'a{2}').
	
	self deny:   (''     matchesRegex: 'a{2,3}').
	self deny:   ('a'    matchesRegex: 'a{2,3}').
	self assert: ('aa'   matchesRegex: 'a{2,3}').
	self assert: ('aaa'  matchesRegex: 'a{2,3}').
	self deny:   ('aaaa' matchesRegex: 'a{2,3}').
	
	self deny:   (''     matchesRegex: 'a{2,}').
	self deny:   ('a'    matchesRegex: 'a{2,}').
	self assert: ('aa'   matchesRegex: 'a{2,}').
	self assert: ('aaa'  matchesRegex: 'a{2,}').
	self assert: ('aaaa' matchesRegex: 'a{2,}').
	
	self assert: (''     matchesRegex: 'a{,3}').
	self assert: ('a'    matchesRegex: 'a{,3}').
	self assert: ('aa'   matchesRegex: 'a{,3}').
	self assert: ('aaa'  matchesRegex: 'a{,3}').
	self deny:   ('aaaa' matchesRegex: 'a{,3}').
]

{ #category : #tests }
RxParserTest >> testQuantifierSimple [
	"Test quantifier expressions that can be expressed with + or *"
	self assert: ('a'  matchesRegex: 'a{1}').
	self deny:   ('aa' matchesRegex: 'a{1}').
	
	self assert: ('a'  matchesRegex: 'a{1,1}').
	self deny:   ('aa' matchesRegex: 'a{1,1}').
	
	self assert: ('ab'   matchesRegex: '(ab){1,}').
	self assert: ('abab' matchesRegex: '(ab){1,}').
	self deny:   (''     matchesRegex: '(ab){1,}').
	
	self assert: ('ab'   matchesRegex: '(ab){,1}').
	self assert: (''     matchesRegex: '(ab){,1}').
	self deny:   ('abab' matchesRegex: '(ab){,1}').
]

{ #category : #tests }
RxParserTest >> testQuotingOperators [
	"As we have seen, characters `*', `+', `?', `(', and `)' have special
meaning in regular expressions. If one of them is to be used
literally, it should be quoted: preceded with a backslash. (Thus,
backslash is also special character, and needs to be quoted for a
literal match--as well as any other special character described
further)."

	self deny: ('ab*' matchesRegex: 'ab*'). "	-- false: star in the right string is special"
	self assert: ('ab*' matchesRegex: 'ab\*').	 		
	self assert: ('a\c' matchesRegex: 'a\\c').		 	
]

{ #category : #tests }
RxParserTest >> testSimpleMatchesRegex [
	"The simplest regular expression is a single character.  It matches
exactly that character. A sequence of characters matches a string with
exactly the same sequence of characters:"

	self assert: ('a' matchesRegex: 'a').
	self assert: ('foobar' matchesRegex: 'foobar')	.
	self deny: ('blorple' matchesRegex: 'foobar')
]

{ #category : #tests }
RxParserTest >> testSimpleMatchesRegexWithStar [
	"The above paragraph in testSimpleMatchesRegex introduced a primitive regular expression (a
character), and an operator (sequencing). Operators are applied to
regular expressions to produce more complex regular expressions.
Sequencing (placing expressions one after another) as an operator is,
in a certain sense, `invisible'--yet it is arguably the most common.
A more `visible' operator is Kleene closure, more often simply
referred to as `a star'.  A regular expression followed by an asterisk
matches any number (including 0) of matches of the original
expression. For example:"

	self assert: ('ab' matchesRegex: 'a*b').		 		
	self assert: ('aaaaab' matchesRegex: 'a*b').	
	self assert: ('b' matchesRegex: 'a*b').	 	
	self deny: ('aac' matchesRegex: 'a*b').	 		
]

{ #category : #tests }
RxParserTest >> testSpecialCharacterInSetRange [
	"Special characters within a set are `^', `-', and `]' that closes the
set. Below are the examples of how to literally use them in a set:
	[01^]		-- put the caret anywhere except the beginning
	[01-]		-- put the dash as the last character
	[]01]		-- put the closing bracket as the first character 
	[^]01]			(thus, empty and universal sets cannot be specified)"

	self assert: ('0' matchesRegex: '[01^]').
	self assert: ('1' matchesRegex: '[01^]').
	self assert: ('^' matchesRegex: '[01^]').
	
	self deny: ('0' matchesRegex: '[^01]').
	self deny: ('1' matchesRegex: '[^01]').
	
	"[^abc] means that everything except abc is matche"
	self assert: ('^' matchesRegex: '[^01]').
	
]

{ #category : #tests }
RxParserTest >> testStarPlusQuestionMark [
	"Two other operators similar to `*' are `+' and `?'. `+' (positive
closure, or simply `plus') matches one or more occurrences of the
original expression. `?' (`optional') matches zero or one, but never
more, occurrences."

	self assert: ('ac' matchesRegex: 'ab*c').  		
	self deny: ('ac' matchesRegex: 'ab+c'). 		"-- false: need at least one b"
	self assert: ('abbc' matchesRegex: 'ab+c').	
	self assert: ('abbbbbbc' matchesRegex: 'ab+c').	
	self deny: ('abbc' matchesRegex: 'ab?c')	 	"-- false: too many b's"
]

{ #category : #tests }
RxParserTest >> testStarPrecedence [
	"A star's precedence is higher than that of sequencing. A star applies
to the shortest possible subexpression that precedes it. For example,
'ab*' means `a followed by zero or more occurrences of b', not `zero
or more occurrences of ab':"

	self assert: ('abbb' matchesRegex: 'ab*'). 
	self deny: ('abab' matchesRegex: 'ab*').	 	
		
	"To actually make a regex matching `zero or more occurrences of ab',
`ab' is enclosed in parentheses:"
	self assert: ('abab' matchesRegex: '(ab)*'). 
	self deny: ('abcab' matchesRegex: '(ab)*')
]

{ #category : #tests }
RxParserTest >> testTranslatingMatchesUsing [
	self assert: ('\<t\w+' asRegexIgnoringCase
		copy: 'now is the Time'
		translatingMatchesUsing: [:match | match asUppercase]) = 'now is THE TIME'.

	"the regular expression matches words beginning with either an uppercase or a lowercase T"
]

{ #category : #tests }
RxParserTest >> toDotestSpecialCharacterInSetRange [
	
	"Special characters within a set are `^', `-', and `]' that closes the
set. Below are the examples of how to literally use them in a set:
	[01^]		-- put the caret anywhere except the beginning
	[01-]		-- put the dash as the last character
	[]01]		-- put the closing bracket as the first character 
	[^]01]			(thus, empty and universal sets cannot be specified)"

	self assert: ('0' matchesRegex: '[01^]').
	
	self assert: ('0' matchesRegex: '[0-9]').	
	
]
