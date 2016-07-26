tests
testSimpleMatchesRegex
	"self debug: #testSimpleMatchesRegex"
	
	"The simplest regular expression is a single character.  It matches
exactly that character. A sequence of characters matches a string with
exactly the same sequence of characters:"

	self assert: ('a' matchesRegex: 'a').
	self assert: ('foobar' matchesRegex: 'foobar')	.
	self deny: ('blorple' matchesRegex: 'foobar')