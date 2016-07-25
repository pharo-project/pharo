tests
testSimpleMatchesRegexWithStar
	"self debug: #testSimpleMatchesRegexWithStar"
	
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