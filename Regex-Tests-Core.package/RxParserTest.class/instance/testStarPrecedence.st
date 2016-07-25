tests
testStarPrecedence
	"self debug: #testStarPrecedence"
	
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