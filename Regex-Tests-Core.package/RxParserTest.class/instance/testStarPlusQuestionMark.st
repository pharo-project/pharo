tests
testStarPlusQuestionMark
	"self debug: #testStarPlusQuestionMark"
	
	"Two other operators similar to `*' are `+' and `?'. `+' (positive
closure, or simply `plus') matches one or more occurrences of the
original expression. `?' (`optional') matches zero or one, but never
more, occurrences."

	self assert: ('ac' matchesRegex: 'ab*c').  		
	self deny: ('ac' matchesRegex: 'ab+c'). 		"-- false: need at least one b"
	self assert: ('abbc' matchesRegex: 'ab+c').	
	self assert: ('abbbbbbc' matchesRegex: 'ab+c').	
	self deny: ('abbc' matchesRegex: 'ab?c')	 	"-- false: too many b's"