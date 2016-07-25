tests
testCharacterSetRange
	"self debug: #testCharacterSetRange"
	
	"For convenience, a set may include ranges: pairs of characters
separated with `-'. This is equivalent to listing all characters
between them: `[0-9]' is the same as `[0123456789]'. "	

	self assert: ('0' matchesRegex: '[0-9]').	
	self assert: ('9' matchesRegex: '[0-9]').	
	self deny: ('a' matchesRegex: '[0-9]').
	self deny: ('01' matchesRegex: '[0-9]').	
	self assert: ('01442128629839374565' matchesRegex: '[0-9]+').	
	