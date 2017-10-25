tests
testIsPatternVariable

	self assert: '`pattern' isPatternVariable.
	self assert: '`pattern:' isPatternVariable.
	self assert: '`pattern:`varible:' isPatternVariable.
	self assert: 'pattern:`varible:' isPatternVariable.
	
	self deny: 'notAPattern:varible:' isPatternVariable.
	self deny: 'notAPattern' isPatternVariable.
		
