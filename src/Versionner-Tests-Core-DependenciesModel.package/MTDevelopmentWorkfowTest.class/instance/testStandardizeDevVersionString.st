tests
testStandardizeDevVersionString
	| workflow |
	
	workflow := MTDevelopmentWorkfow new.
	self assert: (workflow standardizeDevVersionString: '1.0-baseline') equals: '1.0-baseline'.
	self assert: (workflow standardizeDevVersionString: 'baseline-1.3') equals: '1.3-baseline'.