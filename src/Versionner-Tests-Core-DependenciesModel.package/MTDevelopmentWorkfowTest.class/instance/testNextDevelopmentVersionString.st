tests
testNextDevelopmentVersionString
	| workflow |
	
	workflow := MTDevelopmentWorkfow new.
	self assert: (workflow nextDevelopmentVersionString: '1.0-baseline') equals: '1.1-baseline'.
	self assert: (workflow nextDevelopmentVersionString: '1.0') equals: '1.1-baseline'.
	self assert: (workflow nextDevelopmentVersionString: 'baseline-0.9') equals: '0.10-baseline'.