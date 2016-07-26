testing
testGenerateSetter

	| messageImplementor sourceCode |		
	
	messageImplementor := DynamicMessageImplementor
		for: (Message selector: #instVar: argument: 1)
		in: self class.	
	sourceCode := messageImplementor value.	
	self
		assert: sourceCode withSeparatorsCompacted
		equals: 'instVar: anInteger instVar := anInteger' withSeparatorsCompacted