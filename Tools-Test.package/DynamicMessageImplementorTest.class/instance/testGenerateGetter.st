testing
testGenerateGetter

	| messageImplementor sourceCode |		
	
	messageImplementor := DynamicMessageImplementor 
		for: (Message selector: #instVar) 
		in: self class.	
	sourceCode := messageImplementor value.	
	self 
		assert: sourceCode withSeparatorsCompacted 
		equals: 'instVar	^ instVar' withSeparatorsCompacted