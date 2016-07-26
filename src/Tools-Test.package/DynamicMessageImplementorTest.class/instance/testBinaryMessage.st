testing
testBinaryMessage

	| messageImplementor sourceCode |		
	
	messageImplementor := DynamicMessageImplementor for: (Message selector: #+ argument: 1) in: self class.	
	sourceCode := messageImplementor value.	
	self assert: sourceCode withSeparatorsCompacted equals: '+ anInteger self shouldBeImplemented.' withSeparatorsCompacted