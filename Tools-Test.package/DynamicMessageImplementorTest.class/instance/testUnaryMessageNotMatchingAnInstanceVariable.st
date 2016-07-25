testing
testUnaryMessageNotMatchingAnInstanceVariable

	| messageImplementor sourceCode |		
	
	messageImplementor := DynamicMessageImplementor for: (Message selector: #m1) in: self class.	
	sourceCode := messageImplementor value.	
	self assert: sourceCode withSeparatorsCompacted equals: 'm1 self shouldBeImplemented.' withSeparatorsCompacted