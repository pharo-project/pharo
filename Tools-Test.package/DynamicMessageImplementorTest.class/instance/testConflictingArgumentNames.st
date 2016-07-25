testing
testConflictingArgumentNames

	| messageImplementor sourceCode |		
	
	messageImplementor := DynamicMessageImplementor for: (Message selector: #m1:m2:m3: arguments: #(1 2 3)) in: self class.	
	sourceCode := messageImplementor value.	
	self assert: sourceCode withSeparatorsCompacted equals: 'm1: anInteger m2: anInteger2 m3: anInteger3 self shouldBeImplemented.' withSeparatorsCompacted