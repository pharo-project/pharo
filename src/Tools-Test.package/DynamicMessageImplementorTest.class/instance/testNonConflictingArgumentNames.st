testing
testNonConflictingArgumentNames

	| messageImplementor sourceCode |		
	
	messageImplementor := DynamicMessageImplementor for: (Message selector: #m1:m2:m3: arguments: #(1 $a 'string')) in: self class.	
	sourceCode := messageImplementor value.	
	self assert: sourceCode withSeparatorsCompacted equals: 'm1: anInteger m2: aCharacter m3: aString self shouldBeImplemented.' withSeparatorsCompacted