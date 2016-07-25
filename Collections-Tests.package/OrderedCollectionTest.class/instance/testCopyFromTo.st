tests - copying
testCopyFromTo
	"Allows one to create a copy of the receiver that contains elements from position start to end"
	"self run: #testCopyFromTo"
	
	| c1 c2 c3 | 
	c1 := #(1 2 3 4) asOrderedCollection.
	c2 := (c1 copyFrom: 1 to: 2).
	self assert: c2 = #(1 2) asOrderedCollection.
	self should: [c1 copyFrom: 10 to: 20] raise: Error.
	
	c3 := c1 copyFrom: 4 to: 2.
	self assert: c3 isEmpty.
	
	self should: [c1 copyFrom: 4 to: 5 ] raise: Error.
	
	
	
