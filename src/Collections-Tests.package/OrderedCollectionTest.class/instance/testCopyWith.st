tests - copying
testCopyWith
	"Allows one to create a copy of the receiver that contains the new element at the end"
	"self run: #testCopyWith"
	
	| c1 | 
	c1 := #(1 2 3 4) asOrderedCollection.
	c1 := c1 copyWith: 6.
	self assert: c1 = #(1 2 3 4 6) asOrderedCollection.
	
	

	
	
	
