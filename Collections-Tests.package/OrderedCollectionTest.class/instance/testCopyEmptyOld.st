tests - copying
testCopyEmptyOld
	"Allows one to create a copy of the receiver that contains no elements"
	"self run:#testCopyEmpty"
	
	| c1 c2 |
	c1 := #(1 2 3 4 ) asOrderedCollection.
	c2 := c1 copyEmpty.
	self assert: (c2 size = 0).