tests - enumerating
testCollectFromTo
	"Allows one to collect some element of a collection into another collection between a first index and an end index for the collect"
	"self run: #testCollectFromTo"
	
	| c1 res |
	c1 := #(-1 2 -3 4 -5 6 -7 8) asOrderedCollection.
	res := c1 collect: [:each | each abs] from: 1 to: 3.
	self assert: (res = #(1 2 3) asOrderedCollection).
	self should: [c1 collect: [:each | each abs] from: 10 to: 13] raise: Error.
	self should: [c1 collect: [:each | each abs] from: 5 to: 2] raise: Error.