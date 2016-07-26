tests - enumerating
testCollect
	"Allows one to collect some element of a collection into another collection"
	"self run: #testCollect"
	 
	| c1 c2 res |
	c1 := #(-1 2 -3 4) asOrderedCollection.
	c2 := #(1 2 3 4) asOrderedCollection.
	res := c1 collect: [:each | each abs].
	self assert: (c2 = res).