private
indexOfNode: aNode 
	"Try to find the node by first looking for ==, and then for ="

	^ (1 to: statements size) 
		detect: [ :each | (statements at: each) == aNode ]
		ifNone: [ statements indexOf: aNode ]