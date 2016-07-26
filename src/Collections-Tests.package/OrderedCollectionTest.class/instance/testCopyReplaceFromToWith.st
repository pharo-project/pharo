tests - copying
testCopyReplaceFromToWith
	"Allows one to create a copy from the receiver which elements between start and end of the 	receiver being replace by 	element of the collection after with:"
	"self run:#testCopyReplaceFromToWith"

	| c1 c2 c3 c4 |
	c1 := #(1 2 3 4) asOrderedCollection.
	c2 := #(5 6 7 8 9) asOrderedCollection.
	c3 := (c2 copyReplaceFrom: 1 to: 2 with: c1).
	self assert: c3 = #(1 2 3 4 7 8 9) asOrderedCollection.
	self should: [c2 copyReplaceFrom: 3 to: 1 with: c1] raise: Error.
	
	c4 := (c2 copyReplaceFrom: 10 to: 25 with: c1).
	self assert: c4 = #(5 6 7 8 9 1 2 3 4) asOrderedCollection.
	
	