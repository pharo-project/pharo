tests - adding
testAddAllFirst
	"Allows one to add each element of an orderedCollection at the beginning of another
	orderedCollection "
	"self run:#testAddAllFirst" 
	
	| c1 c2 |
	c1 := #(1 2 3 4 ) asOrderedCollection.
	c2 := #(5 6 7 8 9 ) asOrderedCollection.
	c2 addAllFirst: c1.
	self assert: c2 = #(1 2 3 4 5 6 7 8 9) asOrderedCollection