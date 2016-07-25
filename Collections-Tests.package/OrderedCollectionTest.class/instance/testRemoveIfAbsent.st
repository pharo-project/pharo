tests - removing
testRemoveIfAbsent
	"Allows one to remove an element from a collection and to copy it in another collection."
	"If the element isn't in the first collection, the second collection copy the element after ifAbsent"
	"self run:#testRemoveIfAbsent"
	
	| c1 c2 |
	c1 := #(1 2 3 4) asOrderedCollection.
	c2 := OrderedCollection new.
	
	c2 add: (c1 remove: 2 ifAbsent: [6]).
	self assert: (c1 = #(1 3 4) asOrderedCollection).
	self assert: (c2 = #(2) asOrderedCollection).
	
	c2 add: (c1 remove: 18 ifAbsent: [6]).
	self assert: (c1 = #(1 3 4) asOrderedCollection).
	self assert: (c2 = #(2 6) asOrderedCollection).