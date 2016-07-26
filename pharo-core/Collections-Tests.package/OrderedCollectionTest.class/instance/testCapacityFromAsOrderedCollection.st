tests - accessing
testCapacityFromAsOrderedCollection
	"Allows one to check the current capacity of an Ordered collection"
	"self run:#testCapacityFromAsOrderedCollection"
	
	| c1 c2 c3 |
	c1 := #(1 2 ) asOrderedCollection.
	self assert: (c1 capacity =  2).
	c2 := OrderedCollection new: 10.
	c2 add: 3.
	self assert: (c2 capacity = 10).	
	c3 := OrderedCollection new.
	self deny: (c3 capacity =  0).
	