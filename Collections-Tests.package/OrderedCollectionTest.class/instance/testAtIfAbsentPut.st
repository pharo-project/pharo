tests - adding
testAtIfAbsentPut
	"Allows one to add an element at an index if no element exist at this index"

	"self run:#testAtIfAbsentPut"

	| c |
	c := #(1 2 3 4) asOrderedCollection.
	c at: 2 ifAbsentPut: [ 5 ].
	self assert: c = #(1 2 3 4) asOrderedCollection.
	c at: 5 ifAbsentPut: [ 5 ].
	self assert: c = #(1 2 3 4 5) asOrderedCollection.
	c at: 7 ifAbsentPut: [ 7 ].
	self assert: c = #(1 2 3 4 5 nil 7) asOrderedCollection