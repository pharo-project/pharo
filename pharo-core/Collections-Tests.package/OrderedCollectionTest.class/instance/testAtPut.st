tests - accessing
testAtPut
	"Allows one to replace an element but not at an off range index"
	"self run:#testAtPut"
	| c |
	c := #(1 2 3 4 ) asOrderedCollection.
	c at: 2 put: 5.
	self assert: c = #(1 5 3 4 ) asOrderedCollection.
	self
		should: [c at: 5 put: 8]
		raise: Error.
	self deny: c = #(1 5 3 4 8 ) asOrderedCollection