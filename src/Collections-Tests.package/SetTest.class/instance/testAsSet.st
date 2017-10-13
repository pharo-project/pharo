tests - some
testAsSet
	"could be moved in Array or Collection"

	| newFull |
	newFull := #(1 2 3 ) asSet.
	newFull add: 4.
	self assert: (newFull = full).