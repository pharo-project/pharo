tests - public methods
testAddAfterIndex
	"self run: #testAddAfterIndex"
	| l |
	l := #(1 2 3 4) asOrderedCollection.
	l add: 77 afterIndex: 0.
	self assert: (l =  #(77 1 2 3 4) asOrderedCollection).
	l add: 88 afterIndex: 2.
	self assert: (l =  #(77 1 88 2 3 4) asOrderedCollection). 
	l add: 99 afterIndex: l size.
	self assert: (l =  #(77 1 88 2 3 4 99) asOrderedCollection). 
	self should:[l add: 666 afterIndex: -1] raise: Error.
	self should:[l add: 666 afterIndex: l size+1] raise: Error.
	
	"Now make room by removing first two and last two elements,
	and see if the illegal bounds test still fails"
	(l first: 2) , (l last: 2) reversed do: [:e | l remove: e].
	self should: [l add: 666 afterIndex: -1] raise: Error.
	self should: [l add: 666 afterIndex: l size+1] raise: Error.