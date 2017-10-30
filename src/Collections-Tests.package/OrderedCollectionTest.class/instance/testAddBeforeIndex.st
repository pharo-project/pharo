tests - public methods
testAddBeforeIndex
	"self run: #testAddBeforeIndex"
	| l |
	l := #(1 2 3 4) asOrderedCollection.
	l add: 77 beforeIndex: 1.
	self assert: (l =  #(77 1 2 3 4) asOrderedCollection).
	l add: 88 beforeIndex: 3.
	self assert: (l =  #(77 1 88 2 3 4) asOrderedCollection). 
	l add: 99 beforeIndex: l size+1.
	self assert: (l =  #(77 1 88 2 3 4 99) asOrderedCollection). 
	self should:[l add: 666 beforeIndex: 0] raise: Error.
	self should:[l add: 666 beforeIndex: l size+2] raise: Error.
	
	"Now make room by removing first two and last two elements,
	and see if the illegal bounds test still fails"
	(l first: 2) , (l last: 2) reversed do: [:e | l remove: e].
	self should:[l add: 666 beforeIndex: 0] raise: Error.
	self should:[l add: 666 beforeIndex: l size+2] raise: Error.

