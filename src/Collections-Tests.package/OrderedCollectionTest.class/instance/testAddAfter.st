tests - adding
testAddAfter

	| l |
	l := #(1 2 3 4) asOrderedCollection.
	l add: 88 after: 1.
	self assert: (l =  #(1 88 2 3 4) asOrderedCollection).
	l add: 99 after: 2.
	self assert: (l =  #(1 88 2 99 3 4) asOrderedCollection). 

