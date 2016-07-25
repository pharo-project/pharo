tests - adding
testAddBefore

	| l |
	l := #(1 2 3 4) asOrderedCollection.
	l add: 88 before: 1.
	self assert: (l =  #(88 1 2 3 4) asOrderedCollection).
	l add: 99 before: 2.
	self assert: (l =  #(88 1 99 2 3 4) asOrderedCollection). 

