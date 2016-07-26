tests - adding
testAddLast
	| l |
	l := #(1 2 3 4) asOrderedCollection.
	l addLast: 88.
	self assert: (l =  #(1 2 3 4 88) asOrderedCollection).
	l addLast: 99.
	self assert: (l =  #(1 2 3 4 88 99) asOrderedCollection). 

