tests - adding
testAddFirst
	| l |
	l := #(1 2 3 4) asOrderedCollection.
	l addFirst: 88.
	self assert: (l =  #(88 1 2 3 4) asOrderedCollection).
	l addFirst: 99.
	self assert: (l =  #(99 88 1 2 3 4) asOrderedCollection). 

