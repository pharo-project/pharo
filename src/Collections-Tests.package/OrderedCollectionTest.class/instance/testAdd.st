tests - adding
testAdd
	| l |
	l := #(1 2 3 4) asOrderedCollection.
	l add: 88.
	self assert: (l =  #(1 2 3 4 88) asOrderedCollection).
	l add: 99.
	self assert: (l =  #(1 2 3 4 88 99) asOrderedCollection). 

