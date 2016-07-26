tests - adding
testAddBeforeAndRemove

	| l initialCollection |
	l := #(1 2 3 4) asOrderedCollection.
	initialCollection := l shallowCopy.
	l add: 88 before: 1.
	self assert: (l =  #(88 1 2 3 4) asOrderedCollection).
	l add: 99 before: 2.
	self assert: (l =  #(88 1 99 2 3 4) asOrderedCollection). 
	l remove: 99.
	l remove: 88.
	self assert: l = initialCollection.

