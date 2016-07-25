tests - swap
testSwapAdjacentEnd
	|aList|
	aList := LinkedList with: 5 with: 4 with: 3 with: 2 with: 1.
	
	aList swap: 4 with: 5.
	self assert: (aList at: 4) equals: 1.
	self assert: (aList at: 5) equals: 2.
	self assert: aList last equals: 2.