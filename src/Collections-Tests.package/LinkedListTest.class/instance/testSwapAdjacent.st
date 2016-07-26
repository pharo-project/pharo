tests - swap
testSwapAdjacent
	|aList|
	aList := LinkedList with: 5 with: 4 with: 3 with: 2 with: 1.
	
	aList swap: 3 with: 4.
	self assert: (aList at: 3) equals: 2.
	self assert: (aList at: 4) equals: 3.