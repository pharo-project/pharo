tests - swap
testSwapAdjacentStart
	|aList|
	aList := LinkedList with: 5 with: 4 with: 3 with: 2 with: 1.
	
	aList swap: 1 with: 2.
	self assert: (aList at: 1) equals: 4.
	self assert: (aList at: 2) equals: 5.
	self assert: aList first equals: 4.