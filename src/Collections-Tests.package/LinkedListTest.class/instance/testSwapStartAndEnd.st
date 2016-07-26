tests - swap
testSwapStartAndEnd
	|aList|
	aList := LinkedList with: 5 with: 4 with: 3 with: 2 with: 1.
	
	aList swap: 1 with: 5.
	self assert: (aList at: 1) equals: 1.
	self assert: (aList at: 5) equals: 5.
	self assert: aList first equals: 1.
	self assert: aList last equals: 5.