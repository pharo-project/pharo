tests - sorting
testIsSorted
	self assert: self sortedInAscendingOrderCollection isSorted.
	self deny: self unsortedCollection isSorted