tests - sorting
testIsSortedBy
	self assert: (self sortedInAscendingOrderCollection isSortedBy: [:a :b | a<b]).
	self deny: (self sortedInAscendingOrderCollection isSortedBy: [:a :b | a>b]).
