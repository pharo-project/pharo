tests - puting with indexes
testAtLastPut
	| result index |
	index := self indexArray anyOne.
	result := self nonEmpty atLast: index  put: self aValue.
	
	self assert: (self nonEmpty at: (self nonEmpty size +1 - index)) = self aValue .