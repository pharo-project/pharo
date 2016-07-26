tests
testIntegerDivision
	self assert: aDuration // aDuration = 1. 
	self assert: aDuration // 2 =  (aDuration / 2). 
	"is there ever a case where this is not true, since precision is always to the nano second?"