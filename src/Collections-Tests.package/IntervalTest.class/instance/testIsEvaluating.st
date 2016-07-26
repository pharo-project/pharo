tests
testIsEvaluating
	self assert: (1 to: 10) isSelfEvaluating.
	self assert: (1 to: 10 by: 2) isSelfEvaluating