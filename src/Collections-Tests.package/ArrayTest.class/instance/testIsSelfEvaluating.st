tests
testIsSelfEvaluating

	self assert: example1 isSelfEvaluating.
	example1 at: 1 put: Bag new.
	self deny: example1 isSelfEvaluating.
	example1 at: 1 put: 1.