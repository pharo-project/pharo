tests
testLessThan
	self assert: aDuration  < (aDuration + 1 day ).
	self deny: aDuration < aDuration.
	