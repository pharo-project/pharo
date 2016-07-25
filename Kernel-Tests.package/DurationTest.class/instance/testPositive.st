tests
testPositive
	self assert: (Duration nanoSeconds: 0) positive.
	self assert: aDuration positive.
	self deny: aDuration negated positive
