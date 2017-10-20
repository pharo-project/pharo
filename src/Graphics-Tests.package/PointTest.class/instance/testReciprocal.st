tests - arithmetic
testReciprocal

	self assert: (1 @ 1) reciprocal = (1 @ 1).
	self deny: (2 @ 2) reciprocal = (2 @ 2).
	self assert: (1 @ 5) reciprocal = (1 @ (1 / 5)).
	self should: [ (0 @ 0) reciprocal ] raise: ZeroDivide.
	self should: [ (1 @ 0) reciprocal ] raise: ZeroDivide.
	self should: [ (0 @ 1) reciprocal ] raise: ZeroDivide