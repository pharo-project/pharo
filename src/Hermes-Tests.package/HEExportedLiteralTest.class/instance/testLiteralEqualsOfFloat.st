tests
testLiteralEqualsOfFloat
	self assert: ((self export:0.0) literalEqual: 0.0).
	self assert: ((self export:-0.0) literalEqual: 0.0 negated).

	self deny: ((self export:0.0) literalEqual: 0.0 negated).
	self deny: ((self export:-0.0) literalEqual: -0.0 negated).
	self deny: ((self export:0.0) literalEqual: -0.0).


	self assert: ((self export:3.0) literalEqual: 3.0).
	self assert: ((self export:-2.5) literalEqual: -2.5).
