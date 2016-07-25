tests - compare
testLiteralEqual

	self deny: (0.0 literalEqual: 0.0 negated).
	self deny: (-0.0 literalEqual: -0.0 negated).
	self deny: (0.0 literalEqual: -0.0).
