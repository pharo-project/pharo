tests - transforming
testNegated

	self assert: (0 @ 0) negated = (0 @ 0).
	self deny: (0 @ 1) negated = (0 @ 0).
	self deny: (1 @ 0) negated = (0 @ 0).
	self deny: (1 @ 1) negated = (0 @ 0).
	self assert: (1 @ 1) negated = (-1 @ -1).
	self deny: (-1 @ -1) negated = (-1 @ -1).
	self deny: (1 @ -1) negated = (-1 @ -1).
	self deny: (-1 @ 1) negated = (-1 @ -1).
	self assert: (-1 @ -1) negated = (1 @ 1).
	self assert: (1 @ -1) negated = (-1 @ 1).
	self assert: (-1 @ 1) negated = (1 @ -1)