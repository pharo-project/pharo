tests - arithmetic
testAbs

	self assert: (0 @ 0) abs = (0 @ 0).
	self assert: (1 @ 1) abs = (1 @ 1).
	self assert: (-1 @ -1) abs = (1 @ 1).
	self assert: (-1 @ 1) abs = (1 @ 1).
	self deny: (1 @ -1) abs = (1 @ -1).
	self deny: (-1 @ -1) abs = (-1 @ -1).
	self deny: (-1 @ 1) abs = (-1 @ 1).
	self deny: (1 @ -1) abs = (1 @ -1).