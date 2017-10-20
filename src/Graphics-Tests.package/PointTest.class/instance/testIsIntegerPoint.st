tests - truncation and roundoff
testIsIntegerPoint

	self assert: (1 @ 1) isIntegerPoint.
	self assert: (-1 @ 1) isIntegerPoint.
	self assert: (1 @ -1) isIntegerPoint.
	self assert: (-1 @ -1) isIntegerPoint.
	
	self deny: (1 @ 1.2) isIntegerPoint.
	self deny: (1.2 @ 1) isIntegerPoint.
	self deny: (1.2 @ 1.2) isIntegerPoint.
	
	self deny: (-1 @ -1.2) isIntegerPoint.
	self deny: (-1.2 @ -1) isIntegerPoint.
	self deny: (-1.2 @ -1.2) isIntegerPoint.
	