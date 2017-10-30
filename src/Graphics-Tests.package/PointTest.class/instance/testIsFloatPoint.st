tests - truncation and roundoff
testIsFloatPoint

	self deny: (1 @ 1) isFloatPoint.
	self deny: (-1 @ 1) isFloatPoint.
	self deny: (1 @ -1) isFloatPoint.
	self deny: (-1 @ -1) isFloatPoint.
	
	self deny: (1 @ 1.2) isFloatPoint.
	self deny: (1.2 @ 1) isFloatPoint.
	self assert: (1.2 @ 1.2) isFloatPoint.
	
	self deny: (-1 @ -1.2) isFloatPoint.
	self deny: (-1.2 @ -1) isFloatPoint.
	self assert: (-1.2 @ -1.2) isFloatPoint.
	
	self deny: (0 @ 0) isFloatPoint.
	self deny: (0.0 @ 0) isFloatPoint.
	self deny: (0 @ 0.0) isFloatPoint.
	self assert: (0.0 @ 0.0) isFloatPoint.
	