tests - testing
testBearingToPoint
	self assert: (0 @ 0 bearingToPoint: 0 @ 0) = 0.
	self assert: (0 @ 0 bearingToPoint: 0 @ -1) = 0.
	self assert: (0 @ 0 bearingToPoint: 1 @ 0) = 90.
	self assert: (0 @ 0 bearingToPoint: 0 @ 1) = 180.
	self assert: (0 @ 0 bearingToPoint: -1 @ 0) = 270.
	self assert: (0 @ 0 bearingToPoint: 1 @ 1) = 135.
	self assert: (0 @ 0 bearingToPoint: 0.01 @ 0) = 90.
	self assert: (0 @ 0 bearingToPoint: -2 @ -3) = 326.
	self assert: (0 @ 0 bearingToPoint: -0 @ 0) = 0.
	self assert: (-2 @ -3 bearingToPoint: 0 @ 0) = 146