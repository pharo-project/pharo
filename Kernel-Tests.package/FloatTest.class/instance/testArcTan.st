tests - mathematical functions
testArcTan

	self assert: ((100 arcTan: 100) closeTo: Float pi / 4).
	self assert: ((-100 arcTan: 100) closeTo: Float pi / -4).
	self assert: ((100 arcTan: -100) closeTo: Float pi * 3 / 4).
	self assert: ((-100 arcTan: -100) closeTo: Float pi * -3 / 4).
	self assert: ((0 arcTan: 100) closeTo: 0).
	self assert: ((0 arcTan: -100) closeTo: Float pi).
	self assert: ((100 arcTan: 0) closeTo: Float pi / 2).
	self assert: ((-100 arcTan: 0) closeTo: Float pi / -2).
	
	self assert: ((Float negativeZero arcTan: 100) closeTo: 0).
	self assert: ((Float negativeZero arcTan: -100) closeTo: Float pi * -1).
	
	self assert: (0 arcTan: 0) = 0.
	self assert: (Float negativeZero arcTan: 0) = 0.
	self assert: ((0 arcTan: Float negativeZero) closeTo: Float pi).
	self assert: ((Float negativeZero arcTan: Float negativeZero) closeTo: Float pi negated). 