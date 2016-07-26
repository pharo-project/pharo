tests - testing
testCloseTo

	| x y |
	x := 0.00001000001.
	y := 0.0000100000001.

	self assert: (x closeTo: 0.00001).
	self assert: (y closeTo: 0.00001).
	
	self assert: (x@y closeTo: (0.00001@0.00001))