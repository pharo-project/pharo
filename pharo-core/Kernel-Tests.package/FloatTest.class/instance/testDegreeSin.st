tests - mathematical functions
testDegreeSin
	45.0 degreeSin.	"Following tests use approximate equality, because sine are generally evaluated using inexact Floating point arithmetic"
	self assert: (45.0 degreeSin squared - 0.5) abs <= Float epsilon.
	self assert: (30.0 degreeSin - 0.5) abs <= Float epsilon.
	self assert: (-30.0 degreeSin + 0.5) abs <= Float epsilon.
	-360.0 to: 360.0 do: [ :i | self assert: (i degreeSin closeTo: i degreesToRadians sin) ].	"Following tests use strict equality which is a requested property of degreeSin"
	-10.0 to: 10.0 do: [ :k | 
		self assert: (k * 360 + 90) degreeSin - 1 = 0.
		self assert: (k * 360 - 90) degreeSin + 1 = 0.
		self assert: (k * 360 + 180) degreeSin = 0.
		self assert: (k * 360) degreeSin = 0 ]