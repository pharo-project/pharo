tests - mathematical functions
testDegreeSin
	"self run: #testDegreeSin"

	45 degreeSin.	"Following tests use approximate equality, because sine are generally evaluated using inexact Floating point arithmetic"
	self assert: (45 degreeSin squared - (1 / 2)) abs <= Float epsilon.
	self assert: (30 degreeSin - (1 / 2)) abs <= Float epsilon.
	self assert: (-30 degreeSin + (1 / 2)) abs <= Float epsilon.
	-360 to: 360 do: [ :i | self assert: (i degreeSin closeTo: i degreesToRadians sin) ].	"Following tests use strict equality which is a requested property of degreeSin"
	-10 to: 10 do: [ :k | 
		self assert: (k * 360 + 90) degreeSin - 1 = 0.
		self assert: (k * 360 - 90) degreeSin + 1 = 0.
		self assert: (k * 360 + 180) degreeSin = 0.
		self assert: (k * 360) degreeSin = 0 ]