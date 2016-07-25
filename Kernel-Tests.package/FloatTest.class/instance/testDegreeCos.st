tests - mathematical functions
testDegreeCos
	45.0 degreeCos.	"Following tests use approximate equality, because cosine are generally evaluated using inexact Floating point arithmetic"
	self assert: (45.0 degreeCos squared - 0.5) abs <= Float epsilon.
	self assert: (60.0 degreeCos - 0.5) abs <= Float epsilon.
	self assert: (120.0 degreeCos + 0.5) abs <= Float epsilon.
	-360.0 to: 360.0 do: [ :i | self assert: (i degreeCos closeTo: i degreesToRadians cos) ].	"Following tests use strict equality which is a requested property of degreeCos"
	-10.0 to: 10.0 do: [ :k | 
		self assert: (k * 360 + 90) degreeCos = 0.
		self assert: (k * 360 - 90) degreeCos = 0.
		self assert: (k * 360 + 180) degreeCos + 1 = 0.
		self assert: (k * 360) degreeCos - 1 = 0 ]