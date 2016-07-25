tests - mathematical functions
testDegreeCos
	"self run: #testDegreeCos"

	45 degreeCos.	"Following tests use approximate equality, because cosine are generally evaluated using inexact Floating point arithmetic"
	self assert: (45 degreeCos squared - (1 / 2)) abs <= Float epsilon.
	self assert: (60 degreeCos - (1 / 2)) abs <= Float epsilon.
	self assert: (120 degreeCos + (1 / 2)) abs <= Float epsilon.
	-360 to: 360 do: [ :i | self assert: (i degreeCos closeTo: i degreesToRadians cos) ].	"Following tests use strict equality which is a requested property of degreeCos"
	-10 to: 10 do: [ :k | 
		self assert: (k * 360 + 90) degreeCos = 0.
		self assert: (k * 360 - 90) degreeCos = 0.
		self assert: (k * 360 + 180) degreeCos + 1 = 0.
		self assert: (k * 360) degreeCos - 1 = 0 ]