tests-accessing selection
testMouseClickShiftClick
	self sendMouseClickAt: 3 withShift: false.
	self sendMouseClickAt: 6 withShift: true.
	self assert: area pointBlock stringIndex equals: 3.
	self assert: area markBlock stringIndex equals: 6