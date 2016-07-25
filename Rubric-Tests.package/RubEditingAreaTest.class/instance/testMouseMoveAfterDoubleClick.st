tests-accessing selection
testMouseMoveAfterDoubleClick
	self sendMouseClickAt: 11 withShift: false.
	self sendDoubleClickAt: 11 withShift: false.
	self sendMouseClickAt: 17 withShift: true.
	self assert: area pointBlock stringIndex equals: 9.
	self assert: area markBlock stringIndex equals: 17.