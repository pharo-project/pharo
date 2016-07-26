tests
testName
	| info |
	info := MBInfo new.
	self assert: (info name isNil).

	info name: 'foobar'.
	self assert: (info name = 'foobar').
	self assert: (info printString = 'foobar').

