tests
testRounded
	self assert: (1999/1000s2) printString = '2.00s2'.
	self assert: (-1999/1000s2) printString = '-2.00s2'