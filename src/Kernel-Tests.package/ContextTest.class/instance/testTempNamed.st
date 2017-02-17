tests
testTempNamed
	| oneTemp context |
	
	oneTemp := 1.
	self assert: (thisContext tempNamed: 'oneTemp') = oneTemp.
	
	context := self class contextWithTempForTesting.
	self assert: (context tempNamed: 'string') = 'test'
