tests
testTempNamedPut
	| oneTemp |
	
	oneTemp := 1.
	self assert: (thisContext tempNamed: 'oneTemp') = oneTemp.
	thisContext tempNamed: 'oneTemp' put: 2.
	self assert: (thisContext tempNamed: 'oneTemp') = 2.