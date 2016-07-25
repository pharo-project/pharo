tests
testLessThan
	self assert: aTime < (Time readFrom: '12:34:57' readStream)