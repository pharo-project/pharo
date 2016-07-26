tests
testReadFrom
	self assert: aDuration = (Duration readFrom: '1:02:03:04.000000005' readStream)