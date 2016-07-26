tests
testSubtractTime
	self assert: (aTime subtractTime: aTime) = (Time readFrom: '00:00:00' readStream)