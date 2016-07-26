tests
testAddTime
	self assert: (aTime addTime: aTime) = (Time readFrom: '01:09:52' readStream)