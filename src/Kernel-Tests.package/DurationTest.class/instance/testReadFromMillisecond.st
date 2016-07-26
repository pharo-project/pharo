tests
testReadFromMillisecond

	self assert: (Duration readFrom: '0:00:00:00.001 ' readStream) nanoSeconds = 1000000