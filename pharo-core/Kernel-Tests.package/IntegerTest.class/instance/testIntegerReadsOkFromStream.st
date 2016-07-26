tests - instance creation
testIntegerReadsOkFromStream
	self assert: (Integer readFrom: '123' readStream) = 123.
	self assert: (Integer readFrom: '-123' readStream) = -123.
	self assert: (Integer readFrom: 'a3' readStream base: 16) = 163.
	self assert: (Integer readFrom: '-a3' readStream base: 16) = -163.
	
	self assert: (Integer readFrom: '3a' readStream base: 10) = 3.