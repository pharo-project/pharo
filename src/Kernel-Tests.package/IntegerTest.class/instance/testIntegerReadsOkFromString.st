tests - instance creation
testIntegerReadsOkFromString
	self assert: (Integer readFrom: '123') = 123.
	self assert: (Integer readFrom: '-123') = -123.
	self assert: (Integer readFrom: 'a3' base: 16) = 163.
	self assert: (Integer readFrom: '-a3' base: 16) = -163.