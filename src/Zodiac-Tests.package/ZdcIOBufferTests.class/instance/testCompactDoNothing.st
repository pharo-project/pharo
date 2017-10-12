tests - compact
testCompactDoNothing
	| ioBuffer |
	ioBuffer := ZdcIOBuffer on: (ByteArray new: 10).
	self assert: ioBuffer isEmpty.
	self assert: ioBuffer availableForReading = 0.
	self assert: ioBuffer availableForWriting = 10.
	ioBuffer compact.
	self assert: ioBuffer isEmpty.
	self assert: ioBuffer availableForReading = 0.
	self assert: ioBuffer availableForWriting = 10.	    
	1 to: 6 do: [ :each | ioBuffer nextPut: each ].	
	self assert: ioBuffer isEmpty not.
	self assert: ioBuffer availableForReading = 6.
	self assert: ioBuffer availableForWriting = 4.
	ioBuffer compact.
	self assert: ioBuffer isEmpty not.
	self assert: ioBuffer availableForReading = 6.
	self assert: ioBuffer availableForWriting = 4.