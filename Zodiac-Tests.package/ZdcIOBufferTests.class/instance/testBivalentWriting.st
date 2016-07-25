testing
testBivalentWriting
	| data ioBuffer |
	data := 'some string'.
	
	ioBuffer := ZdcIOBuffer on: (String new: data size).
	ioBuffer next: data size putAll: data asByteArray startingAt: 1.
	self assert: ioBuffer contents equals: data.
	
	ioBuffer := ZdcIOBuffer on: (ByteArray new: data size).
	ioBuffer next: data size putAll: data startingAt: 1.
	self assert: ioBuffer contents equals: data asByteArray