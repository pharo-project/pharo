testing
testBivalentReading
	| data ioBuffer buffer |
	data := 'some string'.
	
	ioBuffer := ZdcIOBuffer on: data.
	ioBuffer advanceWritePointer: data size.
	buffer := ByteArray new: data size.
	ioBuffer readInto: buffer startingAt: 1 count: data size.
	self assert: buffer equals: data asByteArray.
	
	ioBuffer := ZdcIOBuffer on: data asByteArray.
	ioBuffer advanceWritePointer: data size.
	buffer := String new: data size.
	ioBuffer readInto: buffer startingAt: 1 count: data size.
	self assert: buffer equals: data.
