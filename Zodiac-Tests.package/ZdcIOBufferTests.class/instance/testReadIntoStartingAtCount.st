testing
testReadIntoStartingAtCount
	| data ioBuffer string |
	data := 'abcdefghijkl'.
	ioBuffer := ZdcIOBuffer on: data.
	ioBuffer advanceWritePointer: 12.
	string := String new: 12.
	ioBuffer readInto: string startingAt: 1 count: 4.
	ioBuffer readInto: string startingAt: 5 count: 4.
	ioBuffer readInto: string startingAt: 9 count: 4.
	self assert: string equals: data