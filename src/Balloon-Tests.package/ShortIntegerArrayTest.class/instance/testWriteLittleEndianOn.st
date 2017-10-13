tests
testWriteLittleEndianOn

	|stream|
	stream := WriteStream on: Array new.
	shortIntegerArray write: (12 bitShift: 24) +(34 bitShift: 16)+(56 bitShift: 8)+78 littleEndianOn: stream.
	self assert: #(56 78 12 34) equals: stream contents