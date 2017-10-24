tests
testWriteBigEndianOn

	|stream|
	stream := WriteStream on: Array new.
	shortIntegerArray write: (12 bitShift: 24)+(34 bitShift: 16)+(56 bitShift: 8)+78 bigEndianOn: stream.
	self assert: #(12 34 56 78) equals: stream contents