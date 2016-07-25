conversion
mimeDecodeToByteArray
	"Convert a stream in base 64 with only a-z,A-Z,0-9,+,/ to a full ByteArray of 0-255 values.  Reutrn a whole stream for the user to read."

	| nibA nibB nibC nibD |
	[mimeStream atEnd] whileFalse: [
		(nibA := self nextValue) ifNil: [^ dataStream].
		(nibB := self nextValue) ifNil: [^ dataStream].
		dataStream nextPut: ((nibA bitShift: 2) + (nibB bitShift: -4)).
		nibB := nibB bitAnd: 16rF.
		(nibC := self nextValue) ifNil: [^ dataStream].
		dataStream nextPut: ((nibB bitShift: 4) + (nibC bitShift: -2)).
		nibC := nibC bitAnd: 16r3.
		(nibD := self nextValue) ifNil: [^ dataStream].
		dataStream nextPut: ((nibC bitShift: 6) + nibD).
		].
	^ dataStream