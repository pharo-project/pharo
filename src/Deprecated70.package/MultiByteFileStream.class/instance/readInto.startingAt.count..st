accessing
readInto: buffer startingAt: offset count: requestedCount
	"Read up to requestedCount elements into the given buffer starting at offset.
	Return the number of elements actually read.
	If I am binary or if buffer is a ByteArray, I skip decoding. Yes this is weird.
	This is a necessarily inefficient implementation, reading and decoding characters one by one."

	(self isBinary or: [ buffer class == ByteArray ])
		ifTrue: [ ^ super readInto: buffer startingAt: offset count: requestedCount ].
	0 to: requestedCount - 1 do: [ :count | 
		| element |
		(element := self next) ifNil: [ ^ count ].
		buffer at: offset + count put: element ].
	^ requestedCount