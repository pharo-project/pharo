testing
testReadingBuffered
	| data chunked plain buffer readStream |
	data := String withAll: ($a to: $z), ($A to: $Z).
	chunked := String streamContents: [ :stream |
		self nextPutAll: data on: stream chunked: 16 ].
	readStream := ZnChunkedReadStream on: chunked readStream.
	buffer := String new: 11.
	plain := String streamContents: [ :output | | readCount |
		[ readStream atEnd ] whileFalse: [
			readCount := readStream readInto: buffer startingAt: 1 count: buffer size.
			output next: readCount putAll: buffer ] ].
	self assert: plain equals: data