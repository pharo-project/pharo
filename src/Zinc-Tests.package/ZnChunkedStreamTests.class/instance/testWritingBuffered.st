testing
testWritingBuffered
	| data written read |
	data := String withAll: ($a to: $z), ($A to: $Z).
	written := String streamContents: [ :out |
		| stream |
		stream := ZnBufferedWriteStream on: (ZnChunkedWriteStream on: out).
		stream sizeBuffer: 16.
		stream nextPutAll: data.
		stream close ].
	read := (ZnChunkedReadStream on: written readStream) upToEnd.
	self assert: read equals: data