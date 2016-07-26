testing
testBinaryWriteRead
	| data written read |
	data := ByteArray new: 1024 streamContents: [ :out |
		1024 timesRepeat: [ out nextPut: 256 atRandom - 1 ] ].
	written := ByteArray streamContents: [ :out |
		| stream |
		stream := ZnBufferedWriteStream on: (ZnChunkedWriteStream on: out).
		stream sizeBuffer: 256.
		1 to: 1024 by: 256 do: [ :each |
			stream next: 256 putAll: data startingAt: each ].
		stream close ].
	read := (ZnChunkedReadStream on: written readStream) upToEnd.
	self assert: read equals: data