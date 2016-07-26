testing
testReadingGzipStepped
	| data gzipped chunked unchunked ungzipped |
	data := String withAll: ($a to: $z), ($A to: $Z).
	gzipped := String streamContents: [ :stream |
		(GZipWriteStream on: stream) nextPutAll: data; close ].
	chunked := String streamContents: [ :stream |
		self nextPutAll: gzipped on: stream chunked: 16 ].
	unchunked := (ZnChunkedReadStream on: chunked readStream) upToEnd.
	ungzipped := (GZipReadStream on: unchunked) upToEnd.
	self assert: gzipped equals: unchunked.
	self assert: data equals: ungzipped 