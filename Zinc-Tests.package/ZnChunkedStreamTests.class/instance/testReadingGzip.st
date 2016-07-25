testing
testReadingGzip
	| data gzipped chunked plain |
	data := String withAll: ($a to: $z), ($A to: $Z).
	gzipped := String streamContents: [ :stream |
		(GZipWriteStream on: stream) nextPutAll: data; close ].
	chunked := String streamContents: [ :stream |
		self nextPutAll: gzipped on: stream chunked: 16 ].
	plain := (GZipReadStream on: (ZnChunkedReadStream on: chunked readStream)) upToEnd.
	self assert: data equals: plain 