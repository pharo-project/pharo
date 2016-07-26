testing
testGzipWriteRead
	| data written read |
	data := String streamContents: [ :out |
		{ Collection. Stream. Exception. Magnitude } do: [ :cls |
			cls allSubclassesDo: [ :each | out nextPutAll: each comment ] ] ].
	data := data asByteArray.
	written := ByteArray streamContents: [ :out |
		| stream chunkedStream |
		stream := GZipWriteStream on: (chunkedStream := ZnChunkedWriteStream on: out).
		stream nextPutAll: data.
		stream finish.
		chunkedStream finish ].
	read := (GZipReadStream on: (ZnChunkedReadStream on: written readStream)) upToEnd.
	self assert: read equals: data