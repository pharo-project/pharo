private
nextPutAll: collection on: stream chunked: size
	| written |
	written := 0.
	[ written < collection size ] whileTrue: [ | chunkSize |
		chunkSize := size min: (collection size - written).
		chunkSize printOn: stream base: 16.
		stream nextPutAll: String crlf.
		chunkSize timesRepeat: [ stream nextPut: (collection at: (written := written + 1)) ].
		stream nextPutAll: String crlf ].
	stream print: 0; nextPutAll: String crlf; nextPutAll: String crlf