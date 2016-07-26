chunks
processIDATChunk

	interlaceMethod = 0
		ifTrue: [ self processNonInterlaced ]
		ifFalse: [ self processInterlaced ]
