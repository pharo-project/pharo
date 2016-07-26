public
writeEntity: entity
	| actualStream gzipStream bufferedStream chunkedStream isChunked |
	actualStream := stream.
	chunkedStream := gzipStream := bufferedStream := nil.
	(isChunked := self isChunked)
		ifTrue: [ 
			chunkedStream := actualStream := ZnChunkedWriteStream on: actualStream ].
	self isGzipped 
		ifTrue: [ 
			gzipStream := actualStream := GZipWriteStream on: actualStream ]
		ifFalse: [ 
			(isChunked and: [ self contentType isBinary not ])
				ifTrue: [ bufferedStream := actualStream := ZnUtils bufferedWriteStreamOn: actualStream ] ].
	entity writeOn: actualStream.
	gzipStream 
		ifNotNil: [ gzipStream finish ].
	bufferedStream 
		ifNotNil: [ bufferedStream finish ].
	chunkedStream
		ifNotNil: [ chunkedStream finish ]
	