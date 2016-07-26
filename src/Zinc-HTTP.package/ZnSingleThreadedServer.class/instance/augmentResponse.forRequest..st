request handling
augmentResponse: response forRequest: request
	"Our handler has produced response for request, manipulate the response before writing it"
	
	(self useGzipCompressionAndChunking and: [ response contentType isBinary not ])
		ifTrue: [ 
			(request acceptsEncodingGzip and: [ response hasContentEncoding not ]) 
				ifTrue: [ response setContentEncodingGzip ].
			response hasTransferEncoding
				ifFalse: [ response setTransferEncodingChunked ] ]