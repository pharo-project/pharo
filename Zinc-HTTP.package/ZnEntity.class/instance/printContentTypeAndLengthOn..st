printing
printContentTypeAndLengthOn: stream
	self hasContentType ifTrue: [ 
		stream print: self contentType; space ].
	self hasContentLength ifTrue: [
		stream print: self contentLength; nextPut: $B ]