accessing
bufferSize
	"Return the raw size of the underlying buffer"
	
	buffer ifNil: [ ^ 0 ].
	^ buffer size