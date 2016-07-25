accessing
bufferSize
	^ buffer isNil 
		ifTrue: [ self defaultBufferSize ]
		ifFalse: [ buffer size ]