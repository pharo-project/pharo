private
flushBufferIfFull
	position = self bufferSize 
		ifTrue: [ self flushBuffer ]
