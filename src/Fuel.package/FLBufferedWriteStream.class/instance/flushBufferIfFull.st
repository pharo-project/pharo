private
flushBufferIfFull
	position = buffer size 
		ifTrue: [ self flushBuffer ]
