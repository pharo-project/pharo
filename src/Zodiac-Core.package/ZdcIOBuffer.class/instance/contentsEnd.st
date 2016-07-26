accessing
contentsEnd
	"Return the current valid contents end index into the buffer (inclusive)"
	
	self isEmpty ifTrue: [ self bufferEmptyError ].
	^ writePointer