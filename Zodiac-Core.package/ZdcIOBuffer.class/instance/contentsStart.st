accessing
contentsStart
	"Return the current valid contents start index into the buffer (inclusive)"
	
	self isEmpty ifTrue: [ self bufferEmptyError ].
	^ readPointer + 1