accessing
freeSpaceEnd
	"Return the current valid free space end index into the buffer (inclusive)"
	
	self isFull ifTrue: [ self bufferFullError ]. 
	^ buffer size