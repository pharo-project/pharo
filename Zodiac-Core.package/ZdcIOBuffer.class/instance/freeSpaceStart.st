accessing
freeSpaceStart
	"Return the current valid free space start index into the buffer (inclusive)"
	
	self isFull ifTrue: [ self bufferFullError ]. 
	^ writePointer + 1