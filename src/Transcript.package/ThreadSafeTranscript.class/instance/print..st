streaming
print: anObject
	"Output anObject asString on the receiver, buffered, not yet shown"
	
	self nextPutAll: anObject asString