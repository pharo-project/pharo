printing
printOn: aStream
	"Reimplemented to add a tag showing that the receiver is currently functioning as a 'world', if it is"

	super printOn: aStream.
	aStream nextPutAll: ' [world]'