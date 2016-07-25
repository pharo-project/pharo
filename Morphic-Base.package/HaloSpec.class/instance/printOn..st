printing
printOn: aStream
	"Add a textual printout representing the receiver to a stream"

	super printOn: aStream.
	aStream nextPutAll: ' (', addHandleSelector asString, ' ', iconSymbol asString, ')'