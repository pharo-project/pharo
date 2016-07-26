instance creation
readFrom: aStream 
	"Answer a new Float as described on the stream, aStream."

	^(super readFrom: aStream) asFloat