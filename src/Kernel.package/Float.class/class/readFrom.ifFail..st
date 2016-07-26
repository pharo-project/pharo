instance creation
readFrom: aStream ifFail: aBlock
	"Answer a new Float as described on the stream, aStream."

	^(super readFrom: aStream ifFail: [^aBlock value]) asFloat