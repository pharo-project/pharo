accessing
byteAt: byteOffset put: value
	"Go through a different primitive since the receiver describes data in the outside world"
	^self unsignedByteAt: byteOffset put: value