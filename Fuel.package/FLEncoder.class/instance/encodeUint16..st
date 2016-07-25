encoding
encodeUint16: aSmallInteger 
	"Append to the receiver an Integer as the next two bytes."
	stream nextPut: (aSmallInteger bitShift: -8).
	stream nextPut: (aSmallInteger bitAnd: 255).