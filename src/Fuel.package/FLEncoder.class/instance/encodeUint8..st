encoding
encodeUint8: aSmallInteger 
	"Append to the receiver an Integer as the next two bytes."
	stream nextPut: aSmallInteger 