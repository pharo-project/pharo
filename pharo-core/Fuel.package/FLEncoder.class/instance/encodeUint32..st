encoding
encodeUint32: aSmallInteger 
	"Append to the receiver an Integer as the next 4 bytes."
	stream		
		nextPut: (aSmallInteger bitShift: -24);
		nextPut: ((aSmallInteger bitShift: -16) bitAnd: 255);
		nextPut: ((aSmallInteger bitShift: -8) bitAnd: 255);
		nextPut: (aSmallInteger bitAnd: 255)
