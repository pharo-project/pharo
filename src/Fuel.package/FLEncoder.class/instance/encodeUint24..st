encoding
encodeUint24: aSmallInteger 
	"Append to the receiver an Integer as the next 3 bytes."
	stream		
		nextPut: ((aSmallInteger bitShift: -16) bitAnd: 255);
		nextPut: ((aSmallInteger bitShift: -8) bitAnd: 255);
		nextPut: (aSmallInteger bitAnd: 255)

