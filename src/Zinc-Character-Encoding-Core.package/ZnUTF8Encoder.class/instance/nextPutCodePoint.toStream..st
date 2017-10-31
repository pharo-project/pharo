converting
nextPutCodePoint: codePoint toStream: stream
	"Write the encoding for Integer code point to stream"

	codePoint < 128 ifTrue: [ 
		^ stream nextPut: codePoint ].
	codePoint < 2048 ifTrue: [ 
		^ stream 
			nextPut: (2r11000000 + (codePoint bitShift: -6)); 
			nextPut: (2r10000000 + (codePoint bitAnd: 2r111111)) ].
	(self isSurrogateCodePoint: codePoint) ifTrue: [ ^ self errorOutsideRange ].
	codePoint < 65536 ifTrue: [ 
		^ stream 
			nextPut: (2r11100000 + (codePoint bitShift: -12));
			nextPut: (2r10000000 + ((codePoint bitShift: -6) bitAnd: 2r111111)); 
			nextPut: (2r10000000 + (codePoint bitAnd: 2r111111)) ].
	codePoint <= self maximumUTFCode ifTrue: [ 
		^ stream 
			nextPut: (2r11110000 + (codePoint bitShift: -18));
			nextPut: (2r10000000 + ((codePoint bitShift: -12) bitAnd: 2r111111));
			nextPut: (2r10000000 + ((codePoint bitShift: -6) bitAnd: 2r111111)); 
			nextPut: (2r10000000 + (codePoint bitAnd: 2r111111)) ].
	^ self errorOutsideRange
