utils
paddedByteStringSize: numberOfChars 
	"2 words for the header"
	| originalSize |
	originalSize := (2 * Smalltalk wordSize) + numberOfChars.
	^ ((originalSize % Smalltalk wordSize) = 0) ifTrue:[ ^ originalSize] ifFalse:[
		originalSize + Smalltalk wordSize - (originalSize % Smalltalk wordSize)
	]. 
