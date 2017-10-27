utils
paddedByteStringSize: numberOfChars 
	"64 bits for the header"
	| originalSize  |
	originalSize := (2 * Smalltalk wordSize) + (numberOfChars = 0 ifTrue:[1] ifFalse:[numberOfChars]).
		
	^ self align64Bits: originalSize.