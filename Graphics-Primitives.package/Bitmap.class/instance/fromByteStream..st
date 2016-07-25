initialize
fromByteStream: aStream 
	"Initialize the array of bits by reading integers from the argument, 
	aStream."
	aStream nextWordsInto: self