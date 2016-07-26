printing
printOn: aStream
	"Print my contents in hex with a leading 'R' to show that it is a register object being printed."

	aStream nextPutAll: 'R:'.
	self asInteger storeOn: aStream base: 16.
