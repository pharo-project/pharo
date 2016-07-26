filing
writeUncompressedOn: aStream 
	"Store the array of bits onto the argument, aStream.
	(leading byte ~= 16r80) identifies this as raw bits (uncompressed)."

	aStream nextInt32Put: self size.
	aStream nextPutAll: self
