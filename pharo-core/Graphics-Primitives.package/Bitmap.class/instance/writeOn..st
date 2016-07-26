filing
writeOn: aStream 
	"Store the array of bits onto the argument, aStream. A leading byte of 16r80 identifies this as compressed by compressToByteArray (qv)."
	| b |
	aStream nextPut: 128.
	b := self compressToByteArray.
	aStream
		nextPutAll: (self encodeInt: b size);
		nextPutAll: b