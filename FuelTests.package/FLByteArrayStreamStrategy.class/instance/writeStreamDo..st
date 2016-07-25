writing
writeStreamDo: aValuable
	"Evaluates the argument with a write stream. Answers the result."
	
	inMemoryStream := ByteArray new writeStream.
	^aValuable value: inMemoryStream binary
