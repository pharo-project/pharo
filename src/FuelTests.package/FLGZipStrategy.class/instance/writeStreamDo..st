writing
writeStreamDo: aValuable
	"Evaluates the argument with a write stream. Answers the result."
	
	targetStrategy writeStreamDo: [:aStream |
		^ GZipWriteStream with: aStream do: aValuable]
