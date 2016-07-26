reading
readStreamDo: aValuable
	"Evaluates the argument with a read stream. Answers the result."
	
	targetStrategy readStreamDo: [:aStream |
		^ GZipReadStream with: aStream do: aValuable]
