writing
writeStreamDo: aValuable
	"Evaluates the argument with a write stream. Answers the result."

	inMemoryStream := MultiByteBinaryOrTextStream on: ''.
	^aValuable value: inMemoryStream binary
