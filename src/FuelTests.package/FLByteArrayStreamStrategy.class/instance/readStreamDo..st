reading
readStreamDo: aValuable
	"Evaluates the argument with a read stream. Answers the result."
	
	^aValuable value: inMemoryStream contents readStream