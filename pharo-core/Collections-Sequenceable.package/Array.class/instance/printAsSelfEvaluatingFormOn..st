self evaluating
printAsSelfEvaluatingFormOn: aStream

	aStream nextPut: ${.
	self do: [:el | el printOn: aStream] separatedBy: [ aStream nextPutAll: '. '].
	aStream nextPut: $}