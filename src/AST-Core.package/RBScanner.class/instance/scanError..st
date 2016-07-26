private-scanning
scanError: theCause
	| location |
	"error location is the next not parseable character. Or current stream position + 1 if 
	an expected character is missing."
	location := stream position.
	currentCharacter
		ifNotNil: [ :char | buffer nextPut: char ]
		ifNil: [ location := stream position + 1 ].
	^ RBErrorToken
		value: buffer contents asString
		start: tokenStart
		cause: theCause
		location: location