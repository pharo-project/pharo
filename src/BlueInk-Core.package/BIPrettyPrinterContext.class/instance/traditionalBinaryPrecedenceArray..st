accessing
traditionalBinaryPrecedenceArray: anArray
	traditionalBinaryPrecedenceArray := (self class compiler evaluate: anArray asString)