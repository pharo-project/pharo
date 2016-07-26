accessing
multiLineMessages: anArray
	multiLineMessages := (self class compiler evaluate: anArray asString)