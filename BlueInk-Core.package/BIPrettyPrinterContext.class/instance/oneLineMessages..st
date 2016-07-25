accessing
oneLineMessages: anArray
	oneLineMessages := (self class compiler evaluate: anArray asString)