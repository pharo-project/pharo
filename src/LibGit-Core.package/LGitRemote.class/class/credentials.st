accessing
credentials
	^ Credentials ifNil: [ Credentials := SmallDictionary new ]