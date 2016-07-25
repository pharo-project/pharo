accessing
contentString: aString 
	aString ifNil: [self removeProperty: #contentString]
		ifNotNil: [self setProperty: #contentString toValue: aString]