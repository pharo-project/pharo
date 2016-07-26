user interface
defaultValue
	^ self default value ifNil: [self domainValues first realValue]
