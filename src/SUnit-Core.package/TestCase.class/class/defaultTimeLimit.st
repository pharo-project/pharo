accessing
defaultTimeLimit
	^DefaultTimeLimit ifNil: [DefaultTimeLimit := 1 minutes]