accessing
defaultTimeLimit
	^DefaultTimeLimit ifNil: [DefaultTimeLimit := 2 seconds]