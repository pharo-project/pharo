accessing
defaultTimeLimit
	^DefaultTimeLimit ifNil: [DefaultTimeLimit := 10 seconds]
