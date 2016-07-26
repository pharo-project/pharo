caching
translucentPatterns
	TranslucentPatterns isNil ifTrue:[ self initializeTranslucentPatterns ].
	^ 	TranslucentPatterns  