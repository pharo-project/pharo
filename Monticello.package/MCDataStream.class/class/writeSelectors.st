initialize-release
writeSelectors
	WriteSelectors ifNil: [ self initializeTypeMap ].
	^ WriteSelectors