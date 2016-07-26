initialize-release
readSelectors
	ReadSelectors ifNil: [ self initializeTypeMap ].
	^ReadSelectors 