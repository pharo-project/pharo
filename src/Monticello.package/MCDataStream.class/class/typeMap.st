initialize-release
typeMap
	TypeMap ifNil: [ self initializeTypeMap ].
	^ TypeMap