accessing
typeTable
	TypeTable ifNil: [self initializeTypeTable].
	^ TypeTable