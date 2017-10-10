tests - helper
testObject: object initialState: initialState tuples: tuples
	self 
		testObject: object 
		initialState: initialState 
		tuples: tuples 
		setReadOnlyBlock: [ :value | object setIsReadOnlyObject: value ]