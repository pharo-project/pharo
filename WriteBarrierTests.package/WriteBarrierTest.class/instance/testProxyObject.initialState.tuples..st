tests - helper
testProxyObject: object initialState: initialState tuples: tuples
	self 
		testObject: object 
		initialState: initialState 
		tuples: tuples 
		setReadOnlyBlock: [ :value | self mirror setIsReadOnlyObjectOf: object to: value ]