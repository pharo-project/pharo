tests - object
testSetIsReadOnlyFailure
	self alwaysWritableObjects do: [ :each |
		self 
			testObject: each 
			initialState: false 
			tuples: #( (true false false) (false false false) ) ]
	