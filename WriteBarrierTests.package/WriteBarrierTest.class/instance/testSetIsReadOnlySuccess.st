tests - object
testSetIsReadOnlySuccess
	self maybeReadOnlyObjects do: [ :each |
		self 
			testObject: each 
			initialState: false 
			tuples: #( (true false true) (false true false) ) ]
	