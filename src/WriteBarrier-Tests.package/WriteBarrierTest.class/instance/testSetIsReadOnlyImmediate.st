tests - object
testSetIsReadOnlyImmediate
	self alwaysReadOnlyObjects do: [ :each |
		self 
			testObject: each 
			initialState: true 
			tuples: #( (true true true) (false true true) ) ]
	