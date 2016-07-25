tests - proxy
testSetIsReadOnlyImmediateProxy
	self alwaysReadOnlyObjects do: [ :each |
		self 
			testProxyObject: each 
			initialState: true 
			tuples: #( (true true true) (false true true) ) ]
	