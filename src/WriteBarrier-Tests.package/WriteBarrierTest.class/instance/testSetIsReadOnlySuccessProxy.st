tests - proxy
testSetIsReadOnlySuccessProxy
	self maybeReadOnlyObjects do: [ :each |
		self 
			testProxyObject: each 
			initialState: false 
			tuples: #( (true false true) (false true false) ) ]
	