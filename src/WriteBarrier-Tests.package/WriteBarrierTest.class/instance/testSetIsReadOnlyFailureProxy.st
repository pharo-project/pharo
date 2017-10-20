tests - proxy
testSetIsReadOnlyFailureProxy
	self alwaysWritableObjects do: [ :each |
		self 
			testProxyObject: each 
			initialState: false 
			tuples: #( (true false false) (false false false) ) ]
	