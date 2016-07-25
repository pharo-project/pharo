tests
testCollections
	| collections |
	collections := STON listClass withAll: {
		#(1 2 3).
		OrderedCollection withAll: #(1 2 3).
		Set withAll: #(1 2 3).
		IdentitySet withAll: #(1 2 3).
		Bag withAll: #(1 2 2 3).
		Dictionary new at: 1 put: 1; at: 2 put: 2; yourself.
		IdentityDictionary new at: 1 put: 1; at: 2 put: 2; yourself.
		#[1 2 3].
		#(1 2 3) asIntegerArray.
		#(1 2 3) asFloatArray }.
	self serializeAndMaterialize: collections