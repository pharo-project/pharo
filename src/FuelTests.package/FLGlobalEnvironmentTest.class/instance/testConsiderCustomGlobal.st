tests
testConsiderCustomGlobal
	"A custom global variable is treated as global if we explicitly specify that on serialization."
	
	self analyzer considerGlobal: #FLGlobalVariableForTesting.

	serializationEnvironment 
		at: #FLGlobalVariableForTesting put: 42.

	materializationEnvironment 
		at: #FLGlobalVariableForTesting put: 7.
	
	self assert: (self resultOfSerializeAndMaterialize: 42) equals: 7