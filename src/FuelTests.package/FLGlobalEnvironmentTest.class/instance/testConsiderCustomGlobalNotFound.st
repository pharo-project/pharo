tests
testConsiderCustomGlobalNotFound
	"A custom global variable is treated as global if we explicitly specify that on serialization. Then, if the variable is not defined in the materialization environment, a error should be raised."
	
	self analyzer considerGlobal: #FLGlobalVariableForTesting.

	serializationEnvironment 
		at: #FLGlobalVariableForTesting put: 42.

	self serialize: 42.
	self 
		should: [ self materialization ] 
		raise: FLGlobalNotFound 
		description: 'Raise an error since the variable is not present in materialization global environment'