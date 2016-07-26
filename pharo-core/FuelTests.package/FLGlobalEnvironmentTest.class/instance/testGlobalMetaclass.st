tests
testGlobalMetaclass
	"A metaclass should be global by default. On materialization, the global symbol should be found in the global environment."

	materializationEnvironment 
		at: #Integer 
		put: FLPerson.

	self assert: (self resultOfSerializeAndMaterialize: Integer class) == FLPerson class