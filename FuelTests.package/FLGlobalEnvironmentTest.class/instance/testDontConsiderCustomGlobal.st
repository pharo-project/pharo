tests
testDontConsiderCustomGlobal
	"A custom global variable is not treated as global unless we explicitly specify during serialization."
	
	| anObject |
	anObject := Object new.
	serializationEnvironment 
		at: #FLGlobalVariableForTesting put: anObject.
	materializationEnvironment 
		at: #FLGlobalVariableForTesting put: anObject;
		at: #Object put: Object.
	
	self deny: (self analyzer globalSymbols includes: #FLGlobalVariableForTesting).
	self deny: (self resultOfSerializeAndMaterialize: anObject) == anObject