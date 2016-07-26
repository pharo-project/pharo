tests-globals
testConsiderCustomGlobal
	"A custom global variable is treated as global by Fuel, when we explicitly specify this."
	
	| aPerson globalKey |
	
	globalKey := #FLGlobalVariableForTesting.
	self analyzer considerGlobal: globalKey.

	aPerson := FLPerson new.
	[ 
		Smalltalk globals at: globalKey put: aPerson.
		self assertSerializationIdentityOf: aPerson.
	] ensure: [ Smalltalk globals removeKey: globalKey ]
