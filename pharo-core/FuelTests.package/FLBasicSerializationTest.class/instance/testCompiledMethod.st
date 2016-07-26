tests-globals
testCompiledMethod
	"They should be considered as globals by default."
	
	self assertSerializationIdentityOf: FLPair >> #left
