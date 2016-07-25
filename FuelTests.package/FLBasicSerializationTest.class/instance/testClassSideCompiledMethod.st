tests-globals
testClassSideCompiledMethod
	"They should be considered as globals by default."
	
	self assertSerializationIdentityOf: FLGlobalSendMock class >> #newInstanceToSerialize
