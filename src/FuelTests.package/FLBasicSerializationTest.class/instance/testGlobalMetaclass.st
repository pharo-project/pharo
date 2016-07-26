tests-globals
testGlobalMetaclass
	"A metaclass should be global by default."
	
	self assertSerializationIdentityOf: Integer class.
