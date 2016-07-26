tests-globals
testSystemDictionary
	"We want to treat the <Smalltalk globals> instance of SystemDictionary
	specially but want to serialize all others"

	self assertSerializationEqualityOf: SystemDictionary new