tests
testClassSidePreservesIdentity
	"Tests that serialization of the class side preserves identity"
	
	self assertSerializationIdentityOf: self newClassOrTrait classSide 