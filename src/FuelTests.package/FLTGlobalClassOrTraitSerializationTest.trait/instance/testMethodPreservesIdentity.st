tests
testMethodPreservesIdentity
	"Tests that serialization of a method in a class or trait preserves identity"
	
	| classOrTrait |
	classOrTrait := self newClassOrTrait.
	classOrTrait duringTestCompileSilently: 'you'.
	self assertSerializationIdentityOf: classOrTrait >> #you