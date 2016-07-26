tests
testClassSideMethodPreservesIdentity
	"Tests that serialization of a method in the class-side of a class or trait preserves identity"
	
	| classOrTrait |
	classOrTrait := self newClassOrTrait.
	classOrTrait classSide duringTestCompileSilently: 'you'.
	self assertSerializationIdentityOf: classOrTrait classSide >> #you