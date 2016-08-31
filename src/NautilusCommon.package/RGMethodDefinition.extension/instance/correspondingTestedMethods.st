*NautilusCommon
correspondingTestedMethods
	"Returns the methods tested by the receiver if it is a TestCase test method."

	^ (NautilusUtils correspondingTestedMethodsOf: self)
		collect: [ :each | each asRingDefinition ]