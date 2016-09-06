*NautilusCommon
correspondingTestMethod
	"Returns the corresponding TestCase test method that tests the receiver
	or nil if there isn't one."

	^ (NautilusUtils correspondingTestMethodOf: self) asRingDefinition