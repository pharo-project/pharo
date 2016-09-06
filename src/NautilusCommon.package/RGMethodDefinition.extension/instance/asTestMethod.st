*NautilusCommon
asTestMethod
	"Returns the receiver if it's a TestCase test method, or its corresponding
	TestCase test method if it has one, or nil otherwise."

	^ (NautilusUtils asTestMethod: self) asRingDefinition