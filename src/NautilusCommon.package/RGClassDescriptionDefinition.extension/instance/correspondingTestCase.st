*NautilusCommon
correspondingTestCase
	"Return the TestCase that corresponds to me.
	If it does not exist, it returns nil."
	
	^ (NautilusUtils correspondingTestCaseOf: self) asRingDefinition