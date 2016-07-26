tests
testSetUp
	"Note: In addition to verifying that the setUp worked the way it was expected to, testSetUp is used to illustrate the meaning of the simple access methods, methods that are not normally otherwise 'tested'"

	self assert: aBlockContext home = contextOfaBlockContext.
	self assert: aBlockContext receiver = self.
	self assert: (aBlockContext method isKindOf: CompiledMethod).