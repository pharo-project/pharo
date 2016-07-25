testing
testMczSerialization
	self assertVersionsMatchWith: MCMczWriter.
	self assertExtensionProvidedBy: MCMczWriter.
	self assertVersionInfosMatchWith: MCMczWriter.
	self assertDependenciesMatchWith: MCMczWriter.