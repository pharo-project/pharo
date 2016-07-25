testing
testClassDefinitionB
	writer visitClassDefinition: (self mockClassB asClassDefinition).
	self assertContentsOf: stream match: self expectedClassDefinitionB.
	