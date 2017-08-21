testing
testMultipleClassCreation
	5 timesRepeat: [
		factory newClass].
	self assert: (self testedEnvironment allClasses includesAll: factory createdClasses).
	self assert: factory createdClassNames asSet size = 5.
	self assert: (self testedOrganization listAtCategoryNamed: factory defaultCategory) asSet = factory createdClassNames asSet.
	factory createdClasses do: [ :aClass |
		self assertEnvironmentOf: aClass ]