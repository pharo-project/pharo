tests
testInlineTemporaryBadInterval
	self shouldFail: (RBInlineTemporaryRefactoring 
			inline: (self 
					convertInterval: (29 to: 100)
					for: (RBRefactoryTestDataApp sourceCodeAt: #moveDefinition))
			from: #moveDefinition
			in: RBRefactoryTestDataApp)